
setOldClass("ffdf")

#' S4 class for necessary info to read a bed file into R
#' 
#' @import rJava
#' @slot pl_info PlInfo object
#' @slot jbed jobjRef object, of Bed class in java
#' @slot nsnp numeric. Number of SNPs.
#' @slot nindiv numeric. Number of individuals.
#' @slot nindiv_appr numeric. Apparent number of individuals.
#' @slot bytes_snp numeric. Number of bytes used for each SNP.
#' @slot bim ffdf object. Data from .bim file.
#' @slot fam ffdf object. Data from .fam file.
#' @export 
.RbedInfo = setClass("RbedInfo", 
		representation(
				pl_info = "PlInfo", 
				jbed = "jobjRef", 
				nsnp = "numeric", 
				nindiv = "numeric", 
				nindiv_appr = "numeric", 
				bytes_snp = "numeric", 
#				snp_idx = "numeric",
				bim = "ffdf",
				fam = "ffdf"
		), 
		prototype(
				pl_info = .PlInfo(), 
				jbed = .jnew("java/lang/Integer", 0L), 
				nsnp = 0, 
				nindiv = 0, 
				nindiv_appr = 0, 
				bytes_snp = 0, 
				bim = df2ffdf(data.frame(x = NA)),
				fam = df2ffdf(data.frame(x = NA))
		),
		validity = function(object) {
			if(! .jinstanceof(object@jbed, "vu/co/kaiyin/Bed")) {
				return("jbed is not of java Bed class.")
			}
			if(! bedSizeCorrect(object)) {
				return("bed file size does not agree with that of bim and bed files")
			}
			
			TRUE
		})

#' Constructor of RbedInfo class
#' 
#' @param bedstem character. Path to bed file without extension.
#' @return An RbedInfo object.
#' @examples 
#' \donotrun{
#' debugonce(rbedInfo)
#' rbed_info = rbedInfo(bedstem = "/users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/test")
#' rbed_info@@nsnp
#' rbed_info@@nindiv
#' rbed_info@@bytes_snp
#' theoBedSize(rbed_info)
#' realBedSize(rbed_info)
#' bedSizeCorrect(rbed_info)
#' bed_full = readBed(rbed_info)
#' dim(bed_full)
#' class(bed_full)
#' print(bed_full)
#' bed1 = readBed(rbed_info, 1:4)
#' print(bed1)
#' bed1a = readBed(rbed_info, snp_names = paste("snp", 1:4, sep=""))
#' all(na.omit(bed1a == bed1))
#' bed2 = readBed(rbed_info, 3:6)
#' bed2a = readBed(rbed_info, snp_names = paste("snp", 3:6, sep = ""))
#' all(na.omit(bed2 == bed2a))
#' }
#' 
#' @author kaiyin
#' @export
rbedInfo = function(bedstem) {
	stopifnot(length(bedstem) == 1)
	stopifnot(is.character(bedstem))
	pl_info = plInfo(bedstem = bedstem)
	if(! isSetup(pl_info)) {
		setup(pl_info)
	}
	bed_path = pl_info@plink_trio["bed"]
    bytes_snp = bytesSnp(pl_info)
	nindiv = nIndivPl(pl_info)
	nindiv_appr = nIndivApprPl(pl_info)
	nsnp = nSnpPl(pl_info)
#	snp_idx = 1L:nsnp
	bim = loadBim(pl_info)
	fam = loadFam(pl_info)
	jbed = .jnew("vu/co/kaiyin/Bed", bed_path, as.integer(bytes_snp), as.integer(nindiv))
	.RbedInfo(pl_info = pl_info, 
			jbed = jbed, 
			nsnp = nsnp, 
			nindiv = nindiv, 
			nindiv_appr = nindiv_appr, 
#			snp_idx = snp_idx,
			bytes_snp = bytes_snp, 
			bim = bim,
			fam = fam
	)
}

setGeneric("bedSizeCorrect",
		function(rbed_info, ...) {
			standardGeneric("bedSizeCorrect")
		})

#' Check whether bed file is of correct size
#' 
#' It is correct if its real size is the equal to its theoretical size.
#' 
#' @name bedSizeCorrect
#' 
#' @param rbed_info RbedInfo object
#' @return logical.
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("bedSizeCorrect",
		signature(rbed_info = "RbedInfo"),
		function(rbed_info) {
			realBedSize(rbed_info) == theoBedSize(rbed_info)
		})

setGeneric("realBedSize",
		function(rbed_info, ...) {
			standardGeneric("realBedSize")
		})

#' File size of bed file 
#' 
#' @name realBedSize
#' 
#' @param rbed_info RbedInfo object
#' @return numeric. Size of bed file.
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("realBedSize",
		signature(rbed_info = "RbedInfo"),
		function(rbed_info) {
			as.numeric(file.info(rbed_info@pl_info@plink_trio["bed"])$size)
		})


setGeneric("theoBedSize",
		function(rbed_info, ...) {
			standardGeneric("theoBedSize")
		})

#' Theoretical size of bed file
#' 
#' Computed from dimensions of bim an fam files.
#' 
#' @name theoBedSize
#' 
#' @param rbed_info RbedInfo object
#' @return numeric. Theoretical size of bed file.
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("theoBedSize",
		signature(rbed_info = "RbedInfo"),
		function(rbed_info) {
			(as.numeric(rbed_info@nindiv_appr) * 
						as.numeric(rbed_info@nsnp) / 4) + 3
		})


setGeneric("readBed",
		function(rbed_info, snp_vec, snp_names, ...) {
			standardGeneric("readBed")
		})

#' Read the whole bed file into R
#' 
#' @name readBed
#' 
#' @param rbed_info RbedInfo object
#' @param snp_vec numeric. Vector of SNP index, from 1 to total number of SNPs.
#' @param snp_name character. Vector of SNP names.
#' @return data.frame Genotype data from bed file.
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "missing", snp_names = "missing"),
		function(rbed_info, snp_vec, snp_names) {
			mat_ref = rbed_info@jbed$readbed(
					.jarray(1L:(rbed_info@nsnp)))
			res = getJArray(mat_ref = mat_ref)
			res = setNames(res, rbed_info@bim[, "SNP"])
			res = cbind(fidIid(rbed_info), res)
			res$FID = as.character(res$FID)
			res$IID = as.character(res$IID)
			res
		})

#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "numeric", snp_names = "missing"),
		function(rbed_info, snp_vec, snp_names) {
			snp_vec = as.integer(unique(snp_vec))
			mat_ref = rbed_info@jbed$readbed(
					.jarray(snp_vec))
			res = getJArray(mat_ref = mat_ref)
			res = setNames(res, rbed_info@bim[snp_vec, "SNP"])
			res = cbind(fidIid(rbed_info), res)
			
			res$FID = as.character(res$FID)
			res$IID = as.character(res$IID)
			res
		})

#' FID and IID columns from fam file
#' 
#' @param rbed_info RbedInfo object
#' @return data.frame of two columns "FID" and "IID"
#' 
#' @author kaiyin
#' @export
fidIid = function(rbed_info) {
	rbed_info@fam[, c("FID", "IID")]
}

#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "missing", snp_names = "character"),
		function(rbed_info, snp_vec, snp_names) {
			snp_vec = which(
					rbed_info@bim[, "SNP"] %in% 
							snp_names)
			snp_vec = as.integer(unique(snp_vec))
			readBed(rbed_info, snp_vec)
		})


getJArray <- function(mat_ref) {
	res = .jevalArray(mat_ref, simplify = TRUE)
	res[res == -9] = NA
	res = as.data.frame(res)
	res
}





