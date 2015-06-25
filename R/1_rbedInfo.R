

#' S4 class for necessary info to read a bed file into R
#' 
#' @import rJava
#' @slot pl_info PlInfo object
#' @slot jbed jobjRef object, of Bed class in java
#' @slot nsnp numeric. Number of SNPs.
#' @slot nindiv numeric. Number of individuals.
#' @slot nindiv_appr numeric. Apparent number of individuals.
#' @slot bytes_snp numeric. Number of bytes used for each SNP.
#' @export 
.RbedInfo = setClass("RbedInfo", 
		representation(
				pl_info = "PlInfo", 
				jbed = "jobjRef", 
				nsnp = "numeric", 
				nindiv = "numeric", 
				nindiv_appr = "numeric", 
				bytes_snp = "numeric"
		), 
		prototype(
				pl_info = .PlInfo(), 
				jbed = .jnew("java/lang/Integer", 0L), 
				nsnp = 0, 
				nindiv = 0, 
				nindiv_appr = 0, 
				bytes_snp = 0
		),
		validity = function(object) {
			if(! .jinstanceof(object@jbed, "vu/co/kaiyin/Bed")) {
				return("jbed is not of java Bed class.")
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
#' bed1a = readBed(rbed_info, paste("snp", 1:4, sep=""))
#' all(na.omit(bed1a == bed1))
#' bed2 = readBed(rbed_info, 3:6)
#' bed2a = readBed(rbed_info, paste("snp", 3:6, sep = ""))
#' all(na.omit(bed2 == bed2a))
#' }
#' 
#' @author kaiyin
#' @export
rbedInfo = function(bedstem, ff_setup = FALSE) {
	stopifnot(length(bedstem) == 1)
	stopifnot(is.character(bedstem))
	pl_info = plInfo(bedstem = bedstem, ff_setup = ff_setup)
	bed_path = pl_info@plink_trio["bed"]
	
	rbed_info = .RbedInfo()
	rbed_info@pl_info = pl_info
	rbed_info@jbed = collUtils::rBed(bed_path)
	if(ff_setup) {
		setup(pl_info)
		rbed_info@bytes_snp = bytesSnp(pl_info)
		rbed_info@nindiv = nIndivPl(pl_info)
		rbed_info@nindiv_appr = nIndivApprPl(pl_info)
		rbed_info@nsnp = nSnpPl(pl_info)
		validObject(rbed_info) 
		rbed_info
	} else {
		rbed_info
	}
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
		function(rbed_info, snp_vec, 
				fid_iid = TRUE, snp_names_as_colnames = TRUE, ...) {
			standardGeneric("readBed")
		})

#' Read the whole bed file into R
#' 
#' @name readBed
#' 
#' @param rbed_info RbedInfo object
#' @param snp_vec numeric. Vector of SNP index, from 1 to total number of SNPs.
#' @return data.frame Genotype data from bed file.
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "ANY",
				fid_iid = "logical", snp_names_as_colnames = "logical"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			if(is.numeric(snp_vec)) {
				snp_vec = sort(as.integer(snp_vec))
				snp_names = getQuery(
						sqliteFilePl(rbed_info@pl_info), 
						sprintf("select snp from bim where rowid in %s order by rowid", 
								numVectorSQLRepr(snp_vec)))[, 1]
			} else if(is.character(snp_vec)) {
				snp_names = snp_vec
				snp_vec = snpRowId(rbed_info@pl_info, snp_vec)
				snp_ord = order(snp_vec)
				snp_vec = snp_vec[snp_ord]
				snp_names = snp_names[snp_ord]
			} else {
				stop("snp_vec must be either numeric or character.")
			}

			mat_ref = rbed_info@jbed$readBed(
					.jarray(snp_vec))
			res = getJArray(mat_ref = mat_ref)
			if(snp_names_as_colnames) {
				res = setNames(res, snp_names)
			}
			if(fid_iid) {
				res = cbind(fidIid(rbed_info@pl_info), res)
				res$FID = as.character(res$FID)
				res$IID = as.character(res$IID)
			}
			res
		})

#' Get row number of SNPs from their names
#' 
#' @param pl_info PlInfo object.
#' @param snp_names character. Vector of SNP names.
#' @return integer. Vector of row numbers.
#' 
#' @author kaiyin
#' @export
snpRowId = function(pl_info, snp_names) {
	stopifnot(isSetup(pl_info))
	getQuery(sqliteFilePl(pl_info), 
			sprintf("select rowid from bim where snp in %s order by rowid", strVectorSQLRepr(snp_names)))[, 1]
}



#' Send query to SQLite database
#' 
#' @param db_name character. Path to database.
#' @param query_string character. Query string.
#' 
#' @author kaiyin
#' @export
sendQuery = function(db_name, query_string) {
	db = RSQLite::dbConnect(RSQLite::SQLite(), db_name, synchronous = "full")
	tryCatch({
				RSQLite::dbSendQuery(db, query_string)
			}, finally = {
				RSQLite::dbDisconnect(db)
			})
}

#' Get query results from a SQLite database
#' 
#' @param db_name character. Path to database.
#' @param query_string character. Query string.
#' 
#' @author kaiyin
#' @export
getQuery = function(db_name, query_string) {
	db = RSQLite::dbConnect(RSQLite::SQLite(), db_name)
	tryCatch({
				return(RSQLite::dbGetQuery(db, query_string))
			}, finally = {
				RSQLite::dbDisconnect(db)
			})
}



#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "missing",
				fid_iid = "missing", snp_names_as_colnames = "missing"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			nsnp = rbed_info@jbed$getnSNPs()
			readBed(rbed_info, 1:nsnp, TRUE, TRUE)
		})

#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "ANY",
				fid_iid = "missing", snp_names_as_colnames = "missing"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			readBed(rbed_info, snp_vec, TRUE, TRUE)
		})

#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "missing",
				fid_iid = "logical", snp_names_as_colnames = "missing"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			nsnp = rbed_info@jbed$getnSNPs()
			readBed(rbed_info,
					snp_vec = 1:nsnp,
					fid_iid = fid_iid, 
					snp_names_as_colnames = TRUE)
		})

#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "ANY",
				fid_iid = "logical", snp_names_as_colnames = "missing"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			readBed(rbed_info, snp_vec, fid_iid, TRUE)
		})

#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "missing",
				fid_iid = "missing", snp_names_as_colnames = "logical"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			nsnp = rbed_info@jbed$getnSNPs()
			readBed(rbed_info, 1:nsnp, TRUE, snp_names_as_colnames)
		})


#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "ANY",
				fid_iid = "missing", snp_names_as_colnames = "logical"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			readBed(rbed_info, snp_vec, TRUE, snp_names_as_colnames)
		})

#' @rdname readBed
#' @export 
setMethod("readBed",
		signature(rbed_info = "RbedInfo", snp_vec = "missing",
				fid_iid = "logical", snp_names_as_colnames = "logical"),
		function(rbed_info, snp_vec, 
				fid_iid, snp_names_as_colnames
		) {
			nsnp = rbed_info@jbed$getnSNPs()
			readBed(rbed_info, 1:nsnp, fid_iid, snp_names_as_colnames)
		})

getJArray <- function(mat_ref, na_vals = -9) {
	res = .jevalArray(mat_ref, simplify = TRUE)
	res[res %in% na_vals] = NA
	res = as.data.frame(res)
	res
}

#' Add a "shift" suffix to a stem
#' 
#' @param stem character.
#' @param n_shift numeric.
#' @return character. 
#' @examples 
#' # add suffix to stem
#' shiftedStem("a", 100) == "a_shift_0100"
#' shiftedStem("home/a", 100) == "home/a_shift_0100"
#' shiftedStem("/home/a", 100) == "/home/a_shift_0100"
#' shiftedStem(c("/home/a", "/home/b"), 100) == c("/home/a_shift_0100", 
#' 		"/home/b_shift_0100")
#' 
#' @author kaiyin
#' @export
shiftedStem = function(stem, n_shift) {
	sprintf("%s_shift_%04d", stem, n_shift)
}

#' Shift bed files
#' 
#' Generates collapsed genotypes by shifting the bed file
#' (i.e. SNP1 collapsed with SNP2, SNP2 collapsed with SNP3, etc, 
#' when \code{n_shift == 1}).
#' 
#' @param rbed_info RbedInfo object
#' @param n_shift integer. 
#' @param collapse_matrix matrix of integers. See details.
#' 
#' @details Collapsing matrix.
#' The collapse_matrix parameter allows collapsing of two genotypes in
#' a arbitrary way. Each genotype is represented by either 0, 1, 2, or 3:
#' \describe{
#' \item{0}{Homozygote of the minor allele.}
#' \item{1}{NA}
#' \item{2}{Heterozygote.}
#' \item{3}{Homozygote of the major allele.}
#' }
#' 
#' The collapsing function is implemented as a matrix lookup function, i.e.
#' \eqn{Collapse(S1, S2) = CollapseMatrix[S1][S2]}.
#' 
#' The default collapsing matrix is:
#' 
#' \tabular{rrrr}{
#'   0 \tab 0 \tab 0 \tab 0\cr
#'   0 \tab 1 \tab 1 \tab 1\cr
#'   0 \tab 1 \tab 0 \tab 3\cr
#'   0 \tab 1 \tab 3 \tab 3
#' }
#' 
#' @return RbedInfo object, with the shifted bed file path in it. 
#' 
#' @author kaiyin
#' @export
shiftBed = function(rbed_info, n_shift, ff_setup = FALSE, collapse_matrix = NULL) {
	stopifnot(isS4Class(rbed_info, "RbedInfo"))
	n_shift = as.integer(n_shift)
	if(!is.null(collapse_matrix)) {
		stopifnot(is.integer(collapse_matrix) && 
						all(dim(collapse_matrix) == c(4, 4)))
		rbed_info@jbed$shift(n_shift, collapse_matrix)
	} else {
		rbed_info@jbed$shift(n_shift)
	}
	invisible(
			rbedInfo(bedstem = shiftedStem(rbed_info@pl_info@plink_stem, n_shift), 
			ff_setup = ff_setup)
			) 
}

#' Remove files by matching the starting part
#' 
#' If \code{x} is a string, then this function matches \code{x*} by globbing.
#' If \code{x} is a "PlInfo" object, it matches \code{x@@plink_stem*}, 
#' If \code{x} is a "RbedInfo" object, it matches \code{x@@pl_info@@plink_stem*}.
#' Otherwise nothing is removed.
#' 
#' 
#' @param x character, PlInfo, or RbedInfo object.
#' 
#' @author kaiyin
#' @export
rmFilesByStem = function(x) {
	if(is.character(x)) {
		unlink(Sys.glob(paste(x, "*", sep = "")))
	} else if(isS4Class(x, "PlInfo")) {
		unlink(Sys.glob(paste(x@plink_stem, "*", sep = "")))
	} else if(isS4Class(x, "RbedInfo")) {
		unlink(Sys.glob(paste(x@pl_info@plink_stem, "*", sep = "")))
	} else {
		NULL
	}
}

#' Create gCDH task directories by tag
#' 
#' The task folder is a subfolder of the value of \code{.collapsabel_gcdh}. 
#' It will be created if it does not yet exist.
#' 
#' @param gcdh_tag character. Tag for gCDH task.
#' @return character. Directory of the task.
#' 
#' @author kaiyin
#' @export
gcdhDir = function(gcdh_tag) {
	stopifnot(is.character(gcdh_tag) && length(gcdh_tag) == 1)
	dir_names = c(gcdh_tag, "nmiss", "beta", "stat", "p")
	d = file.path(.collapsabel_gcdh, dir_names[1])
	dir.create2(d)
	ds = file.path(d, dir_names[2:5])
	res = c(d, ds)
	names(res) = c("root", dir_names[2:5])
	res
}





# TODO: test readBed with extremely large bed files (RS123 50G)