#' An S4 class representing info about plink files
#' 
#' Info about plink bed files, including the root directory, 
#' paths of plink .bed, .bim, .fam and .frq files, ff backing 
#' directories for .bim, .fam and .frq files, etc.
#' 
#' @slot main_dir Root directory where .bed, .bim and .fam files sit.
#' @slot plink_stem character. Path to the .bed file sans the extension name
#' @slot plink_trio character of length 3. Paths to .bed, .bim and .fam files (in that order).
#' @slot plink_trio_base character. Basenames of \code{plink_trio}.
#' @slot plink_frq character. Path to .frq file.
#' @slot ff_dir character. Directory for storing ff backing files.
#' @examples 
#' \donotrun{
#' x = .PlInfo()
#' x@plink_trio = c("/tmp/a.pdf")
#' validObject(x) # error
#' }
#' 
#' @name PlInfo
#' TODO: test
.PlInfo = setClass("PlInfo", representation(main_dir = "character", 
				plink_stem = "character",
				plink_trio = "character", 
				plink_trio_base = "character", 
				plink_frq = "character",
				ff_dir = "character", 
				ff_dir_trio = "character"
		), 
		prototype(main_dir = "", 
				plink_stem = "",
				plink_trio = "", 
				plink_trio_base = "", 
				plink_frq = "",
				ff_dir = "", 
				ff_dir_trio = ""
		), validity = function(object) {
			msg = lenCheck(list(object@main_dir,
							object@plink_stem ,
							object@plink_trio ,
							object@plink_trio_base ,
							object@plink_frq ,
							object@ff_dir ,
							object@ff_dir_trio
					), c(1, 1, 3, 3, 1, 1, 3))			
			if(msg != TRUE) {
				return(msg)
			}
			ext_trio = c("bed", "bim", "fam")
			if(!all(tools::file_ext(object@plink_trio) 
							== ext_trio)) {
				return(paste("Extensions should be: ", strConcat(ext_trio)))
			}
			ifLetLen("miss_files", nonExistentFiles(object@plink_trio), {
						miss_files
					}, {
						TRUE
					})
		})


setGeneric("initialize",
		function(pl_info, bedstem, ...) {
			standardGeneric("initialize")
		})

#' Constructor for PlInfo class
#' 
#' Populates an PlInfo object from a given plink bed filename stem (i.e. exclude extension name)
#' 
#' @param pl_info a PlInfo object, possibly empty.
#' @param bedstem path of bed file excluding extension name
#' @return a PlInfo object
#' @examples 
#' \donotrun{
#' pl_info = initialize(.PlInfo(), "/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/mmp13")
#' isSetup(pl_info) # false
#' setup(pl_info)
#' isSetup(pl_info) # true
#' bim_ff = loadMyBim(pl_info)
#' head(bim_ff)
#' fam_ff = loadFam(pl_info)
#' head(fam_ff)
#' summary(fam_ff[, "IID"])
#' which(fam_ff[, "IID"] == "10425")
#' frq_ff = loadFrq(pl_info)
#' head(frq_ff)
#' }
#' 
#' @export 
#' @author kaiyin
setMethod("initialize",
		signature(pl_info = "PlInfo", bedstem = "character"),
		function(pl_info, bedstem) { 
			# plink trio
			ext_trio = c("bed", "bim", "fam")
			plink_stem = bedstem
			plink_trio = filePath(paste(bedstem, ext_trio, sep = "."))@path
			plink_trio_base = basename(plink_trio)
			names(plink_trio) = names(plink_trio_base) = ext_trio
			
			# main dir where plink files sit
			main_dir = dirname(plink_trio[1])
			
			# frq file
			plink_frq = paste(bedstem, ".frq", sep="")
			
			# ff dirs
			ff_dir = file.path(main_dir, paste(".ff", basename(bedstem), sep = "_"))
			ff_dir_trio = file.path(ff_dir, c(basename(plink_frq), plink_trio_base[c("bim", "fam")]))
			names(ff_dir_trio) = c("frq", "bim", "fam")
			
			# return a PlInfo obj
			pl_info@main_dir = main_dir
			pl_info@plink_stem = plink_stem
			pl_info@plink_trio = plink_trio
			pl_info@plink_trio_base = plink_trio_base
			pl_info@plink_frq = plink_frq
			pl_info@ff_dir = ff_dir
			pl_info@ff_dir_trio = ff_dir_trio
			validObject(pl_info)
			pl_info
		})

setGeneric("isSetup",
		function(pl_info, ...) {
			standardGeneric("isSetup")
		})

#' Check if a directory containing .bed .fam and .bim files is properly setup
#'  
#' @param pl_info PlInfo object
#' @return TRUE or FALSE
#' @examples 
#' # see examples in initialize
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("isSetup",
		signature(pl_info = "PlInfo"),
		function(pl_info) {
			all(file.exists(pl_info@ff_dir_trio))
		})

setGeneric("setup",
		function(pl_info, ...) {
			standardGeneric("setup")
		})

#' Setup up a directory containing plink files 
#' 
#' @param pl_info 
#' @examples 
#' # see examples in initialize
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("setup",
		signature(pl_info = "PlInfo"),
		function(pl_info) {
			dir.create(pl_info@ff_dir)
			if(isSetup(pl_info)) {
				TRUE
			} else {
				if(!file.exists(pl_info@plink_frq)) {
					plinkr(bfile = pl_info@plink_stem, 
							freq = "", 
							out = pl_info@plink_stem)
				}
				ff_frq = pl_info@ff_dir_trio["frq"]
				ff_bim = pl_info@ff_dir_trio["bim"]
				ff_fam = pl_info@ff_dir_trio["fam"]
				if(!file.exists(ff_frq)) {
					frq = read.table(pl_info@plink_frq, header = TRUE)
					saveFFDF(frq, dir = pl_info@ff_dir_trio["frq"], overwrite = TRUE)
				}
				if(!file.exists(ff_bim)) {
					bim = txtutils::readBim(pl_info@plink_trio["bim"], "..all")
					saveFFDF(bim, dir = pl_info@ff_dir_trio["bim"], overwrite = TRUE)
				}
				if(!file.exists(ff_fam)) {
					fam = txtutils::readFam(pl_info@plink_trio["fam"], "..all")
					saveFFDF(fam, dir = pl_info@ff_dir_trio["fam"], overwrite = TRUE)
				}
			}
		})





loadPlinkMeta = gtools::defmacro(ext, method_name, expr = {
			setGeneric(method_name, 
					function(pl_info, ...) {
						standardGeneric(method_name)
					})
			setMethod(method_name, 
					signature(pl_info = "PlInfo"), 
					function(pl_info) {
						loadFFDF(pl_info@ff_dir_trio[ext])
					})
		})

loadPlinkMeta("bim", "loadBim")
loadPlinkMeta("fam", "loadFam")
loadPlinkMeta("frq", "loadFrq")


#' Loading function for .bim .fam and .frq data
#' 
#' These three S4 methods \code{loadBim, loadFam, loadFrq} are
#' generated by a factory function. They all receive an PlInfo object as parameter.
#' In a properly setup plink directory, .bim .fam and .frq
#' files should have been converted to ff format. These three 
#' functions load the corresponding ff backing files on disk.
#' 
#' @name loading_functions
#' 
#' @author kaiyin
#' @docType methods
NULL

#' Load plink bim file (in ff format) into R
#' 
#' @name loadBim
#' 
#' @param pl_info PlInfo object
#' @return ffdf object
#' @examples 
#' # see examples in initialize
#' 
#' @author kaiyin
#' @docType methods
#' @export
loadBim

#' Load plink fam file (in ff format) into R
#' 
#' @name loadFam
#' 
#' @param pl_info PlInfo object
#' @return ffdf object
#' @examples 
#' # see examples in initialize
#' 
#' @author kaiyin
#' @docType methods
#' @export
loadFam

#' Load plink frq file (in ff format) into R
#' 
#' @name loadFrq
#' @examples 
#' # see examples in initialize
#' 
#' @param pl_info PlInfo object
#' @return ffdf object
#' 
#' @author kaiyin
#' @docType methods
#' @export
loadFrq

sourceAll = function() {
	r_files = file.path("R", list.files("R"))
	for(f in r_files) {
		print(f)
		source(f)
	}
}

