#' CollapsABEL home directory
#' @name collapsabel_dir
#' @export 
.collapsabel_dir = file.path(Sys.getenv("HOME"), ".collapsabel")

#' CollapsABEL gwas directory
#' @name collapsabel_dir
#' @export 
.collapsabel_gwas = file.path(.collapsabel_dir, "gwas")

#' CollapsABEL gCDH analysis directory
#' @name collapsabel_dir
#' @export 
.collapsabel_gcdh = file.path(.collapsabel_dir, "gcdh")



#' An S4 class representing info about GWAS on plink files
#' 
#' @slot plink_phe character. Phenotype file. First line is header, first 2 columns are FID and IID.
#' @slot phe_name character. Colname of phenotype.
#' @slot covar_name character. A string of covariate names, separated by comma.
#' @slot gwas_tag character. Tag for this GWAS.
#' @slot gwas_notes character. Notes for this GWAS.
#' @examples 
#' \donotrun{
#' }
#' 
#' @name PlGwas
#' @export 
.PlGwas = setClass("PlGwas", 
		representation(
				plink_phe = "character", 
				phe_name = "character",
				covar_name = "character",
				gwas_tag = "character", 
				gwas_notes = "character"
		), 
		prototype(
				plink_phe = "", 
				phe_name = "",
				covar_name = "",
				gwas_tag = "", 
				gwas_notes = ""
		), 
		contains = "PlInfo", 
		validity = function(object) {
			obj_slots = list(
					object@plink_phe,
					object@phe_name, 
					object@covar_name, 
					object@gwas_tag
			)
			names(obj_slots) = c(
					"plink_phe", 
					"phe_name",
					"covar_name",
					"gwas_tag"
			)
			msg = lenCheck(obj_slots, rep(1, 4))
			if(msg != TRUE) {
				return(msg)
			}
			
			if(!file.exists(object@plink_phe)) {
				return("plink phenotype file does not exist.")
			}
			
			first_line = readLiteral(object@plink_phe, nrows = 1)[1, ]
			if(!all(object@phe_name %in% first_line)) {
				return(
						sprintf("phenotype name (%s) not in phenotype file.", 
								object@phe_name))
			}
			
			ifLetLen(".m.covars", covarNames(object), {
						ifLetLen(".m.idx", 
								which(! .m.covars %in% first_line), 
								{
									return(strConcat(c(
															"Covars not found: ", 
															strConcat(.m.covars[.m.idx], " "))))
								})
					})
			

			covars = covarNames(object)
			if(length(covars) > 0) {
				covar_not_there = which(! covars %in% first_line)
				if(length(covar_not_there) > 0) {
					return(
							strConcat(
									c(
											"Covars not found: ",
											strConcat(covars[covar_not_there], " ")
									)))
				}
			}
			
			TRUE
		})


setGeneric("covarNames",
		function(pl_gwas, ...) {
			standardGeneric("covarNames")
		})

#' Get covariate names of a GWAS
#' 
#' @name covarNames
#' 
#' @param pl_gwas PlGwas object.
#' @return character. Vector of covariate names.
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("covarNames",
		signature(pl_gwas = "PlGwas"),
		function(pl_gwas) {
			strsplit(pl_gwas@covar_name, ",")[[1]]
		})

setGeneric("plGwas",
		function(pl_gwas, phe, phe_name, covar_name, gwas_tag, gwas_notes, ...) {
			standardGeneric("plGwas")
		})

#' Constructor for PlGwas class
#' 
#' @name plGwas_methods
#' 
#' @param pl_gwas PlGwas or PlInfo object
#' @param phe character. Phenotype file
#' @param phe_name character. Phenotype names.
#' @param covar_name character. Covariate names.
#' @param gwas_tag character. Tag for this GWAS.
#' @param gwas_notes character. Notes for this GWAS.
#' @return PlGwas object
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("plGwas",
		signature(pl_gwas = "PlGwas", phe = "character", 
				phe_name = "character", covar_name = "character", 
				gwas_tag = "character", gwas_notes = "character"),
		function(pl_gwas, 
				phe, phe_name, covar_name, 
				gwas_tag, gwas_notes) {
			pl_gwas@plink_phe = phe
			pl_gwas@phe_name = phe_name
			pl_gwas@covar_name = covar_name
			pl_gwas@gwas_tag = gwas_tag
			pl_gwas@gwas_notes = gwas_notes
			validObject(pl_gwas)
			pl_gwas
		})


#' @rdname plGwas_methods
#' @export 
setMethod("plGwas",
		signature(pl_gwas = "PlInfo", phe = "character", 
				phe_name = "character", covar_name = "character", 
				gwas_tag = "character", gwas_notes = "character"),
		function(pl_gwas, 
				phe, phe_name, covar_name, 
				gwas_tag, gwas_notes) {
			pl_gwas = as(pl_gwas, "PlGwas")
			plGwas(pl_gwas,  
					phe, phe_name, covar_name, 
					gwas_tag, gwas_notes)
		}
)

#' @rdname plGwas_methods
#' @export 
setMethod("plGwas",
		signature(pl_gwas = "PlInfo", phe = "character", 
				phe_name = "character", covar_name = "character", 
				gwas_tag = "character", gwas_notes = "missing"),
		function(pl_gwas, 
				phe, phe_name, covar_name, 
				gwas_tag, gwas_notes) {
			plGwas(pl_gwas,  
					phe, phe_name, covar_name, 
					gwas_tag, "")
		}
)


pl_info = plInfo(bedstem = "/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/mmp13")
# Error in validObject
pl_gwas = plGwas(pl_info, 
		phe = "/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/mmp13.phe",
		phe_name = "nothing", 
		covar_name = "Sex,Cage", 
		gwas_tag = "mmp13_page_sex_age")
# Error in validObject
pl_gwas = plGwas(pl_info, 
		phe = "/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/mmp13.phe",
		phe_name = "Page", 
		covar_name = "nothing,Cage", 
		gwas_tag = "mmp13_page_sex_age")
pl_gwas = plGwas(pl_info, 
		phe = "/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/mmp13.phe",
		phe_name = "Page", 
		covar_name = "Sex,Cage", 
		gwas_tag = "mmp13_page_sex_age")
all(covarNames(pl_gwas) == c("Sex", "Age"))


#' @rdname plGwas_methods
#' @export 
setMethod("plGwas",
		signature(pl_gwas = "missing", phe = "character", 
				phe_name = "character", covar_name = "character", 
				gwas_tag = "character", gwas_notes = "character"),
		function(pl_gwas, 
				phe, phe_name, covar_name, 
				gwas_tag, gwas_notes) {
			pl_gwas = .PlGwas()
			plGwas(pl_gwas,  
					phe, phe_name, covar_name, 
					gwas_tag, gwas_notes)
		}
)






setGeneric("gwasDir",
		function(pl_gwas, ...) {
			standardGeneric("gwasDir")
		})

#' GWAS results directory of a certain GWAS scan
#'  
#' @name gwasDir_methods
#' @alias gwasDir,PlGwas-method
#' 
#' @param pl_gwas PlGwas object
#' @return character. 
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("gwasDir",
		signature(pl_gwas = "PlGwas"),
		function(pl_gwas) {
			gwas_dir = file.path(.collapsabel_gwas, pl_gwas@gwas_tag)
			if(!file.exists(gwas_dir)) {
				dir.create(gwas_dir)
			}
			gwas_dir
		})
