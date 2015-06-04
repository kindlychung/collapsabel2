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
				dir.create(gwas_dir, recursive = TRUE)
			}
			gwas_dir
		})


setGeneric("gwasOutStem",
		function(pl_gwas, ...) {
			standardGeneric("gwasOutStem")
		})

#' Plink output filename
#' 
#' To be passed as the \code{--out} option to plink.
#' 
#' @name gwasOutStem
#' 
#' @param pl_gwas PlGwas object.
#' @return character. Plink output filename, without extension
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("gwasOutStem",
		signature(pl_gwas = "PlGwas"),
		function(pl_gwas) {
			file.path(gwasDir(pl_gwas), basename(pl_gwas@plink_stem))
		})

isS4Class = function(obj, c) {
	isS4(obj) && is(obj, c)
}

#' Run a GWAS
#' 
#' @param pl_gwas PlGwas object
#' @param opts GWAS options. Default to an empty list.
#' @param assoc logical. If set to TRUE, the \code{--assoc} option will be passed to plink.
#' 
#' @author kaiyin
#' @export
runGwas = function(pl_gwas, assoc = FALSE, opts=list()) {
	# TODO: save.RDS the plgwas obj
	# TODO: list and fetch plgwas obj
	# TODO: add assoc option
	# TODO: try detect binary trait. --linear or --logistic
	stopifnot(isS4Class(pl_gwas, "PlGwas"))
	stopifnot(is.logical(assoc))
	stopifnot(is.list(opts))
	opts$bfile = pl_gwas@plink_stem
	opts$pheno = pl_gwas@plink_phe
	opts$pheno_name = pl_gwas@phe_name
	opts$covar = pl_gwas@plink_phe
	opts$covar_name = pl_gwas@covar_name
	opts$allow_no_sex = ""
	opts$stdout = gwasLog(pl_gwas)
	opts$out = gwasOutStem(pl_gwas)
	if(assoc) {
		opts$assoc = ""
	} else if(binPhe(pl_gwas)) {
		opts$logistic = "hide-covar beta"
	} else {
		opts$linear = "hide-covar"
	}
	do.call(plinkr, opts)
}

#' Read phenotype file
#' 
#' @param pl_gwas PlGwas object
#' @param cn_select Colnames to select. Default to "..all", which means all columns are read in.
#' @return data.frame
#' 
#' @author kaiyin
#' @export
readPhe = function(pl_gwas, cn_select = "..all") {
	stopifnot(isS4Class(pl_gwas, "PlGwas"))
	info = readInfo(pl_gwas@plink_phe)
	phe = info@read_fun(info, cn_select)
	if(length(cn_select) == 1 && cn_select == "..all") {
		classes = colClasses(headPhe(pl_gwas, 1))
	} else {
		classes = colClasses(headPhe(pl_gwas, 1)[, cn_select, drop = FALSE])
	}
	correctTypes(phe, types = classes)
}

#' Read first n lines of a phenotype file
#' 
#' @param pl_gwas PlGwas object
#' @param nrows number of lines to read
#' @return data.frame
#' 
#' @author kaiyin
#' @export
headPhe = function(pl_gwas, nrows = 5L) {
	stopifnot(isS4Class(pl_gwas, "PlGwas"))
	stopifnot(is.numeric(nrows))
	read.table(pl_gwas@plink_phe, header = TRUE, nrows = nrows, 
			stringsAsFactors = FALSE)
}

#' Check whether a trait is binary
#' 
#' @param v numeric vector.
#' @param na_value a vector of numeric values which should be seen as NA.
#' @return logical
#' @examples
#' \donotrun{
#' !isBinary(c(1, 1.1, 1, 1.1, NA))
#' isBinary(c(1, 2, 1, 2, NA))
#' !isBinary(c(-9, 2.3, 4.1, -9, -9), -9)
#' isBinary(c(-9, 2, 4, -9, -9), -9)
#' isBinary(c(1, 2, 2, 1, -9, -9.9), c(-9, -9.9))
#' }
#' 
#' @author kaiyin
#' @export
isBinary = function(v, na_value = NULL) {
	stopifnot(is.numeric(v))
	v = unique(na.omit(v))
	if(! is.null(na_value)) {
		v = v[! v %in% na_value]
	}
	if(length(v) > 2L || any(as.integer(v) != v)) {
		FALSE
	} else {
		TRUE
	}
}

#' Check whether phenotype of a GWAS is binary
#' 
#' @param pl_gwas PlGwas object.
#' @return logical
#' 
#' @author kaiyin
#' @export
binPhe = function(pl_gwas) {
	stopifnot(isS4Class(pl_gwas, "PlGwas"))
	phe = readPhe(pl_gwas, pl_gwas@phe_name)
	isBinary(phe[, 1])
}

#' Plink log fie
#' 
#' Redirect stdout to this file when plink is running.
#' 
#' @param pl_gwas PlGwas object.
#' @return character. Path to log file.
#' 
#' @author kaiyin
#' @export
gwasLog = function(pl_gwas) {
	stopifnot(isS4Class(pl_gwas, "PlGwas"))
	sprintf("%s.stdout_log", gwasOutStem(pl_gwas))
}




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
pl_gwas@phe_name == "Page"
pl_gwas@covar_name == "Sex, Cage"
all(covarNames(pl_gwas) == c("Sex", "Age"))
g_dir = gwasDir(pl_gwas)
print(g_dir)
file.exists(gwasDir(pl_gwas))
file.info(gwasDir(pl_gwas))$isdir
g_out = gwasOutStem(pl_gwas)
print(g_out)
debugonce(plinkr)
runGwas(pl_gwas)
classes = colClasses(headPhe(pl_gwas, 1))
phe = readPhe(pl_gwas)
all(classes == colClasses(phe))
phe = readPhe(pl_gwas, c("FID", "Cage", "Sex"))
all(colClasses(phe) == c("character", "numeric", "integer"))
binPhe(pl_gwas)
gwasLog(pl_gwas)




