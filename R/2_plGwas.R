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
#' @slot gwas_tag character. Tag for this GWAS.
#' @slot opts list. Plink options.
#' 
#' @name PlGwas
#' @export 
.PlGwas = setClass("PlGwas", 
		representation(
				gwas_tag = "character",
				opts = "list"
		), 
		prototype(
				gwas_tag = "",
				opts = list()
		), 
		contains = "RbedInfo", 
		validity = function(object) {
			obj_slots = list( object@gwas_tag)
			names(obj_slots) = c( "gwas_tag")
			msg = lenCheck(obj_slots, 1)
			if(msg != TRUE) {
				return(msg)
			}
			
#			if(!file.exists(object@opts$pheno)) {
#				return("plink phenotype file does not exist.")
#			}
			
			first_line = readLiteral(object@opts$pheno, nrows = 1)[1, ]
			
			if(length(object@opts$pheno_name) != 1) 
				return("one phenotype at a time.")

			if(!(object@opts$pheno_name %in% first_line)) {
				return(
						sprintf("phenotype name (%s) not in phenotype file.", 
								object@opts$phe_name))
			}
			
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
			strsplit(pl_gwas@opts$covar_name, ",")[[1]]
		})

setGeneric("plGwas",
		function(pl_gwas, pheno, pheno_name, covar_name, gwas_tag, 
				assoc, opts, ...) {
			standardGeneric("plGwas")
		})

#' Constructor for PlGwas class
#' 
#' @name plGwas_methods
#' 
#' @param pl_gwas PlGwas or PlInfo object
#' @param pheno character. Phenotype file
#' @param pheno_name character. Phenotype names.
#' @param covar_name character. Covariate names.
#' @param gwas_tag character. Tag for this GWAS.
#' @return PlGwas object
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("plGwas",
		signature(pl_gwas = "PlGwas", pheno = "character", 
				pheno_name = "character", covar_name = "character", 
				gwas_tag = "character",  
				assoc = "logical", opts = "list"
		),
		function(pl_gwas, pheno, 
				pheno_name, covar_name, 
				gwas_tag, 
				assoc, opts
		) {
			browser()
			pl_gwas@gwas_tag = gwas_tag
			pl_gwas@opts$bfile = pl_gwas@pl_info@plink_stem
			pl_gwas@opts$pheno = pheno
			pl_gwas@opts$pheno_name = pheno_name
			pl_gwas@opts$covar = pheno
			pl_gwas@opts$covar_name = covar_name
			pl_gwas@opts$allow_no_sex = ""
#			pl_gwas@opts$stdout = opts$stderr = ""
			pl_gwas@opts$stdout = opts$stderr = gwasLog(pl_gwas)
			pl_gwas@opts$out = gwasOutStem(pl_gwas)
			pl_gwas@opts$wait = FALSE
			if(assoc) {
				pl_gwas@opts$assoc = ""
			} else if(binPhe(pl_gwas)) {
				pl_gwas@opts$logistic = "hide-covar beta"
			} else {
				pl_gwas@opts$linear = "hide-covar"
			}
			validObject(pl_gwas)
			pl_gwas
		})

#' @rdname plGwas_methods
#' @export 
setMethod("plGwas",
		signature(pl_gwas = "RbedInfo", pheno = "character", 
				pheno_name = "character", covar_name = "character", 
				gwas_tag = "character", 
				assoc = "logical", opts = "list"),
		function(pl_gwas, 
				pheno, pheno_name, covar_name, 
				gwas_tag, 
				assoc, opts) {
			pl_gwas = as(pl_gwas, "PlGwas")
			plGwas(pl_gwas,  
					pheno, pheno_name, covar_name, 
					gwas_tag ,
					assoc, opts
			)
		}
)


#' @rdname plGwas_methods
#' @export 
setMethod("plGwas",
		signature(pl_gwas = "RbedInfo", pheno = "character", 
				pheno_name = "character", covar_name = "character", 
				gwas_tag = "character",
				assoc = "missing", opts = "missing"),
		function(pl_gwas, 
				pheno, pheno_name, covar_name, 
				gwas_tag, 
				assoc, opts) {
			plGwas(pl_gwas,  
					pheno, pheno_name, covar_name, 
					gwas_tag, 
					FALSE, list())
		}
)

#' @rdname plGwas_methods
#' @export 
setMethod("plGwas",
		signature(pl_gwas = "RbedInfo", pheno = "character", 
				pheno_name = "character", covar_name = "character", 
				gwas_tag = "character", 
				assoc = "missing", opts = "list"
		),
		function(pl_gwas, 
				pheno, pheno_name, covar_name, 
				gwas_tag, 
				assoc, opts) {
			plGwas(pl_gwas,  
					pheno, pheno_name, covar_name, 
					gwas_tag, 
					FALSE, opts)
		}
)

#' @rdname plGwas_methods
#' @export 
setMethod("plGwas",
		signature(pl_gwas = "RbedInfo", pheno = "character", 
				pheno_name = "character", covar_name = "character", 
				gwas_tag = "character", 
				assoc = "logical", opts = "missing"
		),
		function(pl_gwas, 
				pheno, pheno_name, covar_name, 
				gwas_tag, 
				assoc, opts) {
			plGwas(pl_gwas,  
					pheno, pheno_name, covar_name, 
					gwas_tag, 
					assoc, list())
		}
)



setGeneric("gwasDir",
		function(pl_gwas, ...) {
			standardGeneric("gwasDir")
		})

#' GWAS results directory of a certain GWAS scan
#'  
#' @name gwasDir_methods
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
			file.path(gwasDir(pl_gwas), basename(pl_gwas@pl_info@plink_stem))
		})

#' Check whether an S4 object is of a certain class
#' 
#' @param obj S4 object
#' @param c Class name
#' @return  logical
#' 
#' @author kaiyin
#' @export
isS4Class = function(obj, c) {
	isS4(obj) && is(obj, c)
}

#' GWAS output file name
#' 
#' @param pl_gwas PlGwas object.
#' @return character
#' 
#' @author kaiyin
#' @export
gwasOut = function(pl_gwas) {
	stem = gwasOutStem(pl_gwas)
	if("assoc" %in% names(pl_gwas@opts)) {
		if(binPhe(pl_gwas)) {
			paste(stem, ".assoc", sep = '')
		} else {
			paste(stem, ".qassoc", sep = '')
		}
	} else if("linear" %in% names(pl_gwas@opts)) {
		paste(stem, ".assoc.linear", sep = "")
	} else if("logistic" %in% names(pl_gwas@opts)) {
		paste(stem, ".assoc.logistic", sep = "")
	} else {
		stop("no modeling option specified?")
	}
}


#' Run a GWAS
#' 
#' @param pl_gwas PlGwas object
#' @param opts GWAS options. Default to an empty list.
#' @param assoc logical. If set to TRUE, the \code{--assoc} option will be passed to plink.
#' 
#' @author kaiyin
#' @export
runGwas = function(pl_gwas) {
	# TODO: save.RDS the plgwas obj
	# TODO: list and fetch plgwas obj
	# TODO: add assoc option
	# TODO: try detect binary trait. --linear or --logistic
	stopifnot(isS4Class(pl_gwas, "PlGwas"))
	do.call(plinkr, pl_gwas@opts)
}


#' Check progress of a plink job
#' 
#' @param pl_gwas PlGwas object.
#' @return numeric. Percentage of progress.
#' 
#' @author kaiyin
#' @export
gwasPercentDone = function(pl_gwas) {
	out_file = gwasOut(pl_gwas)
	if(file.exists(out_file)) {
		fileSize(out_file) / theoPlinkOutSize(pl_gwas)
	} else {
		0L
	}
}

#' Approximate output file size of a plink job.
#' 
#' @param pl_gwas PlGwas object.
#' @return integer.
#' 
#' @author kaiyin
#' @export
theoPlinkOutSize = function(pl_gwas) {
	100 * pl_gwas@nsnp
}

#' Get file size
#' 
#' @param filename character. Path to file.
#' @return integer. Size of file.
#' 
#' @author kaiyin
#' @export
fileSize = function(filename) {
	stopifnot(is.character(filename))
	stopifnot(length(filename) == 1)
	filename = filePath(filename)@path
	file.info(filename)$size
}
	
#' Call system command with format string
#' 
#' @param ... passed to \code{sprintf}
#' @examples 
#' \donotrun{
#' systemFormat("ls %s", R.home())
#' }
#' 
#' @author kaiyin
#' @export
systemFormat = function(...) {
	system(sprintf(...))
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
	info = readInfo(pl_gwas@opts$pheno)
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
	stopifnot(is.numeric(nrows) && length(nrows) == 1)
	read.table(pl_gwas@opts$pheno, header = TRUE, nrows = nrows, 
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
	phe = readPhe(pl_gwas, pl_gwas@opts$pheno_name)
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








