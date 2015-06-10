setGeneric("correctTypes",
		function(dat, col_names, types, ...) {
			standardGeneric("correctTypes")
		})

#' Convert columns of a data frame to certain types
#' 
#' @param dat data.frame The data frame whose types you want to change.
#' @param col_names character. Names of columns, the types of which you want to change.
#' @param types character. Names of new types. Should be the same length as \code{col_names}
#' @return data.frame. With specified classes.
#' @examples 
#' \donotrun{
#' dat = randNormDat(3, 3)
#' dat[, 2] = as.character(dat$V2)
#' dat1 = correctTypes(dat, types = rep("numeric", 3))
#' all(colClasses(dat1) == rep("numeric", 3))
#' dat2 = correctTypes(dat, 2, "numeric")
#' all(colClasses(dat2) == rep("numeric", 3))
#' } 
#' @author kaiyin
#' @name correctTypes_methods
#' @export
setMethod("correctTypes",
		signature(dat = "data.frame", col_names = "character", types = "character"),
		function(dat, col_names, types) {
			stopifnot(length(col_names) == length(types))
			dat_colns = colnames(dat)
			for(i in 1:length(col_names)) {
				if(col_names[i] %in% dat_colns) {
					dat[, col_names[i]] = suppressWarnings(
							as(dat[, col_names[i]], types[i]))
				}
			}
			dat
		})

#' @rdname correctTypes_methods
#' @export 
setMethod("correctTypes",
		signature(dat = "data.frame", col_names = "numeric", types = "character"),
		function(dat, col_names, types) {
			col_names = colnames(dat)[col_names]
			correctTypes(dat, col_names, types)
		})

#' @rdname correctTypes_methods
#' @export 
setMethod("correctTypes",
		signature(dat = "data.frame", col_names = "missing", types = "character"),
		function(dat, col_names, types) {
			col_names = colnames(dat)
			correctTypes(dat, col_names, types)
		})


#' Generate read_fun for ReadInfo class
#' 
#' @param header logical. Whether the input file has a header line.
#' @return function.
#' 
#' @author kaiyin
#' @export
readFunFactory = function(header) {
	function(object, cn_select = "..all") {
		filename = object@filename
		cnames = object@cnames
		header = object@header
		# allow selecting all columns
		if(length(cn_select) == 1 && cn_select == "..all") {
			cn_select = cnames
		}

		# allow globbing
		filename = Sys.glob(filename)[1]
		
		
		# get index of selected cols
#		stopifnot(all(cn_select %in% cnames)) 		
		col_sel_idx = which(cnames %in% cn_select)
		
		dat = readcols(filename, 
				col_sel_idx, 
				as.integer(header), 
				1)
		dat = as.data.frame(dat, stringsAsFactors=FALSE)

		# set colnames
		dat_colnames = cnames[col_sel_idx]
		dat = setNames(dat, dat_colnames)
		
		dat
	}
}



#' An S4 class to represent information about a whitespace-delimited text file to be read into R
#' 
#' @slot filename Path of the file
#' @slot cnames character vector of column names
#' @slot header logical. Whether the first line is header
#' @slot read_fun function. The function to be used when reading this file
.ReadInfo = setClass("ReadInfo", representation(filename = 'character', 
				cnames = "character", 
				header = "logical", 
				read_fun = "function"), 
		validity = function(object) {
			errors = character()
			if(length(object@header) != 1) {
				errors = c(errors, "header should be either TRUE or FALSE.")
			}
		})



setGeneric("readInfo",
		function(filename, cnames, ...) {
			standardGeneric("readInfo")
		})

#' ReadInfo constructor
#' 
#' This function takes a file path as parameter, assuming the file is whitspace delimited, 
#' not quoted, and has a header line. It returns a ReadInfo object. 
#' 
#' @name readInfo
#' 
#' @param filename Path of the file to read
#' @return ReadInfo object
#' @examples 
#' \donotrun{
#' ri = readInfo("/Users/kaiyin/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.frq")
#' getSlots("ReadInfo")
#' ri@@cnames
#' ri@@filename
#' ri@@header
#' frq = ri@@read_fun(ri, cn_select = "..all")
#' head(frq)
#' print(ri@@cnames)
#' frq1 = ri@@read_fun(ri, ri@@cnames[1:4])
#' head(frq1)
#' }
#' 
#' @author kaiyin
#' @export
setMethod("readInfo",
		signature(filename = "character", cnames = "missing"),
		function(filename) {
			filename = filePath(filename)@path
			# read the first line of file, which is the header
			first_line = readLiteral(filename, nrows = 1)
			cnames = as.character(first_line[1, ])
			read_fun = readFunFactory(TRUE)
			new("ReadInfo", 
					filename = filename, 
					cnames = cnames, 
					header = TRUE, 
					read_fun = read_fun)
		})

#' Read a file literally (all columns as character)
#' 
#' @param filename Path of file to be read
#' @param ... Passed to \code{read.table}
#' @return data.frame
#' @examples 
#' \donotrun{
#' df = data.frame(x = c("T", "%T", "10341"), 
#' 		y = c("F", "f%t", "431"), 
#' 		z = c("T", "TRUE", "FALSE"))
#' tmpf = tempfile()
#' write.table(df, file = tmpf, quote = FALSE, 
#' 		row.names = FALSE, col.names = FALSE)
#' system(sprintf("head %s", tmpf))
#' df1 = readLiteral(file = tmpf)
#' all(df1 == df)
#' }
#' 
#' @author kaiyin
#' @export
readLiteral = function(filename, ...) {
	filename = filePath(filename)@path
	first_line = read.table(filename, nrows = 1)
	read.table( 
			filename,
			stringsAsFactors = FALSE, 
			colClasses = rep("character", 
					length(first_line)), 
			comment.char = "", 
			header = FALSE,
			...)
}



#' @rdname readInfo
#' @export 
setMethod("readInfo",
		signature(filename = "character", cnames = "character"),
		function(filename, cnames) {
			filename = filePath(filename)@path
			# read the first line of file, this is also the first line
			# of the data, not header
			first_line = readLiteral(filename, nrows = 1)
			
			# make sure cnames match the file in width
			stopifnot(length(cnames) == ncol(first_line))
			# don't skip the first line
			read_fun = readFunFactory(FALSE)
			
			new("ReadInfo", 
					filename = filename, 
					cnames = cnames, 
					header = FALSE, 
					read_fun = read_fun)
		})


#' Correct types of bim data.frame
#' 
#' CHR, BP and GDIST columns should be integers. 
#' 
#' @param bim_dat data.frame read from a .bim file
#' @return data.frame
#' 
#' @author kaiyin
#' @export
bimCorrectTypes = function(bim_dat)  {
	correctTypes(bim_dat, c("CHR", "BP", "GDIST"), rep("integer", 3))
}





# generate xInfo functions, with colnames set. 
# this is only useful for files without a header
infoFactory = function(cnames) {
	function(filename) {
		readInfo(filename, cnames)
	}
}


#' .bim and .fam file information functions
#' 
#' These functions \code{bimInfo, famInfo, assocLinearInfo} are generated by a 
#' factory function. They are similar to readInfo, only with 
#' pre-determined colnames (since they are really known).
#'  
#' @name bim_fam_info
NULL

#' @rdname bim_fam_info
#' @export 
bimInfo = infoFactory(c("CHR", "SNP", "GDIST", "BP", "AL1", "AL2"))
#' @rdname bim_fam_info
#' @export 
famInfo = infoFactory(c("FID", "IID", "PID", "MID", "SEX", "PHE"))



readFactory = function(ext) {
	function(filename, cn_select = "..all") {
		stopifnot(tools::file_ext(filename) == ext)
		info = get(paste(ext, "Info", sep = ""))(filename)
		get(paste(ext, "CorrectTypes", sep = ""))(
				info@read_fun(info, cn_select))
	}
}



#' Read plink .bim files
#' 
#' @param filename .bim file path
#' @param cn_select a character vector for selected colnames
#' @return a data.frame
#' @examples 
#' \donotrun{
#' bim = readBim("/Users/kaiyin/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.bim", 
#' 		cn_select = "..all")
#' head(bim)
#' summary(bim)
#' bim_info = bimInfo("/Users/kaiyin/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.bim")
#' bim_info@@cnames
#' bim1 = bim_info@@read_fun(bim_info, c("CHR", "SNP", "BP"))
#' head(bim1)
#' }
#' 
#' @author kaiyin
#' @export
readBim = readFactory("bim")


#' Correct types of fam data.frame
#' 
#' SEX and PHE columns should be integers. 
#' 
#' @param fam_dat data.frame read from a .fam file
#' @return data.frame
#' 
#' @author kaiyin
#' @export
famCorrectTypes = function(fam_dat)  {
	fam_dat = correctTypes(fam_dat, c("SEX", "PHE"), c("integer", "numeric"))
	fam_dat[fam_dat == -9] = NA
	fam_dat
}


#' Read plink .fam files
#' 
#' @param filename .fam file path
#' @param cn_select a character vector for selected colnames
#' @return a data.frame
#' @examples 
#' \donotrun{
#' fam = readFam("/Users/kaiyin/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.fam", cn_select = "..all")
#' head(fam)
#' summary(fam)
#' fam_info = famInfo("/Users/kaiyin/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.fam")
#' fam_info@@cnames
#' fam1 = fam_info@@read_fun(fam_info, c("FID", "IID"))
#' head(fam1)
#' }
#' 
#' @author kaiyin
#' @export
readFam = readFactory("fam")

#' @rdname readAssoc
#' @export 
readLogistic = function(filename, cn_select = .linear_header)  {
	info = readInfo(filename)
	dat = info@read_fun(info, cn_select)
	correctTypes(dat, c("CHR", "BP", "NMISS", "BETA", "STAT", "P"), 
			c(rep("integer", 3), rep("numeric", 3)))
}

#' @rdname readAssoc
#' @export 
readLinear = readLogistic


#' Plink output file headers
#' 
#' @name plink_out_headers
#' @export 
.assoc_header = c("CHR", "SNP", "BP", "A1", 
				"F_A", "F_U", "A2", "CHISQ", "P", "OR")
#' @rdname plink_out_headers
#' @export 
.qassoc_header = c("CHR", "SNP", "BP", "NMISS", "BETA", "SE", "R2", "T", "P") 

#' @rdname plink_out_headers
#' @export 
.logistic_header = c("CHR", "SNP", "BP", "NMISS", "BETA", "SE", "R2", "T", "P")

#' @rdname plink_out_headers
#' @export 
.linear_header = .logistic_header

#' @examples 
#' \donotrun{
#' dat = readAssoc("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc")
#' head(dat)
#' all(c(dat[1, 1] == 11,
#' 				dat[2, 2] == "rs7127954",
#' 				dat[3, 3] == 101943700,
#' 				dat[4, 4] == "A",
#' 				dat[5, 5] == 0.4369))
#' all(colClasses(dat) == c("integer", "character", "integer", 
#' 		"character", "numeric", "numeric", 
#' 		"character", "numeric", "numeric", "numeric"))
#' dat = readQassoc("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc")
#' head(dat)
#' all(c(dat[1, 1] == 11,
#' 				dat[2, 2] == "rs7127954",
#' 				dat[3, 3] == 101943700,
#' 				dat[4, 4] == 831,
#' 				dat[5, 5] == 0.12400, 
#' 				dat[6, 6] == 0.4211))
#' all(
#' 		colClasses(qassoc) == c("integer", "character",  
#' 		"integer", "integer", 
#' 		"numeric", "numeric", 
#' 		"numeric", "numeric", "numeric"))
#' linear = readLinear("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc.linear")
#' head(linear)
#' }
#' @export 
readAssoc = function(filename, cn_select = .assoc_header) {
	info = readInfo(filename)
	dat = info@read_fun(info, cn_select)
	correctTypes(dat, c("CHR", "BP", "F_A", "F_U", "CHISQ", "P", "OR"),
			c(rep("integer", 2), rep("numeric", 5)))
}


#' Read .qassoc files
#' 
#' @rdname readAssoc
#' @export 
readQassoc = function(filename, cn_select = .qassoc_header) {
	info = readInfo(filename)
	dat = info@read_fun(info, cn_select)
	correctTypes(dat, 
			c("CHR",  "BP", "NMISS", "BETA", "SE", "R2", "T", "P"),
			c(rep("integer", 3), rep("numeric", 5))
			)
}

#' Plink output extensions
#' @name plink_out_ext
#' @export
.plink_out_ext = c("assoc", "qassoc", "linear", "logistic")

#' Read plink output files
#' 
#' @param filename Filenames of plink output files, see \code{.plink_out_ext}
#' @param ... Dispatched to one of \code{readAssoc, readQassoc, readLinear, readLogistic}
#' @return data.frame
#' @examples 
#' \donotrun{
#' dat1 = readPlinkOut("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc")
#' dat2 = readAssoc("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc")
#' all(na.omit(dat1 == dat2))
#' dat1 = readPlinkOut("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc", 
#' 		c("CHR", "SNP", "P", "OR"))
#' dat2 = readAssoc("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc", 
#' 		c("CHR", "SNP", "P", "OR"))
#' all(na.omit(dat1 == dat2))
#' dat1 = readPlinkOut("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc")
#' dat2 = readQassoc("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc")
#' all(na.omit(dat1 == dat2))
#' dat1 = readPlinkOut("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc", 
#' 		c("CHR", "SNP", "P", "R2"))
#' dat2 = readQassoc("/Users/kaiyin/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc", 
#' 		c("CHR", "SNP", "P", "R2"))
#' all(na.omit(dat1 == dat2))
#' }
#' 
#' @author kaiyin
#' @export
readPlinkOut = function(filename, ...) {
	ft = tools::file_ext(filename)
	stopifnot(ft %in% .plink_out_ext)
	func = paste("read", R.utils::capitalize(ft), sep = "")
	get(func)(filename, ...)
}




#' Get classes of columns of a data.frame
#' 
#' 
#' @param dat data.frame
#' @return character. Classes of \code{dat}
#' @examples 
#' dat = data.frame(x = 15L, y = 3.14, z = "abc", 
#'   u = TRUE, stringsAsFactors = FALSE)
#' all(colClasses(dat) == 
#' 				c("integer", "numeric", 
#' 						"character", "logical"))
#' 
#' @author kaiyin
#' @export
colClasses = function(dat) {
	sapply(1:ncol(dat), function(i) class(dat[, i]))
}

#' Represent classes of a data.frame in a character vector
#' 
#' @param dat data.frame 
#' @return character vector
#' @examples 
#' \donotrun{
#' dat = randNormDat(4, 2)
#' x = capture.output(reprClasses(dat), file = NULL)
#' x = eval(parse(text = x))
#' all(x == colClasses(dat))
#' }
#' 
#' @author kaiyin
#' @export
reprClasses = function(dat) {
	res = strVectorRepr(colClasses(dat))
	cat(res)
}


# generate a m by n data.frame from normal distribution
# randNormDat(3, 4)
randNormDat = function(m, n) {
	as.data.frame(matrix(rnorm(m * n), m, n))
}


