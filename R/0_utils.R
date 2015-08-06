
#' Create directory if it does not already exist
#' 
#' @param dir character. Path of directory to be created.
#' 
#' @author Kaiyin Zhong, Fan Liu
#' @export
dir.create2 = function(dir) {
	if(!file.exists(dir)) {
		dir.create(dir, recursive = TRUE)
	} else {
		TRUE
	}
}

#' Create file if it does not already exist
#' @param filename character. Path of file to be created.
#' @author Kaiyin Zhong, Fan Liu
#' @export
file.create2 = function(filename) {
	dir.create2(dirname(filename))
	if(!file.exists(filename)) {
		file.create(filename)
	} else {
		TRUE
	}
}

#' Check whether a file is a SQLite3 database.
#' 
#' @param filename character. Path to file to be checked.
#' @author Kaiyin Zhong, Fan Liu
#' @export
isSQLite3 = function(filename) {
	if(!file.exists(filename)) {
		return(FALSE)
	} 
	if(file.info(filename)$size < 100){
		return(FALSE)
	}
	con = file(filename, "rb")
	tryCatch({
				header = rawToChar(readBin(con, "raw", 15))
				return(header == "SQLite format 3")
			}, finally = {
				close(con)
			})
}


#' Distance with lag
#' 
#' Calculate the distance between each element in a numeric vector and 
#' the element that is \code{lag} positions after it. For the last 
#' \code{lag} elemnts, this distance does not exist, so NA is used as a placeholder.
#' The returned vector is of the same length as the input vector.
#' 
#' @param vec numeric.
#' @param lag integer.
#' @param reverse logical. Default to FALSE, i.e. calculate \code{vec[i+lag] - vec[i]}. When set to TRUE, calculate \code{vec[i] - vec[i+lag]}
#' @return numeric.
#' 
#' @author Kaiyin Zhong, Fan Liu
#' @export
lagDistance = function(vec, lag = 1, reverse = FALSE) {
	new_vec = c(vec[(1+lag):length(vec)], rep(NA, lag))
	stopifnot(length(new_vec) == length(vec))
	res = new_vec - vec
	if(reverse) 
		(-1 * res)
	else
		res
}




sqliteFileGcdh = function(tag, dbname) {
	fp = file.path(tag2Dir(tag, "gcdh"), 
			sprintf("%s.sqlite", dbname))
	file.create2(fp)
	fp
}

#' Call system command with format string
#' 
#' @param ... passed to \code{sprintf}
#' 
#' @author Kaiyin Zhong, Fan Liu
#' @export
systemFormat = function(...) {
	system(sprintf(...))
}

#' Stop with format string
#' 
#' @param ... passed to \code{sprintf}
#' 
#' @author Kaiyin Zhong, Fan Liu
#' @export
stopFormat = function(...) {
	stop(sprintf(...))
}

# like expand.grid, but reverse the columns order
expand.grid.rev = function(...) {
	ret = expand.grid(...)
	ret[, rev(colnames(ret))]
}

#' Correlation coefficient of column-pairs of two data frames
#' 
#' @param dat1 first data.frame
#' @param dat2 second data.frame
#' @return A vector of correlation coefficients.
#' 
#' @author Kaiyin Zhong
#' @export
colCors = function(dat1, dat2) {
	stopifnot(all(dim(dat1) == dim(dat2)))
	sapply(1:ncol(dat1), function(i) {
				cor(dat1[, i], dat2[, i])
			})
}

#' Default value for expression. 
#' 
#' When an expression evals to NULL, take the default value instead. Copied from dplyr source.
#' @usage x \%||\% y
#' @param x expression to be evaled. 
#' @param y default value.
#' @name getOrElse-operator
#' @author Hadley Wickham
#' @export
"%||%" <- function(x, y) if(is.null(x)) y else x


#' Change extension names
#' 
#' @param filename character. File path 
#' @param ext_name character. New extension name
#' @importFrom tools file_path_sans_ext
#' 
#' @author Kaiyin Zhong
#' @export
chExt = function(filename, ext_name) {
	base_name = tools::file_path_sans_ext(filename)
	paste0(base_name, ".", ext_name)
}

# ctime of db must be later than mtime of plink files
dbUpToDate = function(dbname) {
	fam_file = chExt(dbname, "fam")
	bim_file = chExt(dbname, "bim")
	bed_file = chExt(dbname, "bed")
	frq_file = chExt(dbname, "frq")
	db_ctime = as.integer(file.info(dbname)$ctime)
	fam_mtime = as.integer(file.info(fam_file)$mtime)
	bim_mtime = as.integer(file.info(bim_file)$mtime)
	bed_mtime = as.integer(file.info(bed_file)$mtime)
	frq_mtime = as.integer(file.info(frq_file)$mtime)
	frqUpToDate(frq_file) && (db_ctime > fam_mtime) && (db_ctime > bim_mtime) && (db_ctime > bed_mtime) && (db_ctime > frq_mtime)
}

frqUpToDate = function(frq_file) {
	bed_file = chExt(frq_file, "bed")
	bed_ctime = as.integer(file.info(bed_file)$ctime)
	frq_ctime = as.integer(file.info(frq_file)$ctime)
	frq_ctime > bed_ctime
}


rmPlinkFiles = function(plink_stem) {
	unlink(
			paste0(plink_stem, 
					c(".bed", 
							".bim", 
							".fam", 
							".frq", 
							".nosex", 
							".log", 
							".assoc",
							".qassoc",
							".assoc.linear", 
							".assoc.logistic", 
							".sqlite" 
					))
	)
}

#' Clear up CollapsABEL workspace
#' 
#' The workspace folder is defined in \code{collenv$.collapsabel_dir}.
#' 
#' @author Kaiyin Zhong, Fan Liu
#' @export
collClear = function() {
	unlink(Sys.glob(file.path(collenv$.collapsabel_dir, "*", "*")), recursive = TRUE)
}


#' Print quoted expression then its value
#' 
#' @param expr expression to be evaluated.
#' @export 
eprint = function(expr) {
	message(substitute(expr))
	print(expr)
}

#' Generate a m by n data.frame from normal distribution
#' 
#' @param m integer. Number of rows.
#' @param n integer. Number of columns.
#' 
#' @author Kaiyin Zhong
#' @export
randNormDat = function(m, n) {
	as.data.frame(matrix(rnorm(m * n), m, n))
}


#' Read phenotype file
#' @param file character, path to phenotype file.
#' @return data.frame
#' 
#' @author kaiyin
#' @export
read.phe.table = function(file) {
	phe = read.table(file, header = TRUE)
	stopifnot(all(c("FID", "IID") %in% colnames(phe)))
	phe$FID = as.character(phe$FID)
	phe$IID = as.character(phe$IID)
	phe
}

#' Write a phenotype data.frame to file
#' 
#' @param phe data.frame
#' @param file character, path to phenotype file.
#' 
#' @author Kaiyin Zhong
#' @export
write.phe.table = function(phe, file) {
	stopifnot(all(c("FID", "IID") %in% colnames(phe)))
	write.table(phe, file = file, quote = FALSE, row.names = FALSE)
}

#' Collpase genotypes
#' 
#' @param g1 numeric, genotype vector 1.
#' @param g2 numeric, genotype vector 2.
#' @return numeric, collapsed genotype of g1 and g2.
#' 
#' @author Kaiyin Zhong
#' @export
collapse = function(g1, g2) {
	ifelse(g1 == 2 | g2 == 2, 2, 
			ifelse(g1 + g2 >= 2, 2, 0))
}