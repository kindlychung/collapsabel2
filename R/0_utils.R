
#' Create directory if it does not already exist
#' 
#' @param dir character. Path of directory to be created.
#' 
#' @author kaiyin
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
#' @author kaiyin
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
#' @author kaiyin
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
#' @author kaiyin
#' @export
lagDistance = function(vec, lag = 1, reverse = FALSE) {
	new_vec = c(vec[(1+lag):length(vec)], rep(NA, lag))
	stopifnot(length(new_vec) == length(vec))
	if(reverse) vec - new_vec
	else        new_vec - vec
}




# TODO: doc test
sqliteFileGcdh = function(tag, dbname) {
	fp = file.path(tag2Dir(tag, "gcdh"), 
			sprintf("%s.sqlite", dbname))
	file.create2(fp)
	fp
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

#' Stop with format string
#' 
#' @param ... passed to \code{sprintf}
#' @examples 
#' \donotrun{
#' stopFormat("You should put file here: %s", R.home())
#' }
#' 
#' @author kaiyin
#' @export
stopFormat = function(...) {
	stop(sprintf(...))
}

# like expand.grid, but reverse the columns order
expand.grid.rev = function(...) {
	ret = expand.grid(...)
	ret[, rev(colnames(ret))]
}

colCors = function(dat1, dat2) {
	stopifnot(all(dim(dat1) == dim(dat2)))
	sapply(1:ncol(dat1), function(i) {
				cor(dat1[, i], dat2[, i])
			})
}
