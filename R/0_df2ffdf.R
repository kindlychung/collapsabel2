#' Converts a data frame to an ffdf
#' @importFrom ff ffdf
#' @importFrom ff as.ff
#' @importFrom ff as.ram 
#' @param df the data.frame you want to convert.
#' @return an ffdf object.
#' @examples
#' require(ff)
#' d = data.frame(x = rnorm(10), y = sapply(65:74, function(i) rawToChar(as.raw(i))))
#' d.ffd = df2ffdf(d)
#' class(d.ffd)
#' d1 = ff::as.ram(d.ffd)
#' all(d1 == d) # true
setGeneric("df2ffdf",
		valueClass = "ffdf",
		function(df, ...) {
			standardGeneric("df2ffdf")
		})

setMethod("df2ffdf",
		signature(df = "data.frame"),
		function(df) {
			do.call(ffdf, lapply(df, function(i) {
								if(is.character(i)) {
									i = as.factor(i)
								}
								as.ff(i)
							}))
		})

setMethod("df2ffdf", 
		signature(df = "matrix"), 
		function(df) {
			df2ffdf(as.data.frame(df))
		})

# tmpdir = tempdir()
# print(tmpdir)
# unlink(tmpdir)


