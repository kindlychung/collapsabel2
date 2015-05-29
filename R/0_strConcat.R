
setGeneric("strConcat",
		function(ss, sep, ...) {
			standardGeneric("strConcat")
		})

#' Concatenate a vector of strings
#' @name strConcat
#' 
#' @param ss vector of strings
#' @param sep a length-1 string used as separator, default to ""
#' @return a string
#' @examples
#' strConcat(letters)
#' strConcat(letters, " ")
#' 
#' @author kaiyin
#' @docType methods
setMethod("strConcat",
		signature(ss = "character", sep = "character"),
		function(ss, sep) {
			stopifnot(length(sep) != 1)
			paste(ss, collapse = sep)
		})

#' @rdname strConcat
#' @export 
setMethod("strConcat",
		signature(ss = "character", sep = "missing"),
		function(ss, sep) {
			paste(ss, collapse = "")
		})
