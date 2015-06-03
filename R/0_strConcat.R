
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
			stopifnot(length(sep) == 1)
			paste(ss, collapse = sep)
		})

#' @rdname strConcat
#' @export 
setMethod("strConcat",
		signature(ss = "character", sep = "missing"),
		function(ss, sep) {
			paste(ss, collapse = "")
		})
setGeneric("strVectorRepr",
		function(ss, print_out, ...) {
			standardGeneric("strVectorRepr")
		})


#' String Representation of a character vector
#' 
#' 
#' @param ss character. 
#' @param print_out logical. Whether to print out the string representation.
#' @return character.
#' @examples 
#' strVectorRepr(letters[1:3]) == 'c("a", "b", "c")'
#' strVectorRepr(
#'   as.character(1:3)) == 'c("1", "2", "3")'
#' all(eval(parse(text = strVectorRepr(as.character(1:3)))) == 
#'   c("1", "2", "3"))
#' 
#' @author kaiyin
#' @name strVectorRepr_methods
#' @export
setMethod("strVectorRepr",
		signature(ss = "character", print_out = "logical"),
		function(ss, print_out) {
			ss = strConcat(
					c(
							"c(", 
							strConcat(
									paste('"', ss, '"', sep = ""), 
									", "),
							")"
					)
			)
			if(print_out) {
				message(ss)
			}
			ss
		})


#' @rdname strVectorRepr_methods
#' @export 
setMethod("strVectorRepr",
		signature(ss = "character", print_out = "missing"), 
		function(ss, print_out) {
			strVectorRepr(ss, FALSE)
		})