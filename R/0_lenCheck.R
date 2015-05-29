setGeneric("lenCheck",
		function(ilist, ilengths, ...) {
			standardGeneric("lenCheck")
		})

#' Check each element of a list has expected length
#'  
#' Give a \code{list(a, b, ...)} and \code{vector(l1, l2, ...)}, 
#' check that length of a is equal to l1, length of b is equal to l2, etc.
#' 
#' @name lenCheck
#' 
#' @param ilist list of items you want to check.
#' @param ilengths vector of lengths for these items.
#' @return TRUE or a string
#' @examples 
#' x = list(1, 2, 3)
#' str(x[c(1, 3)])
#' lenCheck(list(1, 2, 3), c(1, 1, 0))
#' 
#' @author kaiyin
#' @docType methods
#' @export
# TODO: test
setMethod("lenCheck",
		signature(ilist = "list", ilengths = "numeric"),
		function(ilist, ilengths) {
			stopifnot(length(ilist) == length(ilengths))
			
			list_form = deparse(substitute(ilist))
			lens = sapply(ilist, length)
			bad_idx = lens != ilengths
			
			if(all(bad_idx == FALSE)) {
				TRUE
			} else {
				strConcat(c("\nGiven: \n", 
								list_form, 
								"\nExpected lengths: \n", 
								as.character(ilengths), 
								"\nObserved length: \n", 
								as.character(lens), 
								"\n Differ at these indices: \n", 
								as.character(which(bad_idx)), 
								"\n"), " ")
			}
		})