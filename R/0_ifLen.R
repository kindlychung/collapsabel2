#' IfLen macro
#' 
#' Check whether a object has non-zero length, and 
#' eval expression accordingly.
#' 
#' @param df An object which can be passed to \code{length}
#' @param body1 If \code{length(df)} is not zero, then this clause is evaluated, otherwise, body2 is evaluated.
#' @param body2 See above.
#' @importFrom gtools defmacro
#' 
#' @examples 
#' ifLen(c(1, 2), { print('yes!') }, {print("no!")})
#' 
#' @author kaiyin
#' @export
ifLen = gtools::defmacro(df, body1, body2 = {}, expr = {
			if(length(df) != 0) {
				body1
			} else {
				body2
			}
		})

#' IfLet macro
#' 
#' Eval expression x, assign it to a variable, and if that is TRUE, continue
#' to eval expression1, otherwise eval expression2. Inspired by the clojure 
#' \code{if-let} macro.
#' 
#' @param sym_str a string that will be converted to a symbol to hold value of \code{x}
#' @param x the predicate to be evalueated, and to be assigned to a temporary variable as described in \code{sym_str}
#' @param body1 expression to be evaluated when the temporary variable is TRUE.
#' @param body2 expression to be evaluated when the temporary variable is FALSE.
#' @importFrom gtools defmacro
#' 
#' @examples 
#' ifLet("..temp..", TRUE, {print(paste("true.", as.character(..temp..)))}, 
#' 		{print(paste("false.", as.character(..temp..)))})
#' 
#' @author kaiyin
#' @export
ifLet = gtools::defmacro(sym_str, x, body1, body2={}, expr = {
			stopifnot(is.character(sym_str))
			stopifnot(length(sym_str) == 1)
			assign(sym_str, x)
			if(eval(as.symbol(sym_str))) {
				body1
			} else {
				body2
			}
		})

#' IfLetLen macro
#' 
#' Similar to ifLet, but conditioned on whether the length of 
#' the result of \code{eval(x)} is 0.
#' 
#' 
#' @param x the predicate to be evalueated, and to be assigned to a temporary var called \code{..temp..}
#' @param body1 expression to be evaluated when \code{..temp..} is TRUE.
#' @param body2 expression to be evaluated when \code{..temp..} is FALSE.
#' @importFrom gtools defmacro
#' 
#' @examples 
#' ifLetLen("..temp..", 1:3, {print(paste("true.", as.character(..temp..)))}, 
#' 		{print(paste("false.", as.character(..temp..)))})
#' 
#' @author kaiyin
#' @export
ifLetLen = gtools::defmacro(sym_str, x, body1, body2={}, expr = {
			stopifnot(is.character(sym_str))
			stopifnot(length(sym_str) == 1)
			assign(sym_str, x)
			ifLen(eval(as.symbol(sym_str)), {
				body1
			}, {
				body2
			})
		})
