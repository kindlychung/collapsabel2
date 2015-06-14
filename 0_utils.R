
tabular <- function(df, ...) {
	stopifnot(is.data.frame(df))
	align <- function(x) if (is.numeric(x)) "r" else "l"
	col_align <- vapply(df, align, character(1))
	cols <- lapply(df, format, ...)
	contents <- do.call("paste",
			c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))
	paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
			contents, "\n}\n", sep = "")
}

#x1 = data.frame(
#		x0 = c(0, 0, 0, 0),
#		x1 = c(0, 1, 1, 1),
#		x2 = c(0, 1, 0, 3), 
#		x3 = c(0, 1, 3, 3))
#row.names(x1) = paste("x", 0:3, sep = "")
#print(x1)
#cat(tabular(x1))



