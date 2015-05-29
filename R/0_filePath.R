#' An S4 class to represent a file path
#' 
#' This class comes with a validation function, 
#' making sure that the file exists.
#' 
#' @slot path character, file or dir path
#' 
#' @author kaiyin
#' @export
FilePath = setClass("FilePath", representation(path = "character"), 
		validity = function(object) {
			missing_files = nonExistentFiles(object@path)
			ifLen(missing_files, {
						paste("Missing file", missing_files)
					}, TRUE) 
		})

#' Constructor for FilePath class 
#' 
#' @param s character, path to file or dir
#' @return FilePath object
#' 
#' @author kaiyin
#' @export
filePath = function(s) {
	new("FilePath", path = s)
}

setGeneric("dirName",
		function(fp, ...) {
			standardGeneric("dirName")
		})

#' Directory name of a file path
#'  
#' @name dirName
#' 
#' @param fp FilePath object
#' @return character vector of directories 
#' @examples 
#' fp = filePath(R.home())
#' dirName(fp)
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("dirName",
		signature(fp = "FilePath"),
		function(fp) {
			dirname(fp@path)
		})

setGeneric("baseName",
		function(fp, ...) {
			standardGeneric("baseName")
		})


#' Basename of a FilePath object
#' 
#' @name baseName
#' 
#' @param fp 
#' @return character vector of basenames
#' @examples
#' fp = filePath(R.home())
#' baseName(fp)
#' 
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("baseName",
		signature(fp = "FilePath"),
		function(fp) {
			basename(fp@path)
		})



