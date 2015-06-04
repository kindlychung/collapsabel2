#require(ff)
#require(ffbase)

setGeneric("loadFFDF",
		function(dir, ...) {
			standardGeneric("loadFFDF")
		})

#' Load ffdf directory 
#' @name loadFFDF
#' @importFrom ffbase load.ffdf save.ffdf physical
#' 
#' @param dir the ffdf directory to be loaded
#' @return an ffdf object 
#' @examples
#' \donotrun{
#' iris_ff = df2ffdf(iris)
#' save.ffdf(iris_ff, dir = "iris_ff")
#' rm(iris_ff)
#' # error: object "iris_ff" not found
#' head(iris_ff) 
#' iris_ff = loadFFDF("iris_ff")
#' # print out head of iris data, in ffdf format
#' head(iris_ff) 
#' }
#' 
#' @author kaiyin
#' @docType methods
setMethod("loadFFDF",
		signature(dir = "FilePath"),
		function(dir) {
			dir = dir@path
			old_wd = getwd()
			paren_dir = dirname(dir)
			ffdf_dir = basename(dir)

			setwd(paren_dir)
			tryCatch({
						capture.output(ffbase::load.ffdf(ffdf_dir))
						file_list = list.files(ffdf_dir)
						ff_obj_name = stringr::str_match(string = file_list[1], pattern = "^(.*)\\$")[2]
						eval(as.symbol(ff_obj_name))
					}, finally = {
						setwd(old_wd)
					})
		})

#' @rdname loadFFDF
#' @export 
setMethod("loadFFDF",
		signature(dir = "character"),
		function(dir) {
			dir = gsub("\\/$", "", dir)
			loadFFDF(filePath(dir))
		})



setGeneric("saveFFDF",
		function(df, dir, overwrite, ...) {
			standardGeneric("saveFFDF")
		})

#' Save an R data.frame to ff format
#'  
#' @name saveFFDF
#' 
#' @param df data.frame to be saved
#' @param dir ff directory
#' @param overwrite logical. Default to TRUE
#' 
#' @author kaiyin
#' @docType methods
#' @export
setMethod("saveFFDF",
		signature(df = "data.frame", dir = "character", overwrite = "logical"),
		function(df, dir, overwrite) {
			df.ff = df2ffdf(df)
			ffbase::save.ffdf(df.ff, dir = dir, overwrite = overwrite)
		})

#' @rdname saveFFDF
#' @examples 
#' \donotrun{
#' saveFFDF(iris, "/tmp/iris")
#' iris1 = loadFFDF("/tmp/iris")
#' iris1[1, 1] == 5.1
#' }
#' @export 
setMethod("saveFFDF", 
		signature(df = "data.frame", dir = "character", overwrite = "missing"), 
		function(df, dir, overwrite) {
			saveFFDF(df, dir, TRUE)
		})




