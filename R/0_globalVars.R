#' An environment for storing CollapsABEL package local variables
#' 
#' \describe {
#' \item{.collapsabel_dir}{CollapsABEL home directory}
#' \item{.collapsabel_gwas}{CollapsABEL gwas directory}
#' \item{.collapsabel_gcdh}{CollapsABEL gCDH analysis directory}
#' \item{.assoc_header}{Plink .assoc file headers}
#' \item{.qassoc_header}{Plink .qassoc file headers}
#' \item{.logistic_header}{Plink .assoc.logistic file headers}
#' \item{.logistic_header_default}{Columns from plink .assoc.logistic file headers that are used by default}
#' \item{.linear_header}{Plink .assoc.linear file headers}
#' \item{.linear_header_default}{Columns from plink .assoc.linear file headers that are used by default}
#' \item{.plink_out_ext}{Plink output extensions}
#' \item{.plink_stdout}{Plink stdout}
#' \item{.plink_stderr}{Plink stderr}
#' }
#' @name collenv
collenv <<- new.env()
collenv$.collapsabel_dir = file.path(Sys.getenv("HOME"), ".collapsabel")
collenv$.collapsabel_gwas = file.path(collenv$.collapsabel_dir, "gwas")
collenv$.collapsabel_gcdh = file.path(collenv$.collapsabel_dir, "gcdh")
collenv$.assoc_header = c("CHR", "SNP", "BP", "A1", "F_A", "F_U", "A2", "CHISQ", "P", "OR")
collenv$.qassoc_header = c("CHR", "SNP", "BP", "NMISS", "BETA", "SE", "R2", "T", "P")
collenv$.logistic_header = c("CHR", "SNP", "BP", "A1", "TEST", "NMISS", "BETA", "STAT", "P")
collenv$.logistic_header_default = c("NMISS", "BETA", "STAT", "P")
collenv$.linear_header = collenv$.logistic_header
collenv$.linear_header_default = collenv$.logistic_header_default
collenv$.plink_out_ext = c("assoc", "qassoc", "linear", "logistic")
collenv$.plink_stdout = FALSE
collenv$.plink_stderr = FALSE


#' Alpha-numeric characters
#' @export
alphaNumeric = c(letters, LETTERS, as.character(0:9))

#globalVariables(
#		c(
#				"collenv$.assoc_header",
#				"collenv$.qassoc_header",
#				"collenv$.logistic_header",
#				"collenv$.linear_header",
#				"collenv$.plink_out_ext",
#				"collenv$.collapsabel_dir",
#				"collenv$.collapsabel_gwas",
#				"collenv$.collapsabel_gcdh",
#				"alphaNumeric",
#				"collenv$.plink_stderr",
#				"collenv$.plink_stdout"
#						)
#)
