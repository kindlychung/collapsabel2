#' CollapsABEL home directory
#' @name collapsabel_dir
#' @export 
.collapsabel_dir = file.path(Sys.getenv("HOME"), ".collapsabel")

#' CollapsABEL gwas directory
#' @name collapsabel_dir
#' @export 
.collapsabel_gwas = file.path(.collapsabel_dir, "gwas")

#' CollapsABEL gCDH analysis directory
#' @name collapsabel_dir
#' @export 
.collapsabel_gcdh = file.path(.collapsabel_dir, "gcdh")


#' Plink output file headers
#' 
#' @name plink_out_headers
#' @export 
.assoc_header = c("CHR", "SNP", "BP", "A1", 
				"F_A", "F_U", "A2", "CHISQ", "P", "OR")
#' @rdname plink_out_headers
#' @export 
.qassoc_header = c("CHR", "SNP", "BP", "NMISS", "BETA", "SE", "R2", "T", "P") 

#' @rdname plink_out_headers
#' @export 
.logistic_header = c("CHR", "SNP", "BP", "A1", "TEST", "NMISS", "BETA", "STAT", "P")

#' @rdname plink_out_headers
#' @export 
.logistic_header_default = c("NMISS", "BETA", "STAT", "P")

#' @rdname plink_out_headers
#' @export 
.linear_header = .logistic_header

#' @rdname plink_out_headers
#' @export 
.linear_header_default = .logistic_header_default

#' Plink output extensions
#' @name plink_out_ext
#' @export
.plink_out_ext = c("assoc", "qassoc", "linear", "logistic")


globalVariables(
		c(
				".assoc_header", 
				".qassoc_header",
				".logistic_header",
				".linear_header",
				".plink_out_ext", 
				".collapsabel_dir", 
				".collapsabel_gwas", 
				".collapsabel_gcdh"
						)
)