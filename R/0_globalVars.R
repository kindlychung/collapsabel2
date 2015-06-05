#' Global list of plink jobs
#' 
#' @name plink_job_list
#' @export
.plink_job_list = list()

#' A variable to store status of a running plink job
#' @name plink_job_status
#' @export 
.plink_job_status = NULL

globalVariables(
		c(
				".assoc_header", 
				".plink_out_ext", 
				".qassoc_header",
				".collapsabel_dir", 
				".collapsabel_gwas", 
				".collapsabel_gcdh", 
				".plink_job_list", 
				".plink_job_status")
)