#' Validate a phenotype file
#' 
#' @param phe_file character. Phenotype file.
#' @return FALSE when the file is invalid, or a data.frame when it is. 
#' 
#' @author kaiyin
#' @export
validPhe = function(phe_file) {
	phe = read.table(phe_file, stringsAsFactor = FALSE, header = TRUE)
	if(!all(colnames(phe)[1:2] == c("FID", "IID"))) {
		return(FALSE)
	}
	for(i in 3:ncol(phe)) {
		if(!is.numeric(phe[, i])) {
			return(FALSE)
		}
	}
	phe
}

#' Permute a phenotype file
#' 
#' All columns except FID and IID are permuted.
#' 
#' @param phe_file character. Phenotype file.
#' @param out_file character. Path to permuted phenotype file.
#' @param force logical. When set to TRUE, existing file is overwritten.
#' @param valid logical. Whether to validate the phenotype file first. 
#' 
#' @author kaiyin
#' @export
permutePhe = function(phe_file, out_file, force = FALSE, valid = TRUE) {
	if(file.exists(out_file) && !force) {
		stopFormat("File %s already exists!", out_file)
	}
	if(valid) phe = validPhe(phe_file)
	else phe = read.table(phe_file, stringsAsFactor = FALSE, header = TRUE)
	fidiid = phe[, c("FID", "IID")]
	others = phe[, !(colnames(phe) %in% c("FID", "IID"))]
	others = others[sample(nrow(others)), ]
	phe = cbind(fidiid, others)
	write.table(phe, file = out_file, quote = FALSE, row.names = FALSE)
	invisible(phe)
}

#' Run simulations to control type-I error
#' 
#' @param pl_gwas PlGwas object
#' @param type_one_tag character. Tag for this simulation.
#' @param n_shift integer. \code{n_shift} for each gCDH run.
#' @param n_simu integer. Number of simulations to run.
#' @param dist_threshold See runGcdh.
#' @param collapse_matrix See runGcdh.
#' @param rm_shifted_files  See runGcdh.
#' @return list 
#' 
#' @author kaiyin
#' @export
runTypeI = function(
		pl_gwas,
		n_shift, 
		n_simu,
		dist_threshold = 500e3,
		collapse_matrix = NULL,
		rm_shifted_files = TRUE
) {
	stopifnot(is.data.frame(validPhe(pl_gwas@opts$pheno)))
	# to avoid collision between multiple R sessions, add a suffix of random chars
	type_one_tag = sprintf("%s_%s", pl_gwas@gwas_tag, randomString(6))
	pl_gwas = chGwasTag(pl_gwas, type_one_tag)
	if(!isSetupRbed(pl_gwas)) {
		setupRbed(pl_gwas)
	}
	nsnps = nSnpPl(pl_gwas@pl_info)
	# careful, don't run multiple R sessions on the same type_one_tag!
	bmCreate(tag = type_one_tag, type = "typeI", bm_name = "pmins", nrow = nsnps)
	pmins_bin_file = file.path(collenv$.collapsabel_dir, "typeI", type_one_tag, bmBinFilename("pmins"))
	new_pheno = file.path(tag2Dir(type_one_tag, "typeI"), "permuted.phe")
	for(simu_i in 1:n_simu) {
		permutePhe(pl_gwas@opts$pheno, new_pheno, TRUE, FALSE)
		new_pl_gwas = pl_gwas
		new_pl_gwas@opts$pheno = new_pheno
		new_pl_gwas = chGwasTag(new_pl_gwas, paste0(new_pl_gwas@gwas_tag, "_simulation_", simu_i))
		gcdh_res = runGcdh(
				pl_gwas = new_pl_gwas, 
				gwas_col_select = "P", 
				n_shift = n_shift, 
				collapse_matrix = collapse_matrix, 
				rm_shifted_files = rm_shifted_files, 
				dist_threshold = dist_threshold
		)
		pmin = biganalytics::apply(gcdh_res$P, 1, function(i) biganalytics::min(i, na.rm = TRUE))
		pmins = bigmemory::attach.big.matrix(bin2DescFilename(pmins_bin_file))
		if(simu_i == 1) {
			do.call("[<-", list(x = pmins, j = 1, value = pmin))
		} else {
			bmAddCol(pmins_bin_file, pmin)
		}
		removeTag(gcdh_res$tag, "gcdh")
	}
	pmins = bigmemory::attach.big.matrix(bin2DescFilename(pmins_bin_file))
	pminmins = colmin(pmins, na.rm = TRUE)
	list(
			tag = type_one_tag,
			pmins = pmins,
			pminmins = pminmins
			)
}

