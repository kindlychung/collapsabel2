haplo.g = function(g) {
	a1 = (g > 1) + 1
	a2 = (g > 0) + 1
	data.frame(a1, a2)
}



running.bool = function(true_id, len) {
	stopifnot(all(true_id >= 1 & true_id <= len))
	res = rep(FALSE, len)
	res[true_id] = TRUE 
	res
}

# given a vector of maxid and a vector of lengths,
# return a bool vector
# for example:
# maxid = c(1 2 1 3)
# l     = c(2 3 4 5), then
# ret = c(T, F;          # 2 bools, 1st chosen
#         F, T, F;       # 3 bools, 2nd chosen
#         T, F, F, F;    # 4 bools, 1st chosen
#         F, F, T, F, F) # 5 bools, 3rd chosen
running.bools = function(maxid, len) {
	stopifnot(length(maxid) == length(len))
	res = lapply(1:length(len), function(i) running.bool(maxid[i], len[i]))
	do.call(c, res)
}

haploFormat = function(geno) {
	haplo = data.frame(matrix(NA, nrow(geno), 0))
	geno_ncol = ncol(geno)
	for(i in 1:geno_ncol) {
		# each SNP gets two haplotypes
		haplo = cbind(haplo, haplo.g(geno[, i]))
	}
	old_cnames = rep(colnames(geno), each = 2)
	cnames = paste0(old_cnames, ".a", rep(1:2, geno_ncol))
	colnames(haplo) = cnames
	haplo
}


getHaplo = function(geno) {
	stopifnot(ncol(geno) == 4 && all(c("FID", "IID") %in% colnames(geno)))
	geno = geno[complete.cases(geno), ]
	labels = colnames(geno)[3:4]
	geno$id = 1:nrow(geno)
	save.em = haplo.stats::haplo.em(haploFormat(geno[, 3:4]), labels)
	hapdat = data.frame(id = save.em$subj.id, h1 = save.em$hap1code, h2 = save.em$hap2code, p = save.em$post)
	id_groups = group_by(hapdat, id)
	idx =  summarise(id_groups, maxid = which.max(p))
	len   =  summarise(id_groups, len = length(p))
	select = running.bools(idx$maxid, len$len)
	hapdat = hapdat[select, ]
	haptab = save.em$haplotype
	haptab$h = 1:nrow(haptab)
	hapdat = dplyr::left_join(hapdat, haptab, by = c("h1" = "h"))
#	geno.hapdat = renameLoci(geno.hapdat, 1)
	hapdat = dplyr::left_join(hapdat, haptab, by = c("h2" = "h"))
	hapdat$diplo = apply(hapdat[, 5:8], 1, function(x) paste0("-", paste(x, collapse = "-"), "-"))
	hapdat = dplyr::left_join(geno, hapdat, by = "id")
	hapdat$id = NULL
	hapdat
}
