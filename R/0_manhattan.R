manhattanData = function(chr, bp, p, snp, color_vec = NULL, sort_chr_bp = TRUE) {
	# number of snps
	nsnps = length(chr)
	# vectors should have the same length
	stopifnot(
			length(bp) != nsnps || length(p) != nsnps || length(snp) != nsnps || length(color_vec) != nsnps
	)
	if(!is.numeric(chr)) {
		chr = as.integer(chr)
	}
	if(!is.numeric(bp)) {
		bp = as.integer(bp)
	}
	if(!is.numeric(p)) {
		p = as.integer(p)
	}
	if(!is.character(snp)) {
		snp = as.character(snp)
	}
	mlogp = mLogP(p)
	# add color vec if it's provided
	if(!is.null(color_vec)) {
		dat = data.frame(
				CHR = chr, 
				BP = bp, 
				P = p, 
				SNP = snp,
				MLOGP = mlogp,
				COLOR = color_vec
		)
	} else {
		dat = data.frame(
				CHR = chr, 
				BP = bp, 
				P = p, 
				SNP = snp,
				MLOGP = mlogp
		)
	}
	# order by chr and bp
	if(sort_chr_bp) dat = dat[order(dat$CHR, dat$BP), ]
	# update nsnps
	nsnps = nrow(dat)
	# unique chromosomes
	chr_unique = sort(unique(dat$CHR))
	# apparent chromosomes
	dat$ACHR = appChr(dat$CHR, nsnps, chr_unique)
	dat$XPOS = scaleByChr(dat$ACHR, dat$CHR, dat$BP, nsnps, chr_unique)
	list(
			data = dat,
			nsnps = nsnps,
			chr_unique = chr_unique
			)
}

mLogP = function(p) {
	# QC on p values. Should between 1e-300 and 1
	p[which(p < 1e-300)] = 1e-300
	p[which(p > 1)] = 1
	# -log of p
	-1 * log10(p)
}

# calculate apparent chr
appChr = function(chr, nsnps = NULL, chr_unique = NULL) {
	if(is.null(nsnps)) nsnps = length(chr)
	if(is.null(chr_unique)) chr_unique = sort(unique(chr))
	chr_diff = diff(chr)
	# if CHR is a sequence of natural numbers, then no change is needed
	if(all(chr_diff == 1)) {
		first_chr = chr_unique[1]
		if(first_chr == 1) return(chr)
		else {
			return(chr - first_chr + 1)
		}
	}
	achr = rep(0, nsnps)
	for(i in chr_unique) {
		achr = achr + (chr >= i)
	}
	achr
}

scaleByChr = function(achr, chr, bp, nsnps = NULL, chr_unique = NULL, zero_div_adjust = TRUE) {
	if(is.null(nsnps)) nsnps = length(chr)
	if(is.null(chr_unique)) chr_unique = sort(unique(chr))
	all_chr_scaled_pos = rep(NA, nsnps)
	for(chr_iter in chr_unique) {
		chr_check = chr_iter == chr
		chr_idx = which(chr_check)
		chr_nsnps = length(chr_idx)
		first_pos = bp[chr_idx[1]]
		pos_diff = bp[chr_idx] - first_pos
		scaled_pos = pos_diff / (pos_diff[chr_nsnps] + ifelse(zero_div_adjust, 0.05, 0))
		all_chr_scaled_pos[chr_idx] = scaled_pos
	}
	achr + all_chr_scaled_pos
}

manhattanPlot = function(mh_dat_res, hlines = NULL) {
	mh_data = mh_dat_res$data
	nsnps = mh_dat_res$nsnps
	chr_unique = mh_dat_res$chr_unique
	# limits of the y-axis
	mlogp_range = range(mh_data$MLOGP)
	minlogp = mlogp_range[1]
	maxlogp = ceiling(mlogp_range[2])
	# if nchr > 1, x axis labeled by scaled BP (sbp)
	# if nchr == 1, x axis labeled by BP
	if(length(chr_unique) > 1) {
		chr_color = TRUE
		myplot = ggplot(mh_data, aes(x=XPOS, y=MLOGP)) +
				xlab("Chromosomes") +
				scale_x_continuous(breaks=sort(unique(mh_data$ACHR)), minor_breaks=NULL, labels=chr_unique)
	} else {
		chr_color = FALSE
		myplot = ggplot(mh_data, aes(x=BP / 1e6, y=MLOGP)) +
				xlab(sprintf("Position on CHR %i (Mb)", mh_data$CHR[1]))
	}
	# do we need to use different colors for neighboring chromosomes? if there is only one chromosome, then no.
	if(chr_color) {
		if(!is.null(mh_data$COLOR)) {
			myplot = myplot + suppressWarnings(geom_point(aes(color = factor(paste(COLOR, ACHR %% 2))), alpha=.5)) +
					scale_color_discrete(name="")
		} else {
			myplot = myplot + suppressWarnings(geom_point(aes(color=factor(ACHR %% 2)), alpha=.5)) +
					scale_color_discrete(guide=FALSE)
		}
	} else {
		if(!is.null(mh_data$COLOR)) {
			myplot = myplot + suppressWarnings(geom_point(aes(color=factor(COLOR)), alpha=.5)) +
					scale_color_discrete(name = "")
		} else {
			myplot = myplot + suppressWarnings(geom_point(alpha=.5))
		}
	}
	# set y-axis limits
	myplot = myplot + scale_y_continuous(limits = c(minlogp, maxlogp), minor_breaks = NULL)
	# add horizontal lines if requested
	if(!is.null(hlines)) {
		hlines = -log10(hlines)
		for(hline in hlines) {
			myplot = myplot + geom_hline(yintercept = hline, alpha = .3, color = "blue")
		}
	}
	# set y-label
	myplot = myplot + ylab("-log P")
	myplot
}

contrastData = function(chr, bp, p, gcdh_p, snp) {
	len = length(chr)
	stopifnot(
			length(bp) == len && length(p) == len && length(gcdh_p) == len
			)
	chr = rep(chr, 2)
	bp = rep(bp, 2)
	p = c(p, gcdh_p)
	color_vec = rep(c("Single SNP", "gCDH"), each = len)
	manhattanData(chr, bp, p, snp, color_vec, TRUE)
}

contrastPlot = function(chr, bp, p, gcdh_p, ...) {
	manhattanPlot(contrastData(chr, bp, p, gcdh_p), ...)
}

annoContrastRegion = function(ggp, snp1, snp2) {
	anno_dat = p$data[p$data$SNP == snp1, ]
	anno_dat = anno_dat[anno_dat$colorvec == "gCDH", ]
	sbp2 = bp2 * anno_dat$sbp / anno_dat$BP
	anno_dat2 = within(anno_dat, {BP = bp2; SNP = snp2; sbp=sbp2})
	p1 = p + geom_point(data = anno_dat2, aes(BP/1e6, mlogp), shape=3) +
			geom_segment(aes(x = anno_dat$BP / 1e6,
							y = anno_dat$mlogp,
							xend = anno_dat2$BP / 1e6,
							yend = anno_dat2$mlogp),
					linetype=2)
	p1
}





