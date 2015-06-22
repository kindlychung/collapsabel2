slurp = function(filename) {
	f = file(filename, "r")
	tryCatch({
				res =  paste(readLines(f), collapse = "\n")
			}, finally = {
				close(f)
			})
	res
}

spit = function(s, filename){
	f = file(filename, "w") 
	tryCatch({
				writeLines(con = f, text = s)
			}, finally = {
				close(f)
			})
}

evalFile = function(filename) {
	eval(parse(text = slurp(filename)))
}

readDesc = function(desc_filename, type = "binary") {
	if(type == "binary") {
		readRDS(desc_filename)
	} else {
		evalFile(desc_filename)
	}
}

saveDesc = function(desc_obj, desc_filename, type = "binary") {
	if(type == "binary") {
		saveRDS(desc_obj, file = desc_filename)
	} else {
		dput(desc_obj, desc_filename)
	}
}

listEqual = function(list1, list2) {
	list1 = unlist(list1, recursive = TRUE)
	list2 = unlist(list2, recursive = TRUE)
	all(na.omit(list1 == list2))
}

addColBigMatrix = function(bm, dat) {
	
}

type2Bytes = function(type = "double") {
	if (type == "integer")
		4
	else if (type == "double")
		8
	else if (type == "short")
		2
	else if (type == "char")
		1
	else
		stop("Unknown type!")
}


setGeneric('asBigMatrix', 
		function(x, type=NULL, separated=FALSE,
				backingfile=NULL, backingpath=NULL,
				descriptorfile=NULL, binarydescriptor=FALSE, shared=TRUE, 
				dimnames = FALSE) standardGeneric('asBigMatrix'))

setMethod('asBigMatrix', signature(x='matrix', dimnames = "logical"),
		function(x, type, separated, backingfile, backingpath, descriptorfile,
				binarydescriptor, shared, dimnames)
		{
			if (!is.numeric(x)) {
				warning("Casting to numeric type")
				x <- matrix(as.numeric(x), nrow=nrow(x), dimnames=dimnames(x))
			}
			if(dimnames) {
				dim_names = dimnames(x)
			} else {
				dim_names = NULL
			}
			if (is.null(type)) type <- typeof(x)
			
			if (type=="integer" | type=="double" | type=="short" | type=="char") 
			{
				y <- bigmemory::big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, 
						init=NULL, dimnames=dim_names, separated=separated,
						backingfile=backingfile, backingpath=backingpath,
						descriptorfile=descriptorfile, binarydescriptor=binarydescriptor,
						shared=shared)
				y[1:nrow(x),1:ncol(x)] <- x
				junk <- gc() 
			} else stop('bigmemory: that type is not implemented.')
			return(y)
		})

setMethod('asBigMatrix', signature(x='data.frame', dimnames = "logical"),
		function(x, type, separated, backingfile, backingpath, descriptorfile,
				binarydescriptor, shared, dimnames)
		{
			warning("Coercing data.frame to matrix via factor level numberings.")
			if (is.null(type)) type <- options()$bigmemory.default.type
			if (type=="integer" | type=="double" | type=="short" | type=="char") 
			{
				if(dimnames) {
					dim_names = dimnames(x)
				} else {
					dim_names = NULL
				}
				y <- bigmemory::big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, 
						init=NULL, dimnames=dim_names, separated=separated,
						backingfile=backingfile, backingpath=backingpath,
						descriptorfile=descriptorfile, binarydescriptor=binarydescriptor,
						shared=shared)
				oldbtw <- options()$bigmemory.typecast.warning
				options(bigmemory.typecast.warning=FALSE)
				for (i in 1:ncol(x)) {
					if (is.character(x[,i])) x[,i] <- factor(x[,i])
					if (is.factor(x[,i])) x[,i] <- as.numeric(x[,i])
					y[,i] <- x[,i]
				}
				options(bigmemory.typecast.warning=oldbtw)
				junk <- gc() 
			} else stop('bigmemory: that type is not implemented.')
			return(y)
		})

setMethod('asBigMatrix', signature(x='vector', dimnames = "logical"),
		function(x, type, separated, backingfile, backingpath, descriptorfile,
				binarydescriptor, shared, dimnames)
		{
			if (!is.numeric(x)) {
				warning("Casting to numeric type")
				x <- as.numeric(x)
			}
			x <- matrix(x, length(x), 1)
			warning("Coercing vector to a single-column matrix.")
			return(asBigMatrix(x, type, separated, backingfile, 
							backingpath, descriptorfile, binarydescriptor, shared, dimnames))
		})


bmBinFilename = function(stem) {
	sprintf("%s.bin", stem)
}

bmDescFilename = function(stem) {
	sprintf("%s.desc", stem)
}

bin2DescFilename = function(bin_file) {
	bmDescFilename(tools::file_path_sans_ext(bin_file))
}

desc2BinFilename = function(desc_file) {
	bmBinFilename(tools::file_path_sans_ext(desc_file))
}

readBmBin = function(bin_file, ncols_to_read, desc_type = "binary") {
	desc_file = bin2DescFilename(bin_file)
	desc = readDesc(desc_file, desc_type)
	nrow = desc@description$nrow
	fh = file(bin_file, "rb")
	tryCatch({
				res = readBin(fh, 
						{
						t = desc@description$type
						if(t == "integer" || t == "double") {
							t
						} else {
							"raw"
						}
						},
						desc@description$nrow * ncols_to_read)
			}, finally = {
				close(fh)
			})
	matrix(res, nrow)
}