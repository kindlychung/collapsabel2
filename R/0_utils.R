
dir.create2 = function(dir) {
	if(!file.exists(dir)) {
		dir.create(dir, recursive = TRUE)
	} else {
		TRUE
	}
}

file.create2 = function(filename) {
	if(!file.exists(filename)) {
		file.create(filename, recursive = TRUE)
	} else {
		TRUE
	}
}

isSQLite3 = function(filename) {
	if(!file.exists(filename)) {
		return(FALSE)
	} 
	if(file.info(filename)$size < 100){
		return(FALSE)
	}
	con = file(filename, "rb")
	tryCatch({
				header = rawToChar(readBin(con, "raw", 15))
				return(header == "SQLite format 3")
			}, finally = {
				close(con)
			})
}
