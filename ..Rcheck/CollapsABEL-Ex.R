pkgname <- "CollapsABEL"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "CollapsABEL-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('CollapsABEL')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("PlInfo")
### * PlInfo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlInfo
### Title: An S4 class representing info about plink files
### Aliases: .PlInfo PlInfo

### ** Examples

\donotrun{
x = .PlInfo()
x@plink_trio = c("/tmp/a.pdf")
validObject(x) # error
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlInfo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlInfo_constructor")
### * PlInfo_constructor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlInfo_constructor
### Title: Constructor for PlInfo class
### Aliases: PlInfo_constructor plInfo,PlInfo,character,missing-method
###   plInfo,missing,character,logical-method
###   plInfo,missing,character,missing-method

### ** Examples

\donotrun{
pl_info = plInfo(.PlInfo(), "/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/mmp13")
isSetup(pl_info) # false
setup(pl_info)
isSetup(pl_info) # true
bim_ff = suppressMessages(loadBim(pl_info))
head(bim_ff)
fam_ff = loadFam(pl_info)
head(fam_ff)
summary(fam_ff[, "IID"])
which(fam_ff[, "IID"] == "10425")
frq_ff = loadFrq(pl_info)
head(frq_ff)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlInfo_constructor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("baseName")
### * baseName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: baseName
### Title: Basename of a FilePath object
### Aliases: baseName

### ** Examples

fp = filePath(R.home())
baseName(fp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("baseName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("changeByMap")
### * changeByMap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: changeByMap
### Title: Transform a vector by a mapping
### Aliases: changeByMap

### ** Examples

names_dat = data.frame(c("a", "b", "c"), c("d", "e", "f"), stringsAsFactors=FALSE)
changeByMap(c("a", "a", "b"), names_dat) == c("d", "d", "e")
x = changeByMap(c(NA, "a", "b"), names_dat)
is.na(x[1])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("changeByMap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("charify")
### * charify

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: charify
### Title: Convert certain columns of a data.frame to character type
### Aliases: charify

### ** Examples

\donotrun{
x = data.frame(x = 1:3, y= 2:4)
all(colClasses(x) == c("integer", "integer"))
x = charify(x, "x")
all(colClasses(x) == c("character", "integer"))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("charify", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("checkFileExist")
### * checkFileExist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: checkFileExist
### Title: Stop when any file does not exist
### Aliases: checkFileExist

### ** Examples

\donotrun{
checkFileExist(R.home())
checkFileExist(sapply(1:5, function(i) tempfile()))
checkFileExist(sapply(1:5, function(i) tempdir()))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("checkFileExist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("colClasses")
### * colClasses

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: colClasses
### Title: Get classes of columns of a data.frame
### Aliases: colClasses

### ** Examples

dat = data.frame(x = 15L, y = 3.14, z = "abc",
  u = TRUE, stringsAsFactors = FALSE)
all(colClasses(dat) ==
				c("integer", "numeric",
						"character", "logical"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("colClasses", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("correctTypes_methods")
### * correctTypes_methods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: correctTypes_methods
### Title: Convert columns of a data frame to certain types
### Aliases: correctTypes,data.frame,missing,character-method
###   correctTypes,data.frame,numeric,character-method correctTypes_methods

### ** Examples

\donotrun{
dat = randNormDat(3, 3)
dat[, 2] = as.character(dat$V2)
dat1 = correctTypes(dat, types = rep("numeric", 3))
all(colClasses(dat1) == rep("numeric", 3))
dat2 = correctTypes(dat, 2, "numeric")
all(colClasses(dat2) == rep("numeric", 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("correctTypes_methods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dirName")
### * dirName

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dirName
### Title: Directory name of a file path
### Aliases: dirName

### ** Examples

fp = filePath(R.home())
dirName(fp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dirName", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ifLen")
### * ifLen

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ifLen
### Title: IfLen macro
### Aliases: ifLen

### ** Examples

ifLen(c(1, 2), { print('yes!') }, {print("no!")})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ifLen", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ifLet")
### * ifLet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ifLet
### Title: IfLet macro
### Aliases: ifLet

### ** Examples

ifLet(..temp.., TRUE, {print(paste("true.", as.character(..temp..)))},
		{print(paste("false.", as.character(..temp..)))})
ifLet("..temp..", TRUE, {print(paste("true.", as.character(..temp..)))},
		{print(paste("false.", as.character(..temp..)))})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ifLet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ifLetLen")
### * ifLetLen

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ifLetLen
### Title: IfLetLen macro
### Aliases: ifLetLen

### ** Examples

ifLetLen("..temp..", 1:3, {print(paste("true.", as.character(..temp..)))},
		{print(paste("false.", as.character(..temp..)))})



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ifLetLen", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("isBinary")
### * isBinary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: isBinary
### Title: Check whether a trait is binary
### Aliases: isBinary

### ** Examples

\donotrun{
!isBinary(c(1, 1.1, 1, 1.1, NA))
isBinary(c(1, 2, 1, 2, NA))
!isBinary(c(-9, 2.3, 4.1, -9, -9), -9)
isBinary(c(-9, 2, 4, -9, -9), -9)
isBinary(c(1, 2, 2, 1, -9, -9.9), c(-9, -9.9))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("isBinary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("isSetup-PlInfo-method")
### * isSetup-PlInfo-method

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: isSetup,PlInfo-method
### Title: Check if a directory containing .bed .fam and .bim files is
###   properly setup
### Aliases: isSetup,PlInfo-method

### ** Examples

# see examples in plInfo



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("isSetup-PlInfo-method", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lenCheck")
### * lenCheck

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lenCheck
### Title: Check each element of a list has expected length
### Aliases: lenCheck

### ** Examples

x = list(1, 2, 3)
str(x[c(1, 3)])
lenCheck(list(1, 2, 3), c(1, 1, 0))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lenCheck", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nonExistentFiles-character-method")
### * nonExistentFiles-character-method

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nonExistentFiles,character-method
### Title: Non-existent files from a vector of filenames
### Aliases: nonExistentFiles,character-method

### ** Examples

\donotrun{
nonExistentFiles(R.home())
nonExistentFiles(sapply(1:5, function(i) tempfile()))
nonExistentFiles(sapply(1:5, function(i) tempdir()))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nonExistentFiles-character-method", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rbedInfo")
### * rbedInfo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rbedInfo
### Title: Constructor of RbedInfo class
### Aliases: rbedInfo

### ** Examples

\donotrun{
rbed_info = rbedInfo(bedstem = "/users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/test", TRUE)
rbed_info@nsnp
rbed_info@nindiv
rbed_info@bytes_snp
theoBedSize(rbed_info)
realBedSize(rbed_info)
bedSizeCorrect(rbed_info)
bed_full = readBed(rbed_info)
dim(bed_full)
print(bed_full)
bed1 = readBed(rbed_info, 1:4)
print(bed1)
bed1a = readBed(rbed_info, paste("snp", 1:4, sep=""))
all(na.omit(bed1a == bed1))
bed2 = readBed(rbed_info, 3:6)
bed2a = readBed(rbed_info, paste("snp", 3:6, sep = ""))
all(na.omit(bed2 == bed2a))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rbedInfo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readAssoc")
### * readAssoc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readLogistic
### Title: Read .qassoc files
### Aliases: readAssoc readLinear readLogistic readQassoc

### ** Examples

\donotrun{
dat = readAssoc("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc")
head(dat)
all(c(dat[1, 1] == 11,
				dat[2, 2] == "rs7127954",
				dat[3, 3] == 101943700,
				dat[4, 4] == "A",
				dat[5, 5] == 0.4369))
all(colClasses(dat) == c("integer", "character", "integer",
		"character", "numeric", "numeric",
		"character", "numeric", "numeric", "numeric"))
dat = readQassoc("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc")
head(dat)
all(c(dat[1, 1] == 11,
				dat[2, 2] == "rs7127954",
				dat[3, 3] == 101943700,
				dat[4, 4] == 831,
				dat[5, 5] == 0.12400,
				dat[6, 6] == 0.4211))
all(
		colClasses(qassoc) == c("integer", "character",
		"integer", "integer",
		"numeric", "numeric",
		"numeric", "numeric", "numeric"))
linear = readLinear("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc.linear")
head(linear)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readAssoc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readBim")
### * readBim

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readBim
### Title: Read plink .bim files
### Aliases: readBim

### ** Examples

\donotrun{
bim = readBim("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.bim",
		cn_select = "..all")
head(bim)
summary(bim)
bim_info = bimInfo("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.bim")
bim_info@cnames
bim1 = bim_info@read_fun(bim_info, c("CHR", "SNP", "BP"))
head(bim1)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readBim", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readFam")
### * readFam

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readFam
### Title: Read plink .fam files
### Aliases: readFam

### ** Examples

\donotrun{
fam = readFam("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.fam", cn_select = "..all")
head(fam)
summary(fam)
fam_info = famInfo("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.fam")
fam_info@cnames
fam1 = fam_info@read_fun(fam_info, c("FID", "IID"))
head(fam1)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readFam", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readInfo")
### * readInfo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readInfo
### Title: ReadInfo constructor
### Aliases: readInfo readInfo,character,character-method

### ** Examples

\donotrun{
ri = readInfo("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/collapsabel2/tests/testthat/mmp13.frq")
getSlots("ReadInfo")
ri@cnames
ri@filename
ri@header
frq = ri@read_fun(ri, cn_select = "..all")
head(frq)
print(ri@cnames)
frq1 = ri@read_fun(ri, ri@cnames[1:4])
head(frq1)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readInfo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readLiteral")
### * readLiteral

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readLiteral
### Title: Read a file literally (all columns as character)
### Aliases: readLiteral

### ** Examples

\donotrun{
df = data.frame(x = c("T", "%T", "10341"),
		y = c("F", "f%t", "431"),
		z = c("T", "TRUE", "FALSE"))
tmpf = tempfile()
write.table(df, file = tmpf, quote = FALSE,
		row.names = FALSE, col.names = FALSE)
system(sprintf("head %s", tmpf))
df1 = readLiteral(file = tmpf)
all(df1 == df)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readLiteral", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readPlinkOut")
### * readPlinkOut

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readPlinkOut
### Title: Read plink output files
### Aliases: readPlinkOut

### ** Examples

\donotrun{
dat1 = readPlinkOut("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc")
dat2 = readAssoc("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc")
all(na.omit(dat1 == dat2))
dat1 = readPlinkOut("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc",
		c("CHR", "SNP", "P", "OR"))
dat2 = readAssoc("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.assoc",
		c("CHR", "SNP", "P", "OR"))
all(na.omit(dat1 == dat2))
dat1 = readPlinkOut("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc")
dat2 = readQassoc("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc")
all(na.omit(dat1 == dat2))
dat1 = readPlinkOut("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc",
		c("CHR", "SNP", "P", "R2"))
dat2 = readQassoc("/Users/Kaiyin Zhong, Fan Liu/EclipseWorkspace/CollapsABEL/tests/testthat/assoc/mmp13.qassoc",
		c("CHR", "SNP", "P", "R2"))
all(na.omit(dat1 == dat2))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readPlinkOut", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("reprClasses")
### * reprClasses

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: reprClasses
### Title: Represent classes of a data.frame in a character vector
### Aliases: reprClasses

### ** Examples

\donotrun{
dat = randNormDat(4, 2)
x = capture.output(reprClasses(dat), file = NULL)
x = eval(parse(text = x))
all(x == colClasses(dat))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("reprClasses", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("setup-PlInfo-method")
### * setup-PlInfo-method

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: setup,PlInfo-method
### Title: Setup up a directory containing plink files
### Aliases: setup,PlInfo-method

### ** Examples

# see examples in plInfo



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("setup-PlInfo-method", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("shiftedStem")
### * shiftedStem

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: shiftedStem
### Title: Add a "shift" suffix to a stem
### Aliases: shiftedStem

### ** Examples

# add suffix to stem
shiftedStem("a", 100) == "a_shift_0100"
shiftedStem("home/a", 100) == "home/a_shift_0100"
shiftedStem("/home/a", 100) == "/home/a_shift_0100"
shiftedStem(c("/home/a", "/home/b"), 100) == c("/home/a_shift_0100",
		"/home/b_shift_0100")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("shiftedStem", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("stopFormat")
### * stopFormat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: stopFormat
### Title: Stop with format string
### Aliases: stopFormat

### ** Examples

\donotrun{
stopFormat("You should put file here: %s", R.home())
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("stopFormat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("strConcat")
### * strConcat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: strConcat
### Title: Concatenate a vector of strings
### Aliases: strConcat strConcat,character,missing-method

### ** Examples

strConcat(letters)
strConcat(letters, " ")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("strConcat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("strVectorRepr_methods")
### * strVectorRepr_methods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: strVectorRepr_methods
### Title: String Representation of a character vector
### Aliases: numVectorSQLRepr strVectorRepr,character,missing-method
###   strVectorRepr_methods strVectorSQLRepr

### ** Examples

strVectorRepr(letters[1:3]) == 'c("a", "b", "c")'
strVectorRepr(
  as.character(1:3)) == 'c("1", "2", "3")'
all(eval(parse(text = strVectorRepr(as.character(1:3)))) ==
  c("1", "2", "3"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("strVectorRepr_methods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("systemFormat")
### * systemFormat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: systemFormat
### Title: Call system command with format string
### Aliases: systemFormat

### ** Examples

\donotrun{
systemFormat("ls %s", R.home())
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("systemFormat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
