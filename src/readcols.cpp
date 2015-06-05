/*
 * readcols.cpp
 *
 *  Created on: Aug 11, 2014
 *      Author: kaiyin
 */

#include "readcols.h"
#include "debugPrint.h"



//' Read columns of a whitespace delimited text file
//'
//' @param fn input filepath
//' @param colsel a vector of target column numbers
//' @param nFirstSkipLines Integer. Number of lines to skip in the beginning
//' @param nSkipUnit Integer M. Let the function read one line out of every M
//' @return A matrix of strings from selected columns
//' @export
//' @examples
//' readcol("/tmp/x.txt", c(1, 3, 5), 1, 3)
// [[Rcpp::export]]
Rcpp::CharacterMatrix readcols(std::string fn,
		std::vector<unsigned long> colsel, size_t nFirstSkipLines,
		size_t nSkipUnit) {

	if (colsel.empty()) {
		throw std::string("You didn't select any column!");
	}


	size_t nc_file = ncols(fn);
	size_t nr_file = countlines(fn);
	size_t nr_left = nr_file - nFirstSkipLines;
	size_t nr = (size_t) (nr_left / nSkipUnit);
	size_t nc = colsel.size();
#ifdef txtutilsDebug
    debugPrint("File has ", nc_file, " columns \n");
    debugPrint("File has " , nr_file , " rows \n");
    debugPrint("You want to skip the first " ,  nFirstSkipLines , " lines \n");
    debugPrint("Lines left: " , nr_left , "\n");
	debugPrint("Of the rest you want to select one line out of every " , nSkipUnit , " lines \n");
	debugPrint("You selected " , nr , " rows \n");
	debugPrint("You selected " , nc , " columns \n");
#endif


	{
		size_t remainder = (size_t) ((nr_file - nFirstSkipLines) % nSkipUnit);
		if (remainder != 0) {
            std::cerr << "Number of lines to read is not a multiple of nSkipUnit! \n";
			nr++;
		}
		else {
#ifdef txtutilsDebug
			debugPrint("Number of lines to read is a multiple of nSkipUnit \n");
#endif
		}
	}

	unsigned long colsel_max = *std::max_element(colsel.begin(), colsel.end());
#ifdef txtutilsDebug
	debugPrint("Calculating max column number...\n");
	debugPrint("Max column number is " , colsel_max , "\n");
#endif

	if (colsel_max > nc_file) {
		throw std::string("Some col number(s) are out of range!");
	}

	// c++ is 0-based, adjust for it
	for (unsigned long i = 0; i < colsel.size(); i++) {
		colsel[i]--;
	}

	// initialize a 2d vector (matrix) with fixed size
//	std::vector< std::vector<std::string> > res(nc, std::vector<std::string>(nr));
#ifdef txtutilsDebug
    debugPrint("Allocating a matrix of " , nr , " rows and " , nc , " columns.\n");
#endif
	Rcpp::CharacterMatrix res(nr, nc);
	std::ifstream infile(fn.c_str());
	std::string tmpline;

	// skip lines in the beginning
	for (int lineIter = 0; lineIter < nFirstSkipLines; lineIter++) {
#ifdef txtutilsDebug
		debugPrint("Skipping line " , lineIter + 1 , "\n");
#endif
		getline(infile, tmpline);
	}


	size_t rowIter = 0;
	for (size_t lineIter = 0; lineIter < nr_left; lineIter++) {
		std::string line;
		getline(infile, line);
		if (lineIter % nSkipUnit == 0) {
			std::istringstream lineStream(line);
			size_t colIter = 0;
			for (size_t wordIter = 0; wordIter <= colsel_max; wordIter++) {
				std::string tmpword;
				lineStream >> tmpword;
				if (std::find(colsel.begin(), colsel.end(), wordIter)
						!= colsel.end()) {
					res(rowIter, colIter) = tmpword;
					colIter++;
				}
			}
			rowIter++;
		}
	}

    return res;

}

