/*
 * printlines.cpp
 *
 *  Created on: Jul 26, 2014
 *      Author: kaiyin
 */
#include "printlines.h"

//' print all the lines of a text file.
//'
//' @param fn Input filepath
//' @export
//' @examples
//' printlines("/tmp/x.txt")
// [[Rcpp::export]]
void printlines(std::string fn) {
	std::ifstream infile(fn.c_str());
	while(infile) {
		std::string l;
		std::getline(infile, l);
		std::cout << l << "\n";
	}
}
