#include "countlines.h"

//' Count the number of lines in a file
//'
//' @useDynLib CollapsABEL
//' @importFrom Rcpp sourceCpp evalCpp
//' @param fn Input filepath
//' @return A integer for the number of lines
//' @export
//' @examples
//' countlines("/tmp/x.txt")
// [[Rcpp::export]]
size_t countlines(std::string fn) {
    std::ifstream in_file(fn.c_str());
    size_t nlines =  (size_t) std::count(std::istreambuf_iterator<char>(in_file), std::istreambuf_iterator<char>(), '\n');

    // nlines needs be corrected when the last line is not ended with a newline
    in_file.seekg(-1, in_file.end);
    unsigned char lastchar = in_file.get();
    if(lastchar != '\n') {
        ++nlines;
    }
    return nlines;
}





