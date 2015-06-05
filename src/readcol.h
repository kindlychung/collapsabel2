#ifndef READCOL_H
#define READCOL_H

#include <vector>
#include <string>

std::vector< std::string > readcol(std::string fileName, size_t colNum, size_t nSkip, size_t maxRowNum);

#endif // READCOL_H
