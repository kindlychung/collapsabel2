#include "fileSize.h"

off_t fileSize(std::string fn)
{
    struct stat fsStatBuf;
    stat(fn.c_str(), &fsStatBuf);
    off_t fileLen = fsStatBuf.st_size;
    return fileLen;
}
