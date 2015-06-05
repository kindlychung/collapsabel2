#include "fileExists.h"

//' Check if a file exists, if it does, do nothing, otherwise throw an exception
//'
//' @param name Filename / file path
//' @export
//' @examples
//' fileExists("/tmp/x.txt")
void fileExists (const std::string& name) {
    if ( access( name.c_str(), F_OK ) == -1 ) {
        throw std::string("File does not exist!");
    }
}

bool fileExistsBool(const std::string& name) {
    if (FILE *file = fopen(name.c_str(), "r")) {
        fclose(file);
        return true;
    } else {
        return false;
    }
}


void fileExists(const std::string& name, bool exitIfNotExist, bool exitIfExist) {
    bool existStatus = fileExistsBool(name);
    if(not existStatus) {
        if(exitIfNotExist) {
            std::cerr << "File " << name << " does not exist!\n";
            exit(1);
        }
    }
    else {
        if(exitIfExist) {
            std::cerr << "File " << name << " already exists!\n";
            exit(1);
        }
    }
}

bool fileExists(const std::string& name, bool rmIfExist) {
    bool existStatus = fileExistsBool(name);
    if(existStatus) {
        if(rmIfExist) {
            std::cout << "Removing exsiting file: " << name << "\n";
            if( remove(name.c_str()) != 0 ) {
                std::cerr << "Failed to remove " << name << "!\n";
                exit(1);
            }
        }
    }
    return existStatus;
}


