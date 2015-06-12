#ifndef DEBUGPRINT_H
#define DEBUGPRINT_H

#include <iostream>

template <typename T>
void debugPrint(T t)
{
    std::cout << t;
}

template<typename T, typename... Args>
void debugPrint(T t, Args... args) // recursive variadic debugPrinttion
{
    std::cout << t;
    debugPrint(args...) ;
}




#endif // DEBUGPRINT_H
