#include <primecount.hpp>
#include <string>
#include "test.h"

extern "C" {
    const char * pi_extern(char * input) {
        std::string x = std::string(input);
        std::string res = primecount::pi(x);
        return res.c_str();
    }
}
