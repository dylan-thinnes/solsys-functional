#include <primecount.hpp>
#include <stdio.h>
#include <string>

extern "C" {
    char * pi_extern(char * input) {
        char * out = (char *) malloc(400);
        std::string x = std::string(input);
        std::string res = primecount::pi(x);
        return strcpy(out, res.c_str());
    }
}
