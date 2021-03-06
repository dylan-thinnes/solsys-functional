All paths in this INSTRUCTION file are relative to the directory in which the
file resides.

### COMPILING MSIEVE WRAPPER

#### STEP 0: Extract msieve
A .tar.gz of msieve 1.53 has been provided in ./Primes. Extract this to
Primes/msieve.
Alternately, download you own copy, though I cannot guarantee this version will
work with the glue code I have written.

#### STEP 1: Object files for msieve
Compile msieve's .a file (containing all .o files we need to link against)
by running the following command in Primes/msieve: 
$ make all <options...>

You can customize the options depending on which libraries you have on your
system. Use `make` to see what options are available.

#### STEP 2: Object files for glue/msieve/glue.c
Primes/glue/msieve/glue.c provides the factor_number function, which takes a string representing
the number to factor, and returns the number's factors as a string.
This string can be read by Haskell as a list of integers, e.g. the return
string for "15" is "[3,5]".

To produce the object file for glue.c, we compile it with the relevant
libraries, and make sure to include headers from Primes/msieve/include:
$ gcc -Wall Primes/glue/msieve/glue.c Primes/msieve/libmsieve.a -lgmp -lm -lpthread -ldl -c -IPrimes/msieve/include

#### STEP 3: Compile Haskell library
src/Factor.hs provides the Haskell bindings to use glue.c. Whenever including
it in a module definition, you must include the glue.o and libmsieve.a files
while compiling.
$ ghc Primes/Factor.hs Primes/glue/msieve/glue.o Primes/msieve/libmsieve.a

If main.hs imports Factor.hs,
$ ghc main.hs Primes/glue/msieve/glue.o Primes/msieve/libmsieve.a

### COMPILING PRIMECOUNT WRAPPER

#### STEP 0: Get a copy of kimwalisch/primecount
The glue code in step 1 was created with version 4.4. Support for other
versions is completely unknown.

#### STEP 1: Compile kimwalisch/primecount as a shared library.
**NOTE:** Oddly enough, I'd expect the libprimecount.a file to link fine, but GHC
cannot read the identifiers out of it. If anyone can help with this, please
submit an issue or PR.

We must create a shared object file which we can link against with GHC.
```sh
cmake . -DBUILD_SHARED_LIBS=ON
make
```

#### STEP 2: Compile the glue code.
The glue to provide pi externally in a C style is defined as pi_extern in
Primes/glue/primecount/glue.cc. Compile it into an object file with the following command:
```sh
g++ Primes/glue/primecount/glue.cc -c
```

#### STEP 3: Link and compile the Primecount.hs module
The Primecount module exports a function called pix, which calculates the
number of primes up to a given Integer n.  
Compile it while linking against the relevant object files:
```sh
ghc Primes/Primecount.hs -lgomp -lstdc++ Primes/primecount/libprimecount.so Primes/glue/primecount/glue.o -optl-fexceptions
```

Any produced executables using Primecount will need to have access to the
shared object file at primecount/libprimecount.so. The executable will search
in predefined paths in your system, as well as on any paths defined in the
LD_LIBRARY_PATH environment variable

### COMPILING LOGINT WRAPPER

#### STEP 0: Compile li.cpp
The logint function is defined in and exported from li.cpp. Compile it to an
object file with the following command:
```sh
g++ Primes/logint/li.cpp -lmpfr -c
```

Make sure to have MPFR installed.

#### STEP 1: Link and compile the Logint module
The Logint module exports a single function, logint, which calculates the
logarithmic integral of a given Integer.
Compile it as follows:
```sh
ghc Primes/Logint.hs Primes/logint/li.o -lstdc++ -lmpfr
```

### LINKING EVERYTHING

If you have a main file, such as ./Main.hs, which imports all of these modules,
the necessary object files for ghc in all object files must be included.  The
command that compiles main.hs while including them all, assuming the object
files are in the same place as their source, is as follows:
```sh
stack ghc -- Main.hs -lgomp -lstdc++ -lmpfr -lgmp Primes/glue/msieve/glue.o Primes/msieve/libmsieve.a Primes/primecount/libprimecount.so Primes/glue/primecount/glue.o Primes/logint/li.o -optl-fexceptions
```

To execute the resulting Main, include the required primecount shared library
on its path. One way to do this is using LD_LIBRARY_PATH, as noted in the
Primecount section.
```sh
LD_LIBRARY_PATH=Primes/primecount/ ./Main
```
