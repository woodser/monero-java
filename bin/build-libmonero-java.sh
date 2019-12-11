#!/bin/sh

#EMCC_DEBUG=1 

# Make libmonero-cpp.dylib
cd ./external/monero-cpp-library/ && 
./bin/build-libmonero-cpp.sh &&

# Make libmonero-java.dylib and copy to ./lib
cd ../../ &&
mkdir -p ./build && 
cd build && 
cmake .. && 
cmake --build . && 
make .