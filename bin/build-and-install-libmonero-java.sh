#!/bin/sh

#EMCC_DEBUG=1 

# Make libmonero-cpp.dylib and copy to ./external-libs
cd ./external/monero-cpp-library/ && 
./bin/build-libmonero-cpp.sh &&
cd ../../ && 
mkdir -p ./external-libs &&
cp ./external/monero-cpp-library/build/libmonero-cpp.dylib ./external-libs

# Make libmonero-java.dylib and copy to ./lib
mkdir -p build && 
cd build && 
cmake .. && 
cmake --build . && 
make .
cd ../
mkdir -p ./lib &&
cp ./build/libmonero-java.dylib ./lib