#!/bin/sh

#EMCC_DEBUG=1 


# Make libmonero-cpp.dylib
cd ./external/monero-cpp-library/ && 
./bin/build_libmonero_cpp.sh &&

# Copy libmonero-cpp.dylib to ./build
cd ../../ &&
mkdir -p ./build &&
cp ./external/monero-cpp-library/build/libmonero-cpp.dylib ./build &&

# Make libmonero-java.dylib
HOST_NCORES=$(nproc 2>/dev/null || shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
cd build && 
cmake .. && 
cmake --build . -j$HOST_NCORES && 
make .