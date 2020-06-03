#!/bin/sh

#EMCC_DEBUG=1

HOST_NCORES=$(nproc 2>/dev/null || shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)

# build libmonero-cpp shared library
cd ./external/monero-cpp/ && 
./bin/build_libmonero_cpp.sh &&

# copy libmonero-cpp shared library to ./build
cd ../../ &&
mkdir -p ./build &&
cp ./external/monero-cpp/build/libmonero-cpp.* ./build &&

# build libmonero-java shared library to ./build
cd build && 
cmake .. && 
cmake --build . -j$HOST_NCORES && 
make .