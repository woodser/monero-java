#!/bin/sh

#EMCC_DEBUG=1 

mkdir -p build && 
cd build && 
cmake .. && 
cmake --build . && 
make .