#!/bin/bash

cd build &&
cp ../../../external/monero-cpp/macos_universal/arm/build/libmonero-cpp.dylib . &&
cmake -DCMAKE_TOOLCHAIN_FILE=../../../external/monero-cpp/external/monero-project/contrib/depends/aarch64-apple-darwin11/share/toolchain.cmake .. &&
make

