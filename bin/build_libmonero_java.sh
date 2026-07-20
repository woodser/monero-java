#!/bin/sh

#EMCC_DEBUG=1

HOST_NCORES=$(nproc 2>/dev/null || shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)

# build monero-cpp static library (also builds monero-project static dependencies)
cd ./external/monero-cpp/ &&
./bin/build_libmonero_cpp.sh &&

# build standalone libmonero-java library with dependencies linked statically
cd ../../ &&
mkdir -p ./build &&
cd build &&
cmake -DSTATIC=ON .. &&
cmake --build . -j$HOST_NCORES &&

# stage the built library into lib/<platform>/ so the jar bundles it (mirrors MoneroUtils load paths)
case "$(uname -s)-$(uname -m)" in
  Darwin-arm64)          PLATFORM=mac-arm64;    LIB=libmonero-java.dylib;;
  Darwin-x86_64)         PLATFORM=mac-x86_64;   LIB=libmonero-java.dylib;;
  Linux-aarch64)         PLATFORM=linux-arm64;  LIB=libmonero-java.so;;
  Linux-x86_64)          PLATFORM=linux-x86_64; LIB=libmonero-java.so;;
  MINGW*|MSYS*|CYGWIN*)  PLATFORM=windows;      LIB=libmonero-java.dll;;
  *) echo "Unsupported platform: $(uname -s)-$(uname -m)" >&2; exit 1;;
esac &&
mkdir -p ../lib/$PLATFORM &&
rm -f ../lib/$PLATFORM/libmonero-cpp.* &&
cp ./$LIB ../lib/$PLATFORM/
