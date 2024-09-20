#!/bin/sh

#EMCC_DEBUG=1

export HOST_NCORES=${HOST_NCORES-$(nproc 2>/dev/null || shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)}
BUILD_DIR="$(pwd)/build"
export CMAKE_PREFIX_PATH="$(pwd)/external/monero-cpp/build/install"${CMAKE_PREFIX_PATH+:$CMAKE_PREFIX_PATH}
export USE_DEVICE_TREZOR=OFF
echo "HOST_NCORES=$HOST_NCORES in $0"
echo "CMAKE_PREFIX_PATH=$CMAKE_PREFIX_PATH in $0"
echo "USE_DEVICE_TREZOR=$USE_DEVICE_TREZOR in $0"

[ -d $BUILD_DIR ] || mkdir -p $BUILD_DIR

# build libmonero-cpp static library
(cd ./external/monero-cpp/ && \
./bin/build_libmonero_cpp.sh -DSTATIC=ON) && \

# copy libmonero-cpp static library to ./build
cp ./external/monero-cpp/build/libmonero-cpp.* $BUILD_DIR && \

# build libmonero-java shared library to ./build
cd $BUILD_DIR && \
cmake -D USE_DEVICE_TREZOR=$USE_DEVICE_TREZOR .. && \
cmake --build . -j$HOST_NCORES && 
make -j$HOST_NCORES .
