#!/bin/bash

#EMCC_DEBUG=1

###################
# Set Global Vars 
###################

CURRENT_ARCH=`uname -m`
CURRENT_OS=`uname -s`
HOST_NCORES=$(nproc 2>/dev/null || shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
TOOLCHAIN_FILE=""
BUILD_BOTH_ARCHS=0
OS=""
VENDOR=""

# Set TARGET if it is not supplied. 
TEMPTARGET=$TARGET
if [ -z $TARGET ]; then
    if [ $CURRENT_OS == "Linux" ]; then
        TEMPTARGET="linux"
    elif [ $CURRENT_OS == "Darwin" ]; then
        TEMPTARGET="darwin"
    else
        TEMPTARGET="MSYS"
    fi
fi
TARGET=$TEMPTARGET    

if [ "${TARGET}" == "darwin" ]; then
    OS="darwin11"
    VENDOR="apple"
    if [ -z "${ARCH}" ] && [ $CURRENT_OS == "Linux" ]; then
        BUILD_BOTH_ARCHS=1
    fi
elif [ "${TARGET}" == "MSYS" ] || [ "${TARGET}" == "MINGW64_NT" ]; then
    OS="mingw32"
    VENDOR="w64"
else
    OS="gnu"
    VENDOR="linux"
fi

CPU=""
if [ -n "${ARCH}" ]; then
    CPU="${ARCH}"
else
    CPU=$CURRENT_ARCH 
fi
VERSION="${CPU}-${VENDOR}-${OS}"


###################
# Build Stuff 
###################

# Build monero-cpp and installed it
# "./build" is created here also.
if [ -z $SKIP_MCPP ]; then
    rm -rf build
    printf "\nlibmonero-java build script: Building libmonero-cpp shared library\n"
    if [ $BUILD_BOTH_ARCHS == 1 ]; then
        cd ./external/monero-cpp/ &&
        SKIP_MP=$SKIP_MP TARGET=$TARGET ./bin/build_libmonero_cpp.sh || exit 1
        cd ../..
    else
        cd ./external/monero-cpp/ &&
        ARCH=$CPU SKIP_MP=$SKIP_MP TARGET=$TARGET ./bin/build_libmonero_cpp.sh || exit 1
        cd ../..
    fi
    mkdir build
    if [ $BUILD_BOTH_ARCHS == 1 ]; then
        cp external/monero-cpp/build/darwin/release/libmonero-cpp.dylib ./build || exit 1
    else
        cp external/monero-cpp/build/${VERSION}/release/libmonero-cpp* ./build || exit 1
    fi
else
    mkdir temp_cpp
    mv build/libmonero-cpp* temp_cpp
    rm -rf build
    mv temp_cpp build
fi

# Build monero-java-jni-bridge
mkdir -p build/${VERSION}/release

# Building on Linux using Depends
if [ $CURRENT_OS == "Linux" ] && (
    [ $TARGET == "linux" ] && [ $CURRENT_ARCH != $CPU ] ||
    [ $TARGET != "linux" ]); then

    toolchainBuildJNI () {
        # $1 is the target triplet.
        # CWD is the project root.
        # dependencies are already in their proper place.

        printf "\nBuilding monero-java-jni for ${1} using Depends\n"
        TOOLCHAIN_FILE="external/monero-cpp/external/monero-project/contrib/depends/${1}/share/toolchain.cmake"
        cd  "build/${1}/release" && 
        cmake -D TARGET=$1 -D CMAKE_BUILD_TYPE=Release -D CMAKE_TOOLCHAIN_FILE=../../../$TOOLCHAIN_FILE ../../.. &&
        make -j$HOST_NCORES
        cd ../../..
    }

    # Build using Depends

    if [ $BUILD_BOTH_ARCHS == 1 ]; then
        mkdir -p "build/x86_64-${VENDOR}-${OS}/release"
        mkdir -p "build/aarch64-${VENDOR}-${OS}/release"

        # x86_64
        cp build/libmonero-cpp.dylib "build/x86_64-${VENDOR}-${OS}/release"
        toolchainBuildJNI "x86_64-apple-darwin11"

        # aarch64
        cp build/libmonero-cpp.dylib "build/aarch64-${VENDOR}-${OS}/release"
        toolchainBuildJNI "aarch64-apple-darwin11"

        # lipo it together and store correctly 
        mkdir -p build/darwin/release
        external/monero-cpp/external/monero-project/contrib/depends/${CURRENT_ARCH}-apple-darwin11/native/bin/${CURRENT_ARCH}-apple-darwin11-lipo -create -output build/darwin/release/libmonero-java.dylib build/aarch64-${VENDOR}-${OS}/release/libmonero-java.dylib build/x86_64-${VENDOR}-${OS}/release/libmonero-java.dylib

    else
        # Build 1 archive using Depends
        if [ $TARGET == "linux" ]; then
            cp build/libmonero-cpp.so build/${VERSION}/release
        elif [ $TARGET == "darwin" ]; then
            cp build/libmonero-cpp.dylib build/${VERSION}/release
        else
            cp build/libmonero-cpp.dll build/${VERSION}/release
        fi
        toolchainBuildJNI $VERSION
    fi

else
    # Build monero-java-jni-bridge without using Depends
    # Only build for the current platform

    printf "\nBuilding monero-java-jni for ${VERSION} without Depends\n"

cd build && 
cmake .. && 
cmake --build . -j$HOST_NCORES && 
make .
cd ..
    cp build/libmonero-java* build/${VERSION}/release
    if [ $TARGET == "linux" ]; then
        cp build/libmonero-cpp.so build/${VERSION}/release
    elif [ $TARGET == "darwin" ]; then
        cp build/libmonero-cpp.dylib build/${VERSION}/release
    else
        cp build/libmonero-cpp.dll build/${VERSION}/release
    fi
fi
