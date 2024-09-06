#!/bin/bash
cd build && rm -f libmonero-java.dylib && ../../../external/monero-cpp/external/monero-project/contrib/depends/x86_64-apple-darwin11/native/bin/x86_64-apple-darwin11-lipo -create -output libmonero-java.dylib ../../x86/build/libmonero-java.dylib ../../arm/build/libmonero-java.dylib 
