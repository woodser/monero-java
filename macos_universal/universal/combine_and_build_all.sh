#!/bin/bash

# build arm
cd ../arm && ./clean.sh && ./build.sh
# build x86
cd ../x86 && ./clean.sh && ./build.sh
# combine
./combine.sh
