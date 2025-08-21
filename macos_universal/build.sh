#!/bin/bash
cd .. && ./bin/update_submodules.sh
cd external/monero-cpp/macos_universal && ./build.sh
cd ../../../macos_universal/universal && ./combine_and_build_all.sh
