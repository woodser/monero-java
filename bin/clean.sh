#!/bin/bash

rm -rf build
cd external/monero-cpp
bin/clean.sh || exit 1

