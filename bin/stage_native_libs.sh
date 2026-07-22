#!/usr/bin/env bash

# stage the CI-built native libraries into ./lib so `mvn package` bundles every
# platform. download the monero-java-native-libs artifact from the Actions Build
# run and pass its zip; this unpacks it into ./lib/<platform>/.
#
# usage: ./bin/stage_native_libs.sh <path-to-monero-java-native-libs.zip>

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

zip="${1:-}"
[ -f "$zip" ] || { echo "usage: $(basename "$0") <path-to-monero-java-native-libs.zip>" >&2; exit 1; }

mkdir -p "$ROOT/lib"
unzip -o -q "$zip" -d "$ROOT/lib"
ls "$ROOT/lib"/*/libmonero-java.* >/dev/null 2>&1 || { echo "no native libraries in $zip" >&2; exit 1; }
echo "staged native libraries into ./lib:"
(cd "$ROOT" && find lib -type f | sort)
