#!/usr/bin/env bash

set -eu

help() {
 echo -e "Runs stylish-haskell on every .hs file found within the current directory.
"
}

if [[ "${1-}" == -h* ]]; then
   help
   exit 0
fi

scriptdir=$(dirname $0)
shareddir="$scriptdir"/..

find . -name "*.hs" -and -not -path "*/.stack-work/*" -exec stylish-haskell -c "$shareddir"/.stylish-haskell.yaml -i {} \;
