#!/usr/bin/env bash

set -eu

help() {
  echo -e 'Uses stylish-haskell to find unstylish .hs files in current directory.

If any file fails to parse or is not stylish, the compilation will fail. For
this reason it is helpful to use `check-stylish` as a git pre-commit hook, so
that git forbids you from committing unstylish files. Here is an example on how
to do it:

    echo "#! /usr/bin/env bash
   ./scripts/check-stylish" > .git/hooks/pre-commit
    chmod +x .git/hooks/pre-commit
'
}

if [[ "${1-}" == -h* ]]; then
  help
  exit 0
fi

PATCH_FILE=stylish.patch

echo "Checking style of haskell files"
find . -name "*.hs" -and -not -path "*/.stack-work/*" -and -not -path "*/.s2n/*" -exec bash -c 'mydiff=$(diff {} <(stylish-haskell {})); if test -n "$mydiff"; then echo "Not stylish: {}" ; fi;' \; | tee $PATCH_FILE

if [[ -s $PATCH_FILE ]]; then
  exit 1
fi

echo "All files are stylish."