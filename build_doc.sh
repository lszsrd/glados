#!/usr/bin/env bash

#https://abhinavsarkar.net/notes/2023-standalone-haddock/

set -euo pipefail

if [ $# -ne 2 ]; then
    echo "Usage: $0 <title> <output>"
    exit 1
fi

TITLE=$1
OUTPUT=$2

OS=$(uname -s)

if [ "$OS" = "Darwin" ]
then
    OS="osx"
elif [ "$OS" = "Linux" ]
then
    OS="linux"
else
    printf "OS not supported: %s\n" "$OS" >&2
    exit 1
fi

ARCH=$(uname -m)
GHC_VERSION=$(ghc --version | awk '{print $8}')
PKG_NAME=$(cat ./*.cabal | grep name | head -1 | awk '{print $2}')
PKG_VERSION=$(cat ./*.cabal | grep version | grep -v cabal | awk '{print $2}' | head -1)
BUILD_PATH="dist-newstyle/build/${ARCH}-${OS}/ghc-${GHC_VERSION}/${PKG_NAME}-${PKG_VERSION}"
DOC_PATH="${BUILD_PATH}/l/*/doc/html/${PKG_NAME}"
declare -a MODULES=("backend-rizz" "format" "frontend-lisp" "frontend-rizz")

temp_files=()

cleanup() {
  for file in "${temp_files[@]}"; do
        rm "$file"
  done
}
trap cleanup EXIT INT TERM

bold_print() {
  echo -e "\033[1m$1\033[0m"
}

# build project
bold_print "Building project"
cabal build --enable-documentation

# generate docs
bold_print "Generating docs"
mkdir -p "${OUTPUT}"
cabal haddock --haddock-html \
    --haddock-quickjump \
    --haddock-hyperlink-source \
    --haddock-option="--use-index=../doc-index.html" \
    --haddock-option="--use-contents=../index.html" \
    --haddock-option="--base-url=.." \
    all
cp -f -r ${DOC_PATH}/ "${OUTPUT}"

# generate index
bold_print "Generating index"

CMD_FILE1=$(mktemp)
temp_files+=("$CMD_FILE1")

echo "set -euo pipefail" > "${CMD_FILE1}"
echo -n "haddock -t \"${TITLE}\" -o \"${OUTPUT}\" --quickjump --gen-index --gen-contents " >> "${CMD_FILE1}"
find ${DOC_PATH}/${PKG_NAME}.haddock -print0 | xargs -0 -I {} echo -n "--read-interface=${PKG_NAME},{} " >> "${CMD_FILE1}"
bash "${CMD_FILE1}"

# fix links in hyperlinked source and docs
GEN_SRC_PATH="${OUTPUT}/${PKG_NAME}/src/*.html"
GEN_DOC_PATH="${OUTPUT}/${PKG_NAME}/*.html"

bold_print "Renaming submodules"
# renaming modules
for i in "${MODULES[@]}"
do
    sed -i -e "s/${PKG_NAME}-${PKG_VERSION}/$i/" "${OUTPUT}/index.html"
done


bold_print "Fixing links to project subpackages"
# fix links to source files for project subpackages
perl -i -pe "s|file:[^\"]*?/${BUILD_PATH}/l/.*?/doc/html/${PKG_NAME}/src/||g" ${GEN_SRC_PATH}
# fix links to doc files for project subpackages
perl -i -pe "s|href=\"../${PKG_NAME}/|href=\"|g" ${GEN_DOC_PATH}

bold_print "Fixing links to project dependencies"
# fix links to doc files for project dependency libraries
perl -i -pe "s|href=\"../([^\"]+)/([^\"]+)|href=\"https://hackage.haskell.org/package/\$1/docs/\$2|g" ${GEN_DOC_PATH}

bold_print "Fixing links to libraries in Nix store"
# fix links to source files for GHC libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[\d\w]*-ghc-${GHC_VERSION}-doc/share/doc/ghc/html/libraries/([^\"]*)/src/([^\"]*)|https://hackage.haskell.org/package/\$1/docs/src/\$2|g" ${GEN_SRC_PATH}
# fix links to doc files for GHC libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[\d\w]*-ghc-${GHC_VERSION}-doc/share/doc/ghc/html/libraries/([^\"]*)/src|https://hackage.haskell.org/package/\$1/docs/src/|g" ${GEN_DOC_PATH}

# fix links to source files for Hackage libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[^\"]+-doc/share/doc/([^\"]+)/html/src/([^\"]+)|https://hackage.haskell.org/package/\$1/docs/src/\$2|g" ${GEN_SRC_PATH}
# fix links to doc files for Hackage libraries stored in the Nix store
perl -i -pe "s|file:///nix/store/[^\"]+-doc/share/doc/([^\"]+)/html/src|https://hackage.haskell.org/package/\$1/docs/src|g" ${GEN_DOC_PATH}

# generate Cabal package short ID to package name mapping
bold_print "Generating Cabal package short ID to package name mapping"
SHORT_IDS_FILE=$(mktemp)
temp_files+=("$SHORT_IDS_FILE")

grep -h -o -P "href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/.+?/" ${GEN_SRC_PATH} ${GEN_DOC_PATH} | \
  sort -u | grep -h -o -P "\.cabal/store/ghc-${GHC_VERSION}/.+?/" | cut -d '/' -f 4 > "${SHORT_IDS_FILE}"

PKG_NAMES_FILE=$(mktemp)
temp_files+=("$PKG_NAMES_FILE")

grep -h -o -P "href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/.+?/" ${GEN_SRC_PATH} ${GEN_DOC_PATH} | sort -u | \
  cut -c 14- | xargs -I {} cat {}cabal-hash.txt | grep "pkgid:" | cut -d ' ' -f 2 > "${PKG_NAMES_FILE}"

CMD_FILE2=$(mktemp)
temp_files+=("$CMD_FILE2")

echo "set -euo pipefail" > "${CMD_FILE2}"
paste -d " " "${SHORT_IDS_FILE}" "${PKG_NAMES_FILE}" | awk "{print \"perl -i -pe \\\"s|\"\$1\"|\"\$2\"|g\\\" ${GEN_SRC_PATH} ${GEN_DOC_PATH}\"}" >> "${CMD_FILE2}"
bash "${CMD_FILE2}"

# fix links to doc and source files of libraries stored in the Cabal store
bold_print "Fixing links to libraries in Cabal store"
perl -i -pe "s|href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/(.+?)/share/doc/html/([^\"]+)|href=\"https://hackage.haskell.org/package/\$1/docs/src/\$2|g" ${GEN_DOC_PATH}
perl -i -pe "s|href=\"file://[^\"]+/\.cabal/store/ghc-${GHC_VERSION}/(.+?)/share/doc/html/src|href=\"https://hackage.haskell.org/package/\$1/docs/src/|g" ${GEN_SRC_PATH}

rm "${OUTPUT}/${PKG_NAME}/${PKG_NAME}.haddock"

bold_print "Done"
