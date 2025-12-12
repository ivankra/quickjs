#!/bin/bash
# This script is run after a successful build to strip and copy built binaries
# out of build containers to ../dist/<arch>/, along with a metadata .json file.
#
# SPDX-FileCopyrightText: 2025 Ivan Krasilnikov
# SPDX-License-Identifier: MIT

set -e

ID="$1"
if [[ -z "$ID" ]]; then
  echo "Usage: ./dist.sh <name>[.Dockerfile]"
  exit 1
fi

ID=${ID%.Dockerfile}

ARCH=$(uname -m | sed -e 's/aarch64/arm64/; s/x86_64/amd64/')
DOCKER=$(command -v podman 2>/dev/null || echo docker)

mkdir -p "../dist/$ARCH"
rm -rf "../dist/$ARCH/$ID" >/dev/null 2>&1
rm -rf "../dist/$ARCH/$ID".* >/dev/null 2>&1

CIDFILE="../.cache/cid/$ID"
mkdir -p "../.cache/cid"

if [[ -f $CIDFILE ]]; then
  $DOCKER rm "$(cat "$CIDFILE")" >/dev/null 2>&1 || true
  rm -f "$CIDFILE"
fi

# Prepare /dist directory inside container
$DOCKER run --cidfile="$CIDFILE" "jsz-$ID" /bin/bash -e -c "$(cat <<EOF
mkdir -p /dist
strip -o /dist/$ID /src/qjs
cp /src/quickjs.S /dist/$ID.S

sha256sum /dist/$ID | cut -f 1 -d ' ' >/dist/jsz_binary_sha256
ls -l /dist/$ID 2>/dev/null | sed -e 's/  */ /g' | cut -f 5 -d ' ' >/dist/jsz_binary_size
git rev-parse --short=8 HEAD >/dist/jsz_version
git rev-parse HEAD >/dist/jsz_revision
git log -1 --format='%ad' --date=short HEAD >/dist/jsz_revision_date
git remote get-url origin >/dist/jsz_repository

cd /dist

ID="$ID"
export DIST_BIN="/dist/$ID"

ENGINE="\${ID%_*}"
ENGINE="\${ENGINE%_*}"
ENGINE="\${ENGINE%_*}"
echo "\$ENGINE" >jsz_engine

if [[ "\$ID" == *_* ]]; then
  VARIANT="\${ID#*_}"
  echo "\$VARIANT" >jsz_variant
fi

echo "$ARCH" >jsz_arch

# Assemble $DIST_BIN.json from value fragments in /dist/jsz_*
{
  echo "{"
  for f in jsz_*; do
    key=\${f#jsz_}
    if [[ \$key == binary_size || \$key == dist_size || \$key == loc ]]; then
      val=\$(cat "\$f")
    else
      val=\$(cat "\$f" | python3 -c 'import sys, json; print(json.dumps(sys.stdin.read().strip()));')
    fi
    echo "  \"\$key\": \$val,"
    rm -f "\$f"
  done
} | sed '$ s/,$//' >\$DIST_BIN.json
echo "}" >>\$DIST_BIN.json

ls -l /dist | grep -v ^total

exit 0;
EOF
)"

# Copy dist directory's contents to ../dist/<arch>/

CID=$(cat "$CIDFILE")
TMPCP="../.cache/dist-tmp-$ID"
rm -rf "$TMPCP"
mkdir -p "$TMPCP"
$DOCKER cp "$CID":/dist "$TMPCP"

rm -rf \
  "../dist/$ARCH/$ID" \
  "../dist/$ARCH/$ID."* \
  "../dist/$ARCH/$ID-dist" \
  "../dist/$ARCH/$ID-lib"

for subdir in "" parsers/ transpilers/; do
  for src in "$TMPCP/dist/$subdir$ID"*; do
    if [[ -e "$src" ]]; then
      mkdir -p "../dist/$ARCH/$subdir"
      dst="../dist/$ARCH/$subdir$(basename "$src")"
      if [[ -e "$dst" ]]; then
        chmod a+w "$dst"
        rm -rf "$dst"
      fi
      mv -f "$src" "$dst"
      if [[ -f "$dst" ]]; then
        chmod a-w "$dst"
      fi
    fi
  done
done

$DOCKER rm "$CID" >/dev/null
rm -rf "$TMPCP" "$CIDFILE"

exit 0
