#!/usr/bin/env bash
#
# Build a camlcast-demo folder that runs on a Windows machine with no MSYS2 on
# it. Run from the MSYS2 mingw64 shell, which is where ldd can read a PE file.
#
# Windows is the easy one of the three. Its loader searches the directory the
# executable lives in before anything else, so a DLL beside the binary is found
# with no environment variable and no rewriting of anything — which covers both
# the link closure and the SDL2_image that tsdl-image dlopens by bare name. The
# assets sit beside the binary too, which is the second of the roots
# lib/asset.ml searches.
#
# Only DLLs from /mingw64 are copied. The rest of what ldd reports lives in
# C:\Windows\System32 and is part of the operating system.
#
# Usage: tools/bundle-windows.sh [output directory]

set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-$root/dist}
stage=$out/camlcast-demo

if [ ! -e "$root/_build/default/bin/demo.exe" ]; then
  echo "bundle-windows: no executable; run 'dune build' first" >&2
  exit 1
fi

rm -rf "$stage"
mkdir -p "$stage"

cp "$root/_build/default/bin/demo.exe" "$stage/camlcast-demo.exe"
cp -R "$root/assets" "$stage/assets"

# ldd on a PE file resolves the whole transitive closure, so this needs no
# recursion — only a second pass for the DLL that is dlopened and therefore in
# nobody's closure.
harvest() {
  local from=$1 name path
  while read -r name _ path _; do
    [ -n "${path:-}" ] || continue
    case $path in /mingw64/*) ;; *) continue ;; esac
    [ -e "$stage/$name" ] && continue
    cp "$path" "$stage/$name"
  done < <(ldd "$from" | tr -d '\t' | grep ' => ')
}

harvest "$stage/camlcast-demo.exe"

if [ ! -e /mingw64/bin/SDL2_image.dll ]; then
  echo "bundle-windows: /mingw64/bin/SDL2_image.dll not found" >&2
  exit 1
fi
cp /mingw64/bin/SDL2_image.dll "$stage/SDL2_image.dll"
harvest "$stage/SDL2_image.dll"

# No launcher script: the executable with no argument opens the menu, so
# double-clicking camlcast-demo.exe is already the right thing. A console window
# opens behind it, which is what an OCaml executable on mingw is — the alternative
# is linking -mwindows, and see the CI workflow for how well that goes with tsdl.

for needed in SDL2.dll SDL2_image.dll; do
  if [ ! -e "$stage/$needed" ]; then
    echo "bundle-windows: $needed is missing from the bundle" >&2
    ls "$stage" >&2
    exit 1
  fi
done

echo "bundle-windows: $stage"
echo "bundle-windows: $(find "$stage" -maxdepth 1 -name '*.dll' | wc -l | tr -d ' ') DLLs bundled"
