#!/usr/bin/env bash
#
# Build a camlcast-demo tarball that runs on a Linux box without OCaml or the
# SDL2 development packages.
#
# Same three-part problem as the macOS bundle, solved with the tools this
# platform has: the link closure comes from ldd, libSDL2_image is dlopened by
# tsdl-image and so appears in no closure at all, and its codec tail hangs off
# it. There is no sdl2-compat here — Linux has real SDL2 — so unlike macOS
# there is no hidden SDL3.
#
# The libraries are found through LD_LIBRARY_PATH set by the launcher rather
# than through a patched RPATH, which keeps patchelf out of the build.
#
# Deliberately NOT bundled: glibc and the dynamic loader. A binary must use the
# loader it was built against, and shipping a glibc that disagrees with the
# host's is the classic way to make a tarball that segfaults on someone else's
# machine. The X11, Wayland and GL stacks are absent for a different reason —
# SDL2 dlopens them at runtime, so they never appear in ldd, and they are the
# one thing a machine running a graphical session certainly has.
#
# Usage: tools/bundle-linux.sh [output directory]

set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-$root/dist}
arch=$(uname -m)

stage=$out/camlcast-demo
libs=$stage/lib

if [ ! -x "$root/_build/default/bin/demo.exe" ]; then
  echo "bundle-linux: no executable; run 'dune build' first" >&2
  exit 1
fi

rm -rf "$stage"
mkdir -p "$libs"

# Flat, so that the directory beside the binary is the one holding the art —
# the second of the roots lib/asset.ml searches.
cp "$root/_build/default/bin/demo.exe" "$stage/camlcast-demo-bin"
chmod u+w "$stage/camlcast-demo-bin"
cp -R "$root/assets" "$stage/assets"

# Anything the host is guaranteed to have, or must provide itself.
keep_out() {
  case $1 in
    ld-linux*| libc.so.* | libm.so.* | libdl.so.* | librt.so.* | \
      libpthread.so.* | libresolv.so.* | libgcc_s.so.* | \
      linux-vdso.so.*) return 0 ;;
  esac
  return 1
}

# ldd resolves the whole transitive closure in one go, so unlike the macOS
# walk this needs no recursion — only a second pass for the library that is
# dlopened and therefore in nobody's closure.
harvest() {
  local from=$1 name path
  while read -r name _ path _; do
    [ -n "${path:-}" ] || continue
    [ -e "$path" ] || continue
    keep_out "$name" && continue
    [ -e "$libs/$name" ] && continue
    cp -L "$path" "$libs/$name"
    chmod u+w "$libs/$name"
  done < <(ldd "$from" | tr -d '\t' | grep ' => ')
}

harvest "$stage/camlcast-demo-bin"

image=$(ldconfig -p | awk '/libSDL2_image-2\.0\.so\.0/ {print $NF; exit}')
if [ -z "$image" ] || [ ! -e "$image" ]; then
  echo "bundle-linux: libSDL2_image-2.0.so.0 not found by ldconfig" >&2
  exit 1
fi
cp -L "$image" "$libs/libSDL2_image-2.0.so.0"
chmod u+w "$libs/libSDL2_image-2.0.so.0"
harvest "$libs/libSDL2_image-2.0.so.0"

# LIBSDL2_IMAGE_SHLIB takes a full path and skips tsdl-image's own search, so
# the bundled copy is found without depending on what the host has installed.
cat >"$stage/camlcast-demo" <<'EOF'
#!/bin/sh
here=$(cd "$(dirname "$0")" && pwd)
LD_LIBRARY_PATH="$here/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
LIBSDL2_IMAGE_SHLIB="$here/lib/libSDL2_image-2.0.so.0"
export LD_LIBRARY_PATH LIBSDL2_IMAGE_SHLIB
[ $# -eq 0 ] && set -- showcase
exec "$here/camlcast-demo-bin" "$@"
EOF
chmod +x "$stage/camlcast-demo"

for needed in libSDL2-2.0.so.0 libSDL2_image-2.0.so.0; do
  if [ ! -e "$libs/$needed" ]; then
    echo "bundle-linux: $needed is missing from the bundle" >&2
    ls "$libs" >&2
    exit 1
  fi
done

tar -C "$out" -czf "$out/camlcast-demo-linux-$arch.tar.gz" camlcast-demo

echo "bundle-linux: $stage"
echo "bundle-linux: $(find "$libs" -name '*.so*' | wc -l | tr -d ' ') libraries bundled"
echo "bundle-linux: $out/camlcast-demo-linux-$arch.tar.gz"
