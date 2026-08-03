#!/usr/bin/env bash
#
# Build camlcast-demo.app: a bundle that runs on a Mac with neither OCaml nor
# Homebrew on it.
#
# The OCaml runtime is already a non-issue — ocamlopt links it in. What has to
# come along is the C libraries, and three of them cannot be found by walking
# the executable's load commands, which is what makes this script rather than a
# call to dylibbundler:
#
#   * libSDL2_image, because tsdl-image dlopens it rather than linking it. It
#     appears in no otool output at all. LIBSDL2_IMAGE_SHLIB, set by the
#     launcher below, is the one hook that takes a full path and skips the
#     library's own search.
#   * its codec tail — png, jpeg, tiff, webp, avif, jxl and their own
#     dependencies — which hangs off libSDL2_image and so is invisible for the
#     same reason. Relocating that dylib pulls the whole tail in.
#   * libSDL3, because Homebrew's sdl2 is really sdl2-compat, an SDL2 ABI over
#     SDL3, and it dlopens SDL3 too. It has no LC_LOAD_DYLIB for it; the name it
#     looks for is @loader_path/libSDL3.dylib, so a copy beside the bundled
#     libSDL2 is all it needs and no rewriting is required.
#
# Everything else is the ordinary link graph, walked recursively below.
#
# Usage: tools/bundle-macos.sh [output directory]

set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-$root/dist}
prefix=$(brew --prefix)

# uname says x86_64; the runner labels, the job names and the Windows bundle all
# say x64. One spelling, so the release assets line up and the workflow can name
# the file it is about to upload.
arch=$(uname -m)
case $arch in x86_64) arch=x64 ;; esac

app=$out/camlcast-demo.app
macos=$app/Contents/MacOS
frameworks=$app/Contents/Frameworks
resources=$app/Contents/Resources

if [ ! -x "$root/_build/default/bin/demo.exe" ]; then
  echo "bundle-macos: no executable; run 'dune build' first" >&2
  exit 1
fi

rm -rf "$app"
mkdir -p "$macos" "$frameworks" "$resources"

# Contents/MacOS holds the real binary; Contents/Resources the art. Asset looks
# one directory up and across from the executable for a Resources directory, so
# the names here are the ones core/asset.ml already searches — and the assets
# subdirectory matters, because callers ask for "assets/tiles.png".
cp "$root/_build/default/bin/demo.exe" "$macos/camlcast-demo-bin"
chmod u+w "$macos/camlcast-demo-bin"
cp -R "$root/assets" "$resources/assets"

# Copy a dylib in under its own name, give it an identity relative to whoever
# loads it, and let it find its neighbours in the same directory.
adopt() {
  local src=$1 base
  base=$(basename "$src")
  [ -e "$frameworks/$base" ] && return 0
  cp -L "$src" "$frameworks/$base"
  chmod u+w "$frameworks/$base"
  install_name_tool -id "@rpath/$base" "$frameworks/$base"
  install_name_tool -add_rpath "@loader_path" "$frameworks/$base" 2>/dev/null || true
}

# Walk what a Mach-O file loads, bring each non-system library into Frameworks,
# and point the reference at it. Recurses, so a library's own dependencies come
# too. System libraries are left alone: they are on every Mac by definition.
#
# An already-relative reference is not already handled. Homebrew's libwebp asks
# for @rpath/libsharpyuv.0.dylib and finds it through an LC_RPATH of its own
# that points back into the Cellar — so the name needs no rewriting but the file
# still has to be fetched, and skipping those is a bundle that builds cleanly
# and dies on the first picture. Every Homebrew library lives in one directory,
# which is where such a reference resolves.
relocate() {
  local file=$1 dep base src self candidate
  self=$(basename "$file")
  while read -r dep; do
    case $dep in
      /usr/lib/* | /System/*) continue ;;
    esac
    base=$(basename "$dep")
    # otool -L opens with the file's own identity, which is not a dependency —
    # and once adopt has rewritten that to @rpath/<self>, taking it for one
    # sends the walk looking for a library to put inside itself.
    [ "$base" = "$self" ] && continue
    case $dep in
      @*)
        # Keg-only formulas — libffi is one — are never symlinked into
        # $prefix/lib, so the Cellar's own opt/ directory is the second place
        # to look.
        src=
        for candidate in "$prefix/lib/$base" "$prefix"/opt/*/lib/"$base"; do
          if [ -e "$candidate" ]; then
            src=$candidate
            break
          fi
        done
        if [ -z "$src" ]; then
          echo "bundle-macos: cannot resolve $dep, wanted by $file" >&2
          exit 1
        fi
        ;;
      *) src=$dep ;;
    esac
    if [ ! -e "$frameworks/$base" ]; then
      adopt "$src"
      relocate "$frameworks/$base"
    fi
    [ "$dep" = "@rpath/$base" ] ||
      install_name_tool -change "$dep" "@rpath/$base" "$file"
  done < <(otool -L "$file" | tail -n +2 | awk '{print $1}')
}

install_name_tool -add_rpath "@executable_path/../Frameworks" "$macos/camlcast-demo-bin"
relocate "$macos/camlcast-demo-bin"

# The two that no amount of otool would have found.
adopt "$prefix/lib/libSDL2_image.dylib"
adopt "$prefix/lib/libSDL3.dylib"
relocate "$frameworks/libSDL2_image.dylib"
relocate "$frameworks/libSDL3.dylib"

# Finder gives a launched app no terminal, so a double-click arrives here with
# no arguments — which is exactly what opens the menu. Arguments are still
# passed through for anyone starting it from a shell.
cat >"$macos/camlcast-demo" <<'EOF'
#!/bin/sh
here=$(cd "$(dirname "$0")" && pwd)
LIBSDL2_IMAGE_SHLIB="$here/../Frameworks/libSDL2_image.dylib"
export LIBSDL2_IMAGE_SHLIB
exec "$here/camlcast-demo-bin" "$@"
EOF
chmod +x "$macos/camlcast-demo"

# Every Mach-O file records the OS version it was built against, and the bundle
# cannot run below the highest of them. That number is not ours to choose: the
# libraries came from Homebrew, which builds its bottles for the runner's own
# macOS, and MACOSX_DEPLOYMENT_TARGET would reach our object files and none of
# theirs. So the plist is told what is true of the bundle rather than what would
# be nice -- read back out of the bundle itself, after everything is in it.
floor=$(
  otool -l "$macos/camlcast-demo-bin" "$frameworks"/*.dylib |
    awk '$1 == "minos" { print $2 }' |
    sort -t. -k1,1n -k2,2n |
    tail -1
)
case $floor in
  [0-9]*.[0-9]*) ;;
  *)
    echo "bundle-macos: no minos in the bundle; cannot say what it runs on" >&2
    exit 1
    ;;
esac

cat >"$app/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key><string>camlcast-demo</string>
  <key>CFBundleIdentifier</key><string>io.github.pharick.camlcast-demo</string>
  <key>CFBundleExecutable</key><string>camlcast-demo</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleInfoDictionaryVersion</key><string>6.0</string>
  <key>NSHighResolutionCapable</key><true/>
  <key>LSMinimumSystemVersion</key><string>$floor</string>
</dict>
</plist>
EOF

# Not optional, and not about distribution: an arm64 Mac refuses to execute a
# binary with no signature at all, and every install_name_tool call above
# invalidated the one it had. Ad-hoc is enough to run. It is not enough to
# satisfy Gatekeeper on a machine that downloaded the zip — see README.
find "$frameworks" -name '*.dylib' -exec codesign --force --sign - {} +
codesign --force --sign - "$macos/camlcast-demo-bin"
codesign --force --sign - "$app"
codesign --verify --deep --strict "$app"

# The bundle is only relocatable if nothing still points into Homebrew, and only
# complete if the library nobody could see is in it.
if otool -L "$macos/camlcast-demo-bin" "$frameworks"/*.dylib | grep -q "$prefix"; then
  echo "bundle-macos: a $prefix path survived relocation" >&2
  otool -L "$macos/camlcast-demo-bin" "$frameworks"/*.dylib | grep "$prefix" >&2
  exit 1
fi
for needed in libSDL2_image.dylib libSDL3.dylib; do
  if [ ! -e "$frameworks/$needed" ]; then
    echo "bundle-macos: $needed is missing from the bundle" >&2
    exit 1
  fi
done

# ditto rather than zip, to keep the signature and the symlinks intact.
ditto -c -k --keepParent "$app" "$out/camlcast-demo-macos-$arch.zip"

echo "bundle-macos: $app"
echo "bundle-macos: $(find "$frameworks" -name '*.dylib' | wc -l | tr -d ' ') libraries bundled"
echo "bundle-macos: runs on macOS $floor and later"
echo "bundle-macos: $out/camlcast-demo-macos-$arch.zip"
