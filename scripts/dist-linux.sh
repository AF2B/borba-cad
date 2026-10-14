#!/usr/bin/env bash
# Builds the Linux distribution: a folder that runs from a double click, and its .tar.gz.
#
# Usage: scripts/dist-linux.sh EXECUTABLE OUTPUT-DIRECTORY VERSION
#
# EXECUTABLE is the standalone program made by `make exe`. The folder gets the launcher, the
# program, the libraries the program loads that a desktop may lack (SDL2 with its SDL3 backend,
# FreeType and what FreeType needs), the fonts, the examples, the icon and the install scripts.
# Libraries that belong to the system or to the graphics driver (libc, OpenGL, X11, Wayland, ...)
# are never copied.
#
# Environment: FONT_DIR (default /usr/share/fonts/noto) and FONT_LICENSE (default
# /usr/share/licenses/noto-fonts/LICENSE) say where the Noto fonts and their license are.

set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "usage: $0 EXECUTABLE OUTPUT-DIRECTORY VERSION" >&2
  exit 2
fi

executable=$1
output=$2
version=$3
root=$(cd "$(dirname "$0")/.." && pwd)
name="borba-cad-$version-linux-x86_64"
bundle="$output/$name"
font_dir=${FONT_DIR:-/usr/share/fonts/noto}
font_license=${FONT_LICENSE:-/usr/share/licenses/noto-fonts/LICENSE}

# Names of libraries that are never bundled: the C library and the parts of the system that the
# graphics stack must provide itself.
system_libraries='^(/|linux-vdso|ld-linux|libc\.|libm\.|libdl\.|libpthread\.|librt\.|libresolv\.|libutil\.|libGL|libEGL|libOpenGL|libX|libxcb|libwayland|libdrm|libgbm|libstdc\+\+|libgcc_s)'

# Libraries that the program opens by name while it runs, so that ldd cannot see them.
opened_by_name=(libSDL2-2.0.so.0 libSDL3.so.0 libfreetype.so.6)

die() {
  echo "dist: $*" >&2
  exit 1
}

[ -x "$executable" ] || die "$executable does not exist; run make exe first"
command -v ldd >/dev/null || die "ldd is needed"
command -v ldconfig >/dev/null || die "ldconfig is needed"

rm -rf "$bundle" "$bundle.tar.gz"
mkdir -p "$bundle/lib" "$bundle/fonts" "$bundle/LICENSES" "$bundle/examples"

cp "$executable" "$bundle/borba-cad.bin"
cp "$root/packaging/linux/borba-cad" "$root/packaging/linux/install.sh" "$root/packaging/linux/uninstall.sh" \
   "$root/packaging/linux/README.txt" "$bundle/"
cp "$root/packaging/linux/Borba CAD.desktop" "$bundle/"
cp "$root/packaging/borba-cad.svg" "$bundle/"
cp "$root"/examples/*.bcad "$bundle/examples/"
chmod +x "$bundle/borba-cad" "$bundle/borba-cad.bin" "$bundle/install.sh" "$bundle/uninstall.sh" \
         "$bundle/Borba CAD.desktop"

# Fonts and their license.
for file in NotoSans-Regular.ttf NotoSans-Medium.ttf NotoSansMono-Regular.ttf; do
  [ -f "$font_dir/$file" ] || die "$font_dir/$file is missing; install the Noto fonts or set FONT_DIR"
  cp "$font_dir/$file" "$bundle/fonts/"
done
[ -f "$font_license" ] || die "$font_license is missing; set FONT_LICENSE to the Noto fonts' license"
cp "$font_license" "$bundle/LICENSES/Noto-fonts.txt"

# Libraries: the ones opened by name, and everything they and the program need, recursively.
declare -A copied=()
queue=("$bundle/borba-cad.bin")

cache=$(ldconfig -p)
for library in "${opened_by_name[@]}"; do
  path=$(awk -v name="$library" '$1 == name {print $NF; exit}' <<<"$cache")
  [ -n "$path" ] || die "$library is not installed on this machine"
  cp -L "$path" "$bundle/lib/$library"
  copied[$library]=1
  queue+=("$bundle/lib/$library")
done

while [ "${#queue[@]}" -gt 0 ]; do
  file=${queue[0]}
  queue=("${queue[@]:1}")
  while read -r soname arrow path _; do
    [ "$arrow" = "=>" ] || continue
    [[ "$soname" =~ $system_libraries ]] && continue
    [ -n "${copied[$soname]:-}" ] && continue
    [ -f "$path" ] || die "$file needs $soname, which was not found"
    cp -L "$path" "$bundle/lib/$soname"
    copied[$soname]=1
    queue+=("$bundle/lib/$soname")
  done < <(ldd "$file")
done

cat > "$bundle/LICENSES/THIRD-PARTY.txt" <<'LICENSES'
Borba CAD ships these components. Each keeps its own license; the full texts are with the
projects named here.

  SBCL (the Lisp runtime and system inside borba-cad.bin)   public domain and BSD-style licenses
  Common Lisp libraries inside borba-cad.bin                alexandria, cffi, cl-opengl, cl-sdl2,
                                                            cl-freetype2, FiveAM is not included;
                                                            MIT, BSD and public-domain licenses
  SDL2 compatibility layer and SDL3 (lib/libSDL2*, libSDL3*)   zlib license
  FreeType (lib/libfreetype*)                               FreeType License or GPL version 2
  libpng, zlib, bzip2, Brotli, Zstandard (lib/)             libpng license, zlib license,
                                                            bzip2 license, MIT, BSD-3-Clause
  Noto Sans and Noto Sans Mono (fonts/)                     SIL Open Font License 1.1
                                                            (Noto-fonts.txt)
LICENSES

{
  echo "Borba CAD $version"
  echo "Built:    $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "Machine:  $(uname -srm)"
  echo "glibc:    $(ldd --version 2>&1 | sed -n 1p)"
  echo "SBCL:     $(sbcl --version 2>/dev/null || echo unknown)"
  if git -C "$root" rev-parse --short HEAD >/dev/null 2>&1; then
    echo "Commit:   $(git -C "$root" rev-parse --short HEAD)"
  fi
  echo "Bundled libraries:"
  (cd "$bundle/lib" && ls -1 | sed 's/^/  /')
} > "$bundle/BUILD-INFO.txt"

tar --owner=0 --group=0 -C "$output" -czf "$bundle.tar.gz" "$name"

echo "Folder:  $bundle"
echo "Archive: $bundle.tar.gz ($(du -h "$bundle.tar.gz" | cut -f1))"
echo "Libraries bundled: $(ls "$bundle/lib" | tr '\n' ' ')"
