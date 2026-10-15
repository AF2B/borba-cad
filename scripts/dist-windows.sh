#!/usr/bin/env bash
# Builds the Windows distribution: a folder that runs from a double click, and its .zip.
#
# Run it in an MSYS2 MINGW64 shell, after `make exe` has built build/borba-cad.exe there (see
# docs/development.md, "Windows"). Nothing in this script has been run on Windows yet: it was
# written from the Linux one, and the first run on a Windows machine will probably need a fix.
#
# Usage: scripts/dist-windows.sh EXECUTABLE OUTPUT-DIRECTORY VERSION
#
# The folder gets two copies of the program: borba-cad.exe, which opens no console window, and
# borba-cad-console.exe, which does and shows what the program prints (use it to find out why the
# first one does not start). It also gets the DLLs that the program loads while it runs (SDL2 and
# FreeType, with whatever they need), the fonts, and the examples. DLLs that belong to Windows are
# never copied.
#
# Environment: DLL_DIRECTORY (default /mingw64/bin) says where SDL2.dll and libfreetype-6.dll are;
# FONT_DIRECTORY, when set, holds NotoSans-Regular.ttf, NotoSans-Medium.ttf and
# NotoSansMono-Regular.ttf, which are then bundled (with FONT_LICENSE, the fonts' license file);
# without it the program uses the fonts that Windows has (Segoe UI and Consolas).

set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "usage: $0 EXECUTABLE OUTPUT-DIRECTORY VERSION" >&2
  exit 2
fi

executable=$1
output=$2
version=$3
root=$(cd "$(dirname "$0")/.." && pwd)
name="borba-cad-$version-windows-x86_64"
bundle="$output/$name"
dll_directory=${DLL_DIRECTORY:-/mingw64/bin}

# DLLs that every Windows has: whatever lives under the Windows folder.
system_directory_pattern='^/(c|cygdrive/c)/[Ww][Ii][Nn][Dd][Oo][Ww][Ss]/'

# DLLs that the program opens by name while it runs, so that ldd cannot see them.
opened_by_name=(SDL2.dll libfreetype-6.dll)

die() {
  echo "dist: $*" >&2
  exit 1
}

# set_gui_subsystem FILE: marks a Windows program as a "windows" (graphical) program instead of a
# "console" one, by writing 2 over the Subsystem field of its PE header, so that double-clicking it
# opens no console window. The field sits 68 bytes into the optional header, which begins 24 bytes
# after the "PE" signature; the signature's offset is the 4-byte value at offset 60.
set_gui_subsystem() {
  local file=$1 pe_offset
  [ "$(head -c 2 "$file")" = "MZ" ] || die "$file is not a Windows program"
  pe_offset=$(od -An -tu4 -j60 -N4 "$file" | tr -d ' ')
  [ "$(od -An -c -j"$pe_offset" -N2 "$file" | tr -d ' ')" = "PE" ] || die "$file has no PE header"
  printf '\002\000' | dd of="$file" bs=1 seek=$((pe_offset + 24 + 68)) conv=notrunc 2>/dev/null
}

# When the script is only being read for its functions (the tests do that), stop here.
if [ "${BORBA_CAD_SOURCE_ONLY:-}" = "1" ]; then
  return 0 2>/dev/null || exit 0
fi

[ -f "$executable" ] || die "$executable does not exist; run make exe first"
command -v ldd >/dev/null || die "ldd is needed (it comes with MSYS2)"

rm -rf "$bundle" "$bundle.zip"
mkdir -p "$bundle/fonts" "$bundle/LICENSES" "$bundle/examples"

cp "$executable" "$bundle/borba-cad-console.exe"
cp "$executable" "$bundle/borba-cad.exe"
set_gui_subsystem "$bundle/borba-cad.exe"
cp "$root/packaging/windows/README.txt" "$bundle/"
cp "$root"/examples/*.bcad "$root"/examples/*.lisp "$bundle/examples/"

# DLLs: the ones opened by name, and everything they and the program need, recursively.
declare -A copied=()
queue=("$bundle/borba-cad-console.exe")

for library in "${opened_by_name[@]}"; do
  [ -f "$dll_directory/$library" ] || die "$dll_directory/$library is missing; install SDL2 and FreeType (see docs)"
  cp "$dll_directory/$library" "$bundle/$library"
  copied[$library]=1
  queue+=("$bundle/$library")
done

while [ "${#queue[@]}" -gt 0 ]; do
  file=${queue[0]}
  queue=("${queue[@]:1}")
  while read -r dll arrow path _; do
    [ "$arrow" = "=>" ] || continue
    [[ "$path" =~ $system_directory_pattern ]] && continue
    [[ "$dll" == msys-* ]] && continue
    [ -n "${copied[$dll]:-}" ] && continue
    [ -f "$path" ] || die "$file needs $dll, which was not found"
    cp "$path" "$bundle/$dll"
    copied[$dll]=1
    queue+=("$bundle/$dll")
  done < <(ldd "$file")
done

# Fonts, when the build machine has the Noto ones.
if [ -n "${FONT_DIRECTORY:-}" ]; then
  for file in NotoSans-Regular.ttf NotoSans-Medium.ttf NotoSansMono-Regular.ttf; do
    [ -f "$FONT_DIRECTORY/$file" ] || die "$FONT_DIRECTORY/$file is missing"
    cp "$FONT_DIRECTORY/$file" "$bundle/fonts/"
  done
  [ -n "${FONT_LICENSE:-}" ] && [ -f "$FONT_LICENSE" ] && cp "$FONT_LICENSE" "$bundle/LICENSES/Noto-fonts.txt"
fi

cat > "$bundle/LICENSES/THIRD-PARTY.txt" <<'LICENSES'
Borba CAD ships these components. Each keeps its own license; the full texts are with the
projects named here.

  SBCL (the Lisp runtime and system inside the .exe files)   public domain and BSD-style licenses
  Common Lisp libraries inside the .exe files                alexandria, cffi, cl-opengl, cl-sdl2,
                                                             cl-freetype2; MIT, BSD and
                                                             public-domain licenses
  SDL2 (SDL2.dll)                                            zlib license
  FreeType (libfreetype-6.dll)                               FreeType License or GPL version 2
  Other DLLs of this folder (zlib, libpng, bzip2, Brotli)    their own permissive licenses
  Noto Sans and Noto Sans Mono (fonts/, when present)        SIL Open Font License 1.1
LICENSES

{
  echo "Borba CAD $version"
  echo "Built:    $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "Machine:  $(uname -srm)"
  if git -C "$root" rev-parse --short HEAD >/dev/null 2>&1; then
    echo "Commit:   $(git -C "$root" rev-parse --short HEAD)"
  fi
  echo "Bundled DLLs:"
  (cd "$bundle" && ls -1 ./*.dll | sed 's|^\./|  |')
} > "$bundle/BUILD-INFO.txt"

if command -v zip >/dev/null 2>&1; then
  (cd "$output" && zip -qr "$name.zip" "$name")
  echo "Archive: $bundle.zip"
else
  echo "zip is not installed, so only the folder was made (pacman -S zip in MSYS2)"
fi
echo "Folder:  $bundle"
echo "DLLs bundled: $(cd "$bundle" && ls ./*.dll | sed 's|^\./||' | tr '\n' ' ')"
