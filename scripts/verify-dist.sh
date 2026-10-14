#!/usr/bin/env bash
# Starts the Linux distribution folder the way a double click would, in a clean environment, and
# checks three things: the scripted demonstration draws and saves a valid PNG; a running program
# has loaded its libraries from the folder and not from the system; and it uses the fonts of the
# folder. Needs a graphical session.
#
# Usage: scripts/verify-dist.sh BUNDLE-DIRECTORY

set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 BUNDLE-DIRECTORY" >&2
  exit 2
fi

bundle=$(cd "$1" && pwd -P)
work=$(mktemp -d)
pid=""
program=""
cleanup() {
  # The program and its launcher are throwaways: stop them at once, whatever state they are in.
  [ -z "$program" ] || kill -KILL "$program" 2>/dev/null || true
  [ -z "$pid" ] || kill -KILL "$pid" 2>/dev/null || true
  rm -rf "$work"
}
trap cleanup EXIT

fail() {
  echo "verify-dist: FAILED: $*" >&2
  exit 1
}

if [ -z "${WAYLAND_DISPLAY:-}${DISPLAY:-}" ]; then
  fail "there is no graphical session (WAYLAND_DISPLAY and DISPLAY are empty)"
fi

mkdir -p "$work/home"
clean=(env -i
       HOME="$work/home"
       PATH=/usr/bin:/bin
       XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-}"
       WAYLAND_DISPLAY="${WAYLAND_DISPLAY:-}"
       DISPLAY="${DISPLAY:-}")

# 1. The scripted demonstration runs, draws and saves a PNG.
"${clean[@]}" "$bundle/borba-cad" --demo --exit-after-demo --screenshot "$work/demo.png" \
  > "$work/demo.out" 2>&1 || { cat "$work/demo.out" >&2; fail "the demonstration did not run"; }
[ -s "$work/demo.png" ] || fail "the demonstration saved no screenshot"
signature=$(head -c 8 "$work/demo.png" | od -An -tx1 | tr -d ' \n')
[ "$signature" = "89504e470d0a1a0a" ] || fail "the screenshot is not a PNG"
size=$(stat -c %s "$work/demo.png")
[ "$size" -gt 20000 ] || fail "the screenshot is only $size bytes; it should show a whole window"
echo "demonstration: ok, screenshot of $size bytes"

# 2. The libraries of the folder do not lean on system copies of anything but the C library and
# the graphics stack: every other dependency of the program and of each library resolves inside it.
system_libraries='^(/|linux-vdso|ld-linux|libc\.|libm\.|libdl\.|libpthread\.|librt\.|libresolv\.|libutil\.|libGL|libEGL|libOpenGL|libX|libxcb|libwayland|libdrm|libgbm|libstdc\+\+|libgcc_s)'
for file in "$bundle/borba-cad.bin" "$bundle"/lib/*; do
  while read -r soname arrow path _; do
    [ "$arrow" = "=>" ] || continue
    [[ "$soname" =~ $system_libraries ]] && continue
    case "$path" in
      "$bundle"/lib/*) ;;
      *) fail "$(basename "$file") depends on $soname, which resolves to $path and not to the folder" ;;
    esac
  done < <(LD_LIBRARY_PATH="$bundle/lib" ldd "$file")
done
echo "dependencies: every library the folder needs resolves inside it"

# 3. A running program loads the SDL libraries from the folder. FreeType may already be loaded by
# the desktop's own libraries (client-side window decorations use it), and the system copy then
# serves; that is reported, not refused, because the folder's copy is for systems that lack one.
"${clean[@]}" "$bundle/borba-cad" > "$work/run.out" 2>&1 &
pid=$!
mapped=""
for _ in $(seq 1 40); do
  sleep 0.5
  kill -0 "$pid" 2>/dev/null || { cat "$work/run.out" >&2; fail "the program exited by itself"; }
  # The launcher stays alive to report a failed start; the program is its child.
  program=$(pgrep -P "$pid" -f borba-cad.bin | sed -n 1p || true)
  [ -n "$program" ] || continue
  mapped=$(grep -E 'libSDL|libfreetype' "/proc/$program/maps" 2>/dev/null | awk '{print $6}' | sort -u || true)
  if grep -q libSDL3 <<<"$mapped" && grep -q libfreetype <<<"$mapped"; then
    break
  fi
done
[ -n "$mapped" ] || fail "no SDL or FreeType library is loaded after 20 seconds"
while read -r library; do
  case "$library" in
    "$bundle"/lib/*)
      echo "  from the folder: $library" ;;
    */libSDL*)
      fail "$library was loaded from outside the folder" ;;
    *)
      echo "  from the system: $library (already loaded by the desktop's libraries)" ;;
  esac
done <<<"$mapped"

# 4. The program reports the fonts it opened; they must be the folder's.
log="$work/home/.local/state/borba-cad/log/borba-cad.log"
for _ in $(seq 1 20); do
  [ -f "$log" ] && grep -q 'app.start' "$log" && break
  sleep 0.5
done
[ -f "$log" ] || fail "the program wrote no log in $work/home/.local/state/borba-cad/log"
start=$(grep 'app.start' "$log" | head -n 1)
for field in font_mono font_ui font_ui_strong; do
  value=$(grep -o "$field=[^ ]*" <<<"$start" | head -n 1 | cut -d= -f2- | tr -d '"')
  case "$value" in
    "$bundle"/fonts/*) ;;
    *) fail "$field is '$value', not a font of the folder" ;;
  esac
done
echo "fonts from the folder: ok"

echo "verify-dist: ok ($bundle)"
