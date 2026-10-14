#!/usr/bin/env bash
# Borba CAD environment check. Prints one line per requirement and exits with status 1 when a
# hard requirement is missing. Called by `make check-env`; safe to run at any time (read-only).
#
# Usage: scripts/check-env.sh [QUICKLISP_SETUP] [EXPECTED_DIST_VERSION]

set -u

readonly QUICKLISP_SETUP="${1:-$HOME/quicklisp/setup.lisp}"
readonly EXPECTED_DIST_VERSION="${2:-2026-01-01}"
readonly TESTED_SBCL_VERSION="2.6.9"
readonly MINIMUM_SBCL_MAJOR=2
readonly MINIMUM_SBCL_MINOR=4
readonly FONT_CANDIDATES=(
  /usr/share/fonts/noto/NotoSansMono-Regular.ttf
  /usr/share/fonts/TTF/DejaVuSansMono.ttf
  /usr/share/fonts/dejavu/DejaVuSansMono.ttf
  /usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf
  /usr/share/fonts/liberation/LiberationMono-Regular.ttf
  /usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf
  /usr/share/fonts/gnu-free/FreeMono.otf
)

failures=0
warnings=0

report_ok() { printf '  [ ok ] %-28s %s\n' "$1" "$2"; }
report_warn() { printf '  [warn] %-28s %s\n' "$1" "$2"; warnings=$((warnings + 1)); }
report_fail() { printf '  [FAIL] %-28s %s\n' "$1" "$2"; failures=$((failures + 1)); }

has_command() { command -v "$1" >/dev/null 2>&1; }

has_library() { ldconfig -p 2>/dev/null | grep -q "$1"; }

check_sbcl() {
  if ! has_command sbcl; then
    report_fail "sbcl" "not found; install SBCL (Arch: pacman -S sbcl)"
    return
  fi
  local version major minor
  version="$(sbcl --noinform --version | awk '{print $2}')"
  major="${version%%.*}"
  minor="$(echo "$version" | cut -d. -f2)"
  if [[ "$major" -lt "$MINIMUM_SBCL_MAJOR" ||
        ( "$major" -eq "$MINIMUM_SBCL_MAJOR" && "$minor" -lt "$MINIMUM_SBCL_MINOR" ) ]]; then
    report_fail "sbcl" "version $version is older than $MINIMUM_SBCL_MAJOR.$MINIMUM_SBCL_MINOR"
  elif [[ "$version" != "$TESTED_SBCL_VERSION" ]]; then
    report_warn "sbcl" "version $version (tested with $TESTED_SBCL_VERSION)"
  else
    report_ok "sbcl" "version $version"
  fi
}

check_quicklisp() {
  if [[ ! -f "$QUICKLISP_SETUP" ]]; then
    report_fail "quicklisp" "$QUICKLISP_SETUP not found; see docs/development.md"
    return
  fi
  local distinfo version
  distinfo="$(dirname "$QUICKLISP_SETUP")/dists/quicklisp/distinfo.txt"
  if [[ ! -f "$distinfo" ]]; then
    report_fail "quicklisp dist" "$distinfo not found"
    return
  fi
  version="$(awk '/^version:/ {print $2}' "$distinfo")"
  if [[ "$version" == "$EXPECTED_DIST_VERSION" ]]; then
    report_ok "quicklisp dist" "$version (pinned)"
  else
    report_warn "quicklisp dist" "$version; the project is tested with $EXPECTED_DIST_VERSION (ql-dist:install-dist to pin it)"
  fi
}

check_native_libraries() {
  if ! has_command ldconfig; then
    report_warn "native libraries" "ldconfig not found; cannot verify SDL2, OpenGL and FreeType"
    return
  fi
  has_library "libSDL2-2.0.so.0" \
    && report_ok "libSDL2" "found" \
    || report_fail "libSDL2" "libSDL2-2.0.so.0 not found (Arch: pacman -S sdl2-compat)"
  has_library "libGL.so.1" \
    && report_ok "libGL" "found" \
    || report_fail "libGL" "libGL.so.1 not found (install your GPU driver's OpenGL package)"
  has_library "libfreetype.so.6" \
    && report_ok "libfreetype" "found" \
    || report_fail "libfreetype" "libfreetype.so.6 not found (Arch: pacman -S freetype2)"
}

check_build_tools() {
  has_command gcc \
    && report_ok "gcc" "$(gcc -dumpversion)" \
    || report_fail "gcc" "not found; cl-freetype2 compiles a small C helper (Arch: pacman -S gcc)"
  if [[ -f /usr/include/freetype2/ft2build.h ]]; then
    report_ok "freetype headers" "/usr/include/freetype2"
  else
    report_fail "freetype headers" "ft2build.h not found (Arch: pacman -S freetype2)"
  fi
  has_command make && report_ok "make" "$(make --version | head -n 1)" || report_fail "make" "not found"
  has_command git && report_ok "git" "$(git --version)" || report_warn "git" "not found; benchmark reports will lack a revision"
  has_command rlwrap && report_ok "rlwrap" "found (line editing in make repl)" \
    || report_warn "rlwrap" "not found; make repl works without line editing"
}

check_font() {
  local font
  for font in "${FONT_CANDIDATES[@]}"; do
    if [[ -f "$font" ]]; then
      report_ok "monospace font" "$font"
      return
    fi
  done
  report_fail "monospace font" "none of the usual locations has one; install Noto Sans Mono or set BORBA_CAD_FONT"
}

check_display() {
  if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
    report_ok "graphical session" "Wayland ($WAYLAND_DISPLAY)"
  elif [[ -n "${DISPLAY:-}" ]]; then
    report_ok "graphical session" "X11 ($DISPLAY)"
  else
    report_warn "graphical session" "none; make run, demo, test-gui and bench-gui need one (the rest works headless)"
  fi
}

check_writable() {
  local root
  root="$(cd "$(dirname "$0")/.." && pwd)"
  if mkdir -p "$root/build" "$root/var/log" 2>/dev/null && [[ -w "$root/build" && -w "$root/var/log" ]]; then
    report_ok "writable directories" "build/ and var/log/"
  else
    report_fail "writable directories" "cannot write to $root/build or $root/var/log"
  fi
}

printf 'Borba CAD environment check\n'
check_sbcl
check_quicklisp
check_native_libraries
check_build_tools
check_font
check_display
check_writable

printf '\n%d failure(s), %d warning(s)\n' "$failures" "$warnings"
[[ "$failures" -eq 0 ]]
