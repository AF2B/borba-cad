#!/bin/sh
# Installs Borba CAD for the current user: copies the program to ~/.local/share/borba-cad, adds it
# to the applications menu with its icon, and makes double-clicking a .bcad file open it.
# Nothing outside your home folder is touched. Run uninstall.sh to remove it.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)
data=${XDG_DATA_HOME:-$HOME/.local/share}
target=$data/borba-cad/app
applications=$data/applications
mime_packages=$data/mime/packages

mkdir -p "$target" "$applications" "$mime_packages"
cp -a "$here"/. "$target"/

cat > "$applications/borba-cad.desktop" <<DESKTOP
[Desktop Entry]
Type=Application
Name=Borba CAD
Comment=2D CAD written in Common Lisp
Exec=$target/borba-cad %f
Icon=$target/borba-cad.svg
Terminal=false
Categories=Graphics;Engineering;
MimeType=application/x-borba-cad;
StartupWMClass=borba-cad
DESKTOP

cat > "$mime_packages/application-x-borba-cad.xml" <<'MIME'
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
  <mime-type type="application/x-borba-cad">
    <comment>Borba CAD drawing</comment>
    <glob pattern="*.bcad"/>
  </mime-type>
</mime-info>
MIME

if command -v update-mime-database >/dev/null 2>&1; then
  update-mime-database "$data/mime" >/dev/null 2>&1 || true
fi
if command -v update-desktop-database >/dev/null 2>&1; then
  update-desktop-database "$applications" >/dev/null 2>&1 || true
fi

echo "Borba CAD is installed. Find it in your applications menu, or run $target/borba-cad"
