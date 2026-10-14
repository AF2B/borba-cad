#!/bin/sh
# Removes what install.sh added. Your drawings are never touched; the log and the list of recent
# drawings in ~/.local/state/borba-cad are left, delete that folder too if you want them gone.

set -eu

data=${XDG_DATA_HOME:-$HOME/.local/share}
rm -rf "$data/borba-cad/app"
rmdir "$data/borba-cad" 2>/dev/null || true
rm -f "$data/applications/borba-cad.desktop" "$data/mime/packages/application-x-borba-cad.xml"
if command -v update-mime-database >/dev/null 2>&1; then
  update-mime-database "$data/mime" >/dev/null 2>&1 || true
fi
echo "Borba CAD is removed."
