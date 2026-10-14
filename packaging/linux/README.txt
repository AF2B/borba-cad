Borba CAD - 2D CAD written in Common Lisp
=========================================

To run it: double-click "Borba CAD.desktop" (or the file "borba-cad"). Your file manager may ask
once whether to run it; choose Execute. From a terminal:  ./borba-cad  [drawing.bcad]

To install it for your user (menu entry, icon, double-click on .bcad files): run ./install.sh

What is in this folder
  borba-cad        the launcher (a shell script)
  borba-cad.bin    the program
  lib/             the libraries it needs that most systems do not have (SDL2, FreeType, ...)
  fonts/           Noto Sans and Noto Sans Mono (SIL Open Font License, see LICENSES/)
  examples/        a sample drawing

What it needs from your system
  Linux x86_64 with a graphical session (Wayland or X11), OpenGL 3.3, and a C library (glibc) at
  least as new as the one of the machine that built this folder; the build machine is named in
  BUILD-INFO.txt. Where it keeps things: the log and the list of recent drawings in
  ~/.local/state/borba-cad. If it does not start, that folder has last-error.txt.
