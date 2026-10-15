Borba CAD - 2D CAD written in Common Lisp
=========================================

To run it: double-click borba-cad.exe. It opens no console window. If nothing happens, double-click
borba-cad-console.exe instead: it opens one and shows what the program prints, and the log is in
%LOCALAPPDATA%\BorbaCAD\log.

What is in this folder
  borba-cad.exe          the program
  borba-cad-console.exe  the same program, with a console window for diagnosing a problem
  *.dll                  the libraries it needs (SDL2, FreeType and what they need)
  fonts\                 Noto Sans and Noto Sans Mono, when this folder has them; otherwise the
                         program uses Segoe UI and Consolas from Windows
  examples\              a sample drawing and two scripts
  LICENSES\              licenses of the parts that are not Borba CAD

What it needs from your system
  Windows 10 or 11, 64 bits, and a graphics driver with OpenGL 3.3. It keeps its log and the list of
  recent drawings in %LOCALAPPDATA%\BorbaCAD. You can also drag a .bcad file onto borba-cad.exe.
