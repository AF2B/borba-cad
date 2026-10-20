# Borba CAD

A native 2D CAD written in Common Lisp: it draws, edits, measures and prints to PDF, with a console that is Lisp itself.

![Borba CAD drawing a house plan, with the door selected and its grips](images/main-window.png)

- **Download:** the Windows and Linux programs of every version are on the
  [releases page](https://github.com/AF2B/borba-cad/releases). A version is a tag of the repository.
- **Source and README:** [github.com/AF2B/borba-cad](https://github.com/AF2B/borba-cad).

## Run it

There is nothing to install and no Lisp to set up: each download is a folder that carries the program, the libraries
and the fonts it needs. Download, extract, double-click.

| System | Steps |
|---|---|
| **Windows** 10 or 11, 64 bits | 1. Download `borba-cad-VERSION-windows-x86_64.zip`.<br>2. Extract it (right-click, *Extract All*).<br>3. Double-click `borba-cad.exe`. Windows warns about an unknown app because the program is not signed: click *More info*, then *Run anyway*. |
| **Linux** x86_64 | 1. Download `borba-cad-VERSION-linux-x86_64.tar.gz`.<br>2. Extract it anywhere.<br>3. Double-click `Borba CAD.desktop`. The file manager may ask once whether to run it: choose *Execute*. |

If the Windows program does not open, double-click `borba-cad-console.exe`: it shows what the program prints. On
Linux, run `./borba-cad` from a terminal to see it, and `./install.sh` adds a menu entry and opens `.bcad` files
with the program. Both need a graphics driver with OpenGL 3.3. The Windows program is made by GitHub and has not yet
been opened on a real Windows machine.

## Documentation

| Page | What it is |
|---|---|
| [Development](development.md) | How to build, test, run, package and release the program |
| [Architecture](architecture.md) | The modules, what each may depend on, and the decisions behind them |
| [Geometry](geometry.md) | The geometry of every tool, and the tolerances |
| [Performance](performance.md) | What is measured, how, and the baseline |
| [API reference](reference.md) | Every exported symbol, from its docstring |
| [Specification](specification.md) | The approved specification |
| [Experiment report](experiment-report.md) | The spikes that came before the design |
