;;;; Borba CAD: ASDF systems.
;;;;
;;;; borba-cad            headless core: no window, GPU or foreign-library dependency
;;;; borba-cad/platform   SDL window, OpenGL renderer, FreeType fonts, event loop
;;;; borba-cad/test-support, borba-cad/tests, borba-cad/bench(-gui)   correctness tests and benchmarks

(asdf:defsystem "borba-cad"
  :description "Borba CAD core: headless 2D CAD engine (domain, geometry, persistence, observability, scene, UI and application logic)."
  :version "1.2.0"
  :depends-on ("uiop"
               #-win32 (:require "sb-posix"))
  :pathname "src/"
  :serial t
  :components ((:module "sbcl-support"
                :serial t
                :components ((:file "package")
                             (:file "clock")
                             (:file "memory")
                             (:file "system")
                             (:file "streams")
                             (:file "portable-filesystem")
                             (:file "filesystem")))
               (:module "i18n"
                :serial t
                :components ((:file "package")
                             (:file "i18n")
                             (:static-file "pt-br.catalog")))
               (:module "base"
                :serial t
                :components ((:file "package")
                             (:file "tolerances")
                             (:file "conditions")
                             (:file "units")
                             (:file "config")))
               (:module "geometry"
                :serial t
                :components ((:file "package")
                             (:file "bounds")
                             (:file "distance")
                             (:file "clipping")
                             (:file "crossing")
                             (:file "curves")
                             (:file "intersect")
                             (:file "shapes")
                             (:file "measure")
                             (:file "text-extent")
                             (:file "dimension")
                             (:file "annotation")))
               (:module "domain"
                :serial t
                :components ((:file "package")
                             (:file "layer")
                             (:file "entity")
                             (:file "line")
                             (:file "rectangle")
                             (:file "circle")
                             (:file "polyline")
                             (:file "text")
                             (:file "dimension")
                             (:file "radial-dimension")
                             (:file "angular-dimension")
                             (:file "arc")
                             (:file "ellipse")
                             (:file "polygon")
                             (:file "transforms")
                             (:file "store")
                             (:file "drawing")
                             (:file "changes")
                             (:file "snapping")
                             (:file "picking")
                             (:file "commands")
                             (:file "reshaping")
                             (:file "transfer")))
               (:module "viewport"
                :serial t
                :components ((:file "package")
                             (:file "viewport")
                             (:file "grid")))
               (:module "persistence"
                :serial t
                :components ((:file "package")
                             (:file "reader")
                             (:file "format")
                             (:file "writer")
                             (:file "loader")
                             (:file "paths")
                             (:file "recent")
                             (:file "settings")
                             (:file "files")))
               (:module "dxf"
                :serial t
                :components ((:file "package")
                             (:file "colors")
                             (:file "reader")
                             (:file "spline")
                             (:file "import")
                             (:file "export")
                             (:file "files")))
               (:module "plot"
                :serial t
                :components ((:file "package")
                             (:file "layout")
                             (:file "pdf")
                             (:file "svg")))
               (:module "image"
                :serial t
                :components ((:file "package")
                             (:file "jpeg")))
               (:module "script"
                :serial t
                :components ((:file "package")
                             (:file "doubles")
                             (:file "context")
                             (:file "dsl")
                             (:file "evaluator")
                             (:file "export")))
               (:module "observability"
                :serial t
                :components ((:file "package")
                             (:file "statistics")
                             (:file "logger")
                             (:file "metrics")
                             (:file "telemetry")
                             (:file "timing")
                             (:file "summary")))
               (:module "scene"
                :serial t
                :components ((:file "package")
                             (:file "draw-list")
                             (:file "shapes")
                             (:file "theme")
                             (:file "scene")))
               (:module "ui"
                :serial t
                :components ((:file "package")
                             (:file "metrics")
                             (:file "tokens")
                             (:file "icons")
                             (:file "logo")
                             (:file "text-edit")
                             (:file "widgets")
                             (:file "draw-widgets")))
               (:module "app"
                :serial t
                :components ((:file "package")
                             (:file "events")
                             (:file "state")
                             (:file "clipboard")
                             (:file "prompts")
                             (:file "operations")
                             (:file "dxf-operations")
                             (:file "console")
                             (:file "script-operations")
                             (:file "image-operations")
                             (:file "plot-operations")
                             (:file "autosave")
                             (:file "constraints")
                             (:file "grips")
                             (:file "file-browser")
                             (:file "tools")
                             (:file "transform-tools")
                             (:file "measure-tools")
                             (:file "dimension-tools")
                             (:file "reshape-tools")
                             (:file "entity-clipboard")
                             (:file "panels")
                             (:file "side-panels")
                             (:file "menus")
                             (:file "welcome")
                             (:file "file-dialog")
                             (:file "file-dialog-widgets")
                             (:file "dialogs")
                             (:file "input")
                             (:file "frame")
                             (:file "scenario"))))
  :in-order-to ((test-op (test-op "borba-cad/tests"))))

(asdf:defsystem "borba-cad/platform"
  :description "Borba CAD window program: SDL window, OpenGL renderer, FreeType glyph atlas, event loop."
  :depends-on ("borba-cad"
               "cffi"
               "cl-opengl"
               "cl-freetype2"
               "sdl2")
  :pathname "src/platform/"
  :serial t
  :components ((:file "package")
               (:file "png")
               (:file "font")
               (:file "renderer")
               (:file "events")
               (:file "clipboard")
               (:file "main")))

(asdf:defsystem "borba-cad/test-support"
  :description "Seeded PRNG and deterministic workload generator shared by tests and benchmarks."
  :depends-on ("borba-cad")
  :pathname "test-support/"
  :serial t
  :components ((:file "package")
               (:file "prng")
               (:file "workload")
               (:file "checksum")
               (:file "files")))

(asdf:defsystem "borba-cad/bench"
  :description "Reproducible benchmarks with environment fingerprints and workload checksums."
  :depends-on ("borba-cad"
               "borba-cad/test-support")
  :pathname "bench/"
  :serial t
  :components ((:file "package")
               (:file "measure")
               (:file "environment")
               (:file "scenarios")
               (:file "report")
               (:file "run")))

(asdf:defsystem "borba-cad/bench-gui"
  :description "Windowed frame benchmark: the real renderer and display (needs a graphical session)."
  :depends-on ("borba-cad/bench"
               "borba-cad/platform")
  :pathname "bench/"
  :serial t
  :components ((:file "gui")))

(asdf:defsystem "borba-cad/platform-tests"
  :description "Tests of the window program; the renderer tests need a graphical session and skip with a message otherwise."
  :depends-on ("borba-cad/platform"
               "borba-cad/test-support"
               "fiveam")
  :pathname "platform-tests/"
  :serial t
  :components ((:file "package")
               (:file "png-tests")
               (:file "events-tests")
               (:file "options-tests")
               (:file "font-tests")
               (:file "session-tests")
               (:file "renderer-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:borba-cad/platform-tests '#:run-all)
               (error "Borba CAD platform tests failed"))))

(asdf:defsystem "borba-cad/tests"
  :description "Headless correctness tests for Borba CAD (FiveAM)."
  :depends-on ("borba-cad"
               "borba-cad/test-support"
               "borba-cad/bench"
               "fiveam")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "base-tests")
               (:file "sbcl-support-tests")
               (:file "prng-tests")
               (:file "checksum-tests")
               (:file "geometry-tests")
               (:file "domain-entity-tests")
               (:file "domain-drawing-tests")
               (:file "viewport-tests")
               (:file "persistence-tests")
               (:file "observability-tests")
               (:file "scene-tests")
               (:file "shapes-tests")
               (:file "ui-tests")
               (:file "app-tests")
               (:file "panels-tests")
               (:file "menu-tests")
               (:file "entity-clipboard-tests")
               (:file "crossing-tests")
               (:file "migration-tests")
               (:file "text-angle-tests")
               (:file "curve-entities-tests")
               (:file "grips-tests")
               (:file "dxf-tests")
               (:file "dxf-app-tests")
               (:file "script-tests")
               (:file "console-tests")
               (:file "welcome-tests")
               (:file "image-tests")
               (:file "autosave-tests")
               (:file "constraints-tests")
               (:file "snapping-tests")
               (:file "transform-tests")
               (:file "measure-tests")
               (:file "dimension-tests")
               (:file "reshaping-tests")
               (:file "plot-tests")
               (:file "i18n-tests")
               (:file "file-dialog-tests")
               (:file "packaging-tests")
               (:file "clipboard-tests")
               (:file "file-tests")
               (:file "bench-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:borba-cad/tests '#:run-all)
               (error "Borba CAD tests failed"))))
