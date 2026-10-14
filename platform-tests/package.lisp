(defpackage #:borba-cad/platform-tests
  (:use #:cl #:fiveam)
  (:import-from #:borba-cad/test-support
                #:with-temporary-directory)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:sbcl #:borba-cad/sbcl-support)
                    (#:geo #:borba-cad/geometry)
                    (#:rng #:borba-cad/test-support)
                    (#:scene #:borba-cad/scene)
                    (#:ui #:borba-cad/ui)
                    (#:app #:borba-cad/app)
                    (#:dom #:borba-cad/domain)
                    (#:obs #:borba-cad/observability)
                    (#:platform #:borba-cad/platform))
  (:documentation "Tests of the window program: PNG encoding, SDL event translation, font
selection and, when a graphical session exists, the real OpenGL renderer.")
  (:export #:run-all
           #:all))

(in-package #:borba-cad/platform-tests)

(def-suite all
  :description "Every test of the window program.")

(defun run-all (&key (suite 'all))
  "Run SUITE and return true when every check passed.

SUITE: FiveAM suite designator; defaults to the root suite."
  (let ((results (run suite)))
    (explain! results)
    (results-status results)))

(defun display-or-skip ()
  "Return true when a graphical session exists; otherwise record an explicit skip, so a run
without a display says so instead of passing silently.

The caller must stop when this returns NIL."
  (or (platform:display-available-p)
      (progn (skip "no graphical session (WAYLAND_DISPLAY and DISPLAY are unset)")
             nil)))
