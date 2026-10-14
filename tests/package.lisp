(defpackage #:borba-cad/tests
  (:use #:cl #:fiveam)
  (:import-from #:borba-cad/test-support
                #:make-temporary-directory
                #:with-temporary-directory)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n)
                    (#:sbcl #:borba-cad/sbcl-support)
                    (#:geo #:borba-cad/geometry)
                    (#:rng #:borba-cad/test-support)
                    (#:dom #:borba-cad/domain)
                    (#:view #:borba-cad/viewport)
                    (#:persist #:borba-cad/persistence)
                    (#:obs #:borba-cad/observability)
                    (#:scene #:borba-cad/scene)
                    (#:ui #:borba-cad/ui)
                    (#:app #:borba-cad/app)
                    (#:bench #:borba-cad/bench))
  (:export #:run-all
           #:all))

(in-package #:borba-cad/tests)

(def-suite all
  :description "Every Borba CAD correctness test.")

(defun run-all (&key (suite 'all))
  "Run SUITE and return true when every check passed.

SUITE: FiveAM suite designator; defaults to the root suite."
  (let ((results (run suite)))
    (explain! results)
    (results-status results)))
