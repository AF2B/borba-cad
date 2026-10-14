(in-package #:borba-cad/platform-tests)

(def-suite session
  :description "The window loop's own logic: what it does between two batches of SDL events."
  :in all)

(in-suite session)

(defconstant +demo-iteration-limit+ 5000
  "Loop passes after which a demonstration that has not finished counts as stuck. The real
demonstration needs about 400.")

(defun draw-like-the-window (session)
  "Stand in for DRAW-FRAME without a window: render the application, note the first frame like the
real drawing does, and take the screenshot when it is due.

SESSION: the session."
  (app:app-render (platform::session-app session))
  (platform::log-first-frame session (sbcl:monotonic-nanoseconds))
  (when (platform::screenshot-due-p session)
    (setf (platform::session-screenshot session) nil)))

(defun run-demo-without-events (directory
                                &key screenshot)
  "Run the demonstration through STEP-SESSION the way the window loop does, but with no window and
without a single SDL event, which is what a hidden or occluded window delivers. Returns the
session and the number of passes it took, or the iteration limit when it never finished.

DIRECTORY: scratch directory for the log.
SCREENSHOT: true to also wait for the demonstration screenshot."
  (let* ((config (base:default-config :project-root directory))
         (telemetry (obs:make-telemetry config))
         (application (app:make-app config :telemetry telemetry))
         (session (platform::make-session :app application
                                          :window nil
                                          :renderer nil
                                          :telemetry telemetry
                                          :phases (list :vsync t)
                                          :demo t
                                          :exit-after-demo t
                                          :screenshot (and screenshot "unused.png")
                                          :steps (app:demo-steps application)
                                          :begun (sbcl:monotonic-nanoseconds)))
         (passes 0))
    (unwind-protect
         (loop until (or (app:app-quit-requested application)
                         (>= passes +demo-iteration-limit+))
               do (incf passes)
                  (platform::step-session session #'draw-like-the-window))
      (obs:close-telemetry telemetry))
    (values session passes)))

(test the-demonstration-finishes-although-no-event-ever-arrives
  (with-temporary-directory (directory)
    (multiple-value-bind (session passes) (run-demo-without-events directory)
      (is (null (platform::session-steps session)) "the steps ran out")
      (is (app:app-quit-requested (platform::session-app session)) "the demonstration asked to quit")
      (is (< passes +demo-iteration-limit+) "it took ~d passes" passes)
      (is (= 10 (dom:drawing-entity-count (app:app-drawing (platform::session-app session))))))))

(test the-demonstration-screenshot-is-taken-before-the-demonstration-quits
  (with-temporary-directory (directory)
    (multiple-value-bind (session passes) (run-demo-without-events directory :screenshot t)
      (is (null (platform::session-screenshot session)) "the screenshot was taken")
      (is (app:app-quit-requested (platform::session-app session)))
      (is (< passes +demo-iteration-limit+)))))

(test a-session-without-a-demonstration-draws-only-when-asked
  (with-temporary-directory (directory)
    (let* ((config (base:default-config :project-root directory))
           (telemetry (obs:make-telemetry config))
           (application (app:make-app config :telemetry telemetry))
           (session (platform::make-session :app application
                                            :window nil
                                            :renderer nil
                                            :telemetry telemetry
                                            :phases (list :vsync t)
                                            :begun 0))
           (frames 0))
      (unwind-protect
           (flet ((run-pass ()
                    (platform::step-session session
                                            (lambda (current)
                                              (incf frames)
                                              (draw-like-the-window current)))))
             (app:app-request-redraw application)
             (run-pass)
             (is (= 1 frames) "a requested redraw is drawn")
             (run-pass)
             (run-pass)
             (is (= 1 frames) "nothing is drawn while nothing changed")
             (app:app-request-redraw application)
             (run-pass)
             (is (= 2 frames)))
        (obs:close-telemetry telemetry)))))
