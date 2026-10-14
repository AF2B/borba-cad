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

;;; An unexpected error ends one event or one frame, never the session

(defun a-guarded-application (directory)
  "Return an application, and the log file's directory, with telemetry written into DIRECTORY."
  (let* ((config (base:default-config :project-root directory))
         (telemetry (obs:make-telemetry config)))
    (values (app:make-app config :telemetry telemetry) telemetry)))

(defun log-text (directory)
  "Return the text of the log written under DIRECTORY."
  (let ((file (merge-pathnames "var/log/borba-cad.log" directory)))
    (if (probe-file file)
        (with-open-file (in file)
          (let ((text (make-string (file-length in))))
            (subseq text 0 (read-sequence text in))))
        "")))

(test an-unexpected-error-in-an-event-is-reported-logged-and-survived
  (with-temporary-directory (directory)
    (multiple-value-bind (application telemetry) (a-guarded-application directory)
      (unwind-protect
           (progn
             (is (null (platform::call-guarded application
                                               "handling input"
                                               (lambda () (error 'type-error :datum 1 :expected-type 'string))))
                 "after the error the call returns nothing")
             (let ((message (app:app-message application)))
               (is (eq :error (cdr message)))
               (is (search "Internal error while handling input" (car message)))
               (is (search "Save your work" (car message))))
             (is (typep (app:app-last-error application) 'type-error))
             (is (= 7 (platform::call-guarded application "anything" (lambda () 7))) "and success passes through"))
        (obs:close-telemetry telemetry))
      (let ((text (log-text directory)))
        (is (search "app.internal-error" text))
        (is (search "context=" text))
        (is (search "backtrace=" text) "the log keeps the call stack")))))

(test with-the-debug-switch-an-unexpected-error-is-left-for-the-debugger
  (with-temporary-directory (directory)
    (multiple-value-bind (application telemetry) (a-guarded-application directory)
      (unwind-protect
           (progn
             (sb-posix:setenv "BORBA_CAD_DEBUG" "1" 1)
             (unwind-protect
                  (signals error
                    (platform::call-guarded application "handling input" (lambda () (error "boom"))))
               (sb-posix:unsetenv "BORBA_CAD_DEBUG")))
        (obs:close-telemetry telemetry)))))

(test a-frame-that-fails-is-retried-and-three-failures-in-a-row-end-the-program
  (with-temporary-directory (directory)
    (multiple-value-bind (application telemetry) (a-guarded-application directory)
      (let ((session (platform::make-session :app application
                                             :window nil
                                             :renderer nil
                                             :telemetry telemetry
                                             :phases (list :vsync t)
                                             :begun 0))
            (attempts 0))
        (unwind-protect
             (flet ((fails (session)
                      (declare (ignore session))
                      (incf attempts)
                      (error "the frame cannot be drawn")))
               (platform::draw-guarded session #'fails)
               (platform::draw-guarded session #'fails)
               (is (= 2 attempts) "two failures are survived")
               (is (= 2 (platform::session-frame-failures session)))
               (signals error (platform::draw-guarded session #'fails)))
          (obs:close-telemetry telemetry))))))

(test a-frame-that-draws-resets-the-count-of-failures
  (with-temporary-directory (directory)
    (multiple-value-bind (application telemetry) (a-guarded-application directory)
      (let ((session (platform::make-session :app application
                                             :window nil
                                             :renderer nil
                                             :telemetry telemetry
                                             :phases (list :vsync t)
                                             :begun 0))
            (fail-next t))
        (unwind-protect
             (flet ((sometimes (session)
                      (declare (ignore session))
                      (when fail-next
                        (error "not this time"))
                      (app:app-render application)))
               (platform::draw-guarded session #'sometimes)
               (platform::draw-guarded session #'sometimes)
               (is (= 2 (platform::session-frame-failures session)))
               (setf fail-next nil)
               (platform::draw-guarded session #'sometimes)
               (is (= 0 (platform::session-frame-failures session))))
          (obs:close-telemetry telemetry))))))
