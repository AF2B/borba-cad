(in-package #:borba-cad/app)

;;;; Autosave and recovery. While a drawing has changes that are not saved, a copy of it is written to the state
;;;; directory every minute, so that a crash or a power cut loses a minute of work and not the session. The copy
;;;; is taken away as soon as the drawing is saved or closed in the usual ways, and when the program starts and
;;;; finds one it asks whether to take the work back.

(defconstant +autosave-interval-nanoseconds+ (* 60 sbcl:+nanoseconds-per-second+)
  "How long a drawing with unsaved changes goes without a copy being written.")

(defparameter *autosave-file-name* "autosave.bcad"
  "Name, in the state directory, of the copy of the drawing that has unsaved changes.")

(defparameter *autosave-origin-file-name* "autosave-origin.txt"
  "Name, in the state directory, of the file that says which file the copy came from: one line, empty for a
drawing that had no file yet.")

(defun autosave-file (app
                      name)
  "Return the path of a file of the autosave in the state directory, or NIL when the program has none.

APP: the application.
NAME: the file name."
  (let ((state (base:config-state-directory (app-config app))))
    (and state
         (merge-pathnames name (uiop:ensure-directory-pathname state)))))

(defun autosave-exists-p (app)
  "Return true when a copy of a drawing waits in the state directory.

APP: the application."
  (let ((file (autosave-file app *autosave-file-name*)))
    (and file (eq :file (sbcl:file-kind file)))))

(defun write-autosave (app)
  "Write the copy of the drawing, and the note of the file it came from. A failure is logged and never reaches the
user: the copy is a convenience, and it must not get in the way of the work it protects.

APP: the application."
  (let ((started (sbcl:monotonic-nanoseconds))
        (file (autosave-file app *autosave-file-name*))
        (origin (autosave-file app *autosave-origin-file-name*)))
    (handler-case
        (multiple-value-bind (saved stats)
            (progn
              (ensure-directories-exist file)
              (persist:save-drawing (app-drawing app) file))
          (declare (ignore saved))
          (persist:save-text-file origin
                                  (lambda (stream)
                                    (write-line (if (app-file-path app)
                                                    (uiop:native-namestring (app-file-path app))
                                                    "")
                                                stream)))
          (setf (app-autosave-revision app) (dom:drawing-revision (app-drawing app))
                (app-autosave-written app) t)
          (obs:record-operation-result (app-telemetry app)
                                       "file.autosave"
                                       (- (sbcl:monotonic-nanoseconds) started)
                                       :info
                                       :bytes (getf stats :bytes)
                                       :entity-count (dom:drawing-entity-count (app-drawing app))
                                       :result :success))
      (error (condition)
        (when (app-telemetry app)
          (obs:log-condition (obs:telemetry-logger (app-telemetry app))
                             :warn
                             "app.autosave-failed"
                             condition))))))

(defun discard-autosave (app)
  "Take the copy of the drawing away, because the work in it is saved, or given up.

APP: the application."
  (dolist (name (list *autosave-file-name* *autosave-origin-file-name*))
    (let ((file (autosave-file app name)))
      (when file
        (sbcl:delete-file-if-exists file))))
  (setf (app-autosave-revision app) -1
        (app-autosave-written app) nil))

(defun autosave-tick (app
                      &optional (now (sbcl:monotonic-nanoseconds)))
  "Do what the autosave needs now: write a copy when the drawing has changes that are not saved and a minute has
passed since the last look, and take the copy away once the drawing has no unsaved changes. Cheap enough to be
called on every pass of the window loop.

APP: the application.
NOW: the monotonic time, in nanoseconds."
  (when (base:config-state-directory (app-config app))
    (cond ((null (app-autosave-time app))
           (setf (app-autosave-time app) now))
          ((>= (- now (app-autosave-time app)) +autosave-interval-nanoseconds+)
           (setf (app-autosave-time app) now)
           (cond ((app-dirty-p app)
                  (unless (eql (app-autosave-revision app) (dom:drawing-revision (app-drawing app)))
                    (write-autosave app)))
                 ((app-autosave-written app)
                  (discard-autosave app)))))))

;;; Recovery

(defun autosave-origin (app)
  "Return the pathname of the file the waiting copy came from, or NIL when it came from a drawing that had no file.

APP: the application."
  (let ((file (autosave-file app *autosave-origin-file-name*)))
    (when (and file (eq :file (sbcl:file-kind file)))
      (let ((line (string-trim '(#\Space #\Tab #\Return #\Newline)
                               (or (with-open-file (stream file :external-format :utf-8)
                                     (read-line stream nil nil))
                                   ""))))
        (and (plusp (length line))
             (uiop:parse-native-namestring line))))))

(defun autosave-time-text (app)
  "Return when the waiting copy was written, as the date and the minute.

APP: the application."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (file-write-date (autosave-file app *autosave-file-name*)))
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun recover-autosave (app)
  "Make the waiting copy the drawing being edited, as a drawing with unsaved changes that belongs to the file it
came from. Returns true on success. When the copy cannot be read the reason is shown and the copy is kept.

APP: the application."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (let* ((drawing (persist:load-drawing (autosave-file app *autosave-file-name*)))
               (origin (autosave-origin app)))
          (reset-document app
                          drawing
                          (and origin (eq :file (sbcl:file-kind origin)) origin))
          (setf (app-saved-revision app) -1
                (app-autosave-written app) t
                (app-autosave-revision app) (dom:drawing-revision drawing))
          (set-message app (i18n:tr "The work that was not saved is back."))
          t)
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "file.recover"
                             started
                             condition)))))

(defun offer-autosave-recovery (app)
  "When the program was closed with work that was not saved, ask whether to take it back. Recover opens it as a
drawing with unsaved changes, Discard throws it away, and Not now (which Escape chooses) leaves it for the next
time.

APP: the application."
  (when (autosave-exists-p app)
    (let ((origin (autosave-origin app)))
      (open-choice-prompt app
                          (i18n:tr "Recover unsaved work?")
                          (i18n:trf "The program closed before ~a was saved. Recover what was kept at ~a?"
                                    (if origin
                                        (file-namestring origin)
                                        (i18n:tr "a new drawing"))
                                    (autosave-time-text app))
                          (list (cons (i18n:tr "Recover")
                                      (lambda () (recover-autosave app)))
                                (cons (i18n:tr "Discard")
                                      (lambda () (discard-autosave app)))
                                (cons (i18n:tr "Not now") nil))))))
