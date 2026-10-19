(in-package #:borba-cad/app)

;;;; Recording what the user does as a script. The steps that undo keeps are the record: when the recording
;;;; begins the latest step is remembered, and when it ends the steps that came after it, as undo has left them,
;;;; are written as the commands that make them. A step that was undone is not in the script, and one that was
;;;; done again is. Layers and blocks are not steps of undo, so the script makes the ones it uses.

(defstruct (recording (:constructor make-recording (history
                                                    marker)))
  "A recording that is under way.

HISTORY: the history the steps are in; a new drawing or an opened one has another, and ends the recording.
MARKER: the latest step when the recording began, or NIL when there was none."
  history
  marker)

(defun app-recording-p (app)
  "Return true while what the user does is being recorded.

APP: the application."
  (and (app-recording app) t))

(defun recorded-steps (app)
  "Return the steps made since the recording began, oldest first, or NIL as the second value when they cannot be
told: the drawing was replaced, or steps from before the recording were undone.

APP: the application."
  (let ((recording (app-recording app)))
    (if (eq (recording-history recording) (app-history app))
        (dom:history-steps-since (app-history app)
                                 (recording-marker recording))
        (values '() nil))))

(defun operation-start-recording (app)
  "Begin recording what the user does, from the drawing as it is now.

APP: the application."
  (setf (app-recording app)
        (make-recording (app-history app)
                        (dom:history-latest-step (app-history app))))
  (set-message app (i18n:tr "Recording: what you draw and change is being written as a script."))
  (app-request-redraw app))

(defun operation-stop-recording (app)
  "End the recording and ask where to save the script that it made; say so when there is nothing to save or the
recording cannot be told.

APP: the application."
  (multiple-value-bind (steps known) (recorded-steps app)
    (setf (app-recording app) nil)
    (app-request-redraw app)
    (cond ((not known)
           (show-notice app
                        (i18n:tr "The recording was lost")
                        (i18n:tr "The drawing was replaced, or actions from before the recording were undone.")))
          ((null steps)
           (show-notice app
                        (i18n:tr "Nothing was recorded")
                        (i18n:tr "Draw or change something while recording, then stop.")))
          (t
           (setf (app-recorded-script app) (script:steps-script steps (app-drawing app)))
           (open-file-dialog app
                             :save
                             nil
                             :recording)))))

(defun operation-toggle-recording (app)
  "Begin recording when nothing is recorded, and end the recording when it is.

APP: the application."
  (if (app-recording-p app)
      (operation-stop-recording app)
      (operation-start-recording app)))

(defun save-recording-from-app (app
                                path-text)
  "Save the recorded script at PATH-TEXT (.lisp is added to a name without an extension) and tell the user how it
went in a modal. On failure the previous file is untouched. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (multiple-value-bind (saved stats)
            (persist:save-text-file (script-file-path path-text)
                                    (lambda (stream)
                                      (write-string (app-recorded-script app) stream)))
          (obs:record-operation-result (app-telemetry app)
                                       "file.save-recording"
                                       (- (sbcl:monotonic-nanoseconds) started)
                                       :info
                                       :bytes (getf stats :bytes)
                                       :result :success)
          (show-notice app
                       (i18n:tr "Recording saved")
                       (i18n:trf "~a was saved (~a). Run it on the drawing as it was when the recording began."
                                 (file-namestring saved)
                                 (base:format-byte-size (getf stats :bytes))))
          t)
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-export-operation app
                               "file.save-recording"
                               started
                               condition)))))
