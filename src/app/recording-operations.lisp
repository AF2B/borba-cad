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

(defun recording-touches-existing-p (steps)
  "Return true when the steps of a recording change or erase an entity that was in the drawing before it began, which a
script that is run on another drawing can only do if that drawing has an entity with the same number.

STEPS: the steps of the recording, oldest first, each a list of changes."
  (let ((made (make-hash-table)))
    (dolist (step steps nil)
      (dolist (change step)
        (cond ((null (dom:change-before change))
               (setf (gethash (dom:change-entity-id change) made) t))
              ((not (gethash (dom:change-entity-id change) made))
               (return-from recording-touches-existing-p t)))))))

(defun command-saved-message (name
                              touches-existing)
  "Return what the user is told when a recording has been kept as a command.

NAME: the name of the command.
TOUCHES-EXISTING: true when the recording changes what was in the drawing before it began."
  (format nil "~a~@[ ~a~]"
          (i18n:trf "Kept as the command ~a. Run it from the command palette, Ctrl+K." name)
          (and touches-existing
               (i18n:tr "It changes entities already in the drawing, by number, so it needs the same ones there."))))

(defun save-recording-as-command (app
                                  name
                                  steps)
  "Keep the recorded script as a command in the folder of commands, so that the command palette lists it. Returns NIL
when it was saved, or the sentence that says why it was not.

APP: the application.
NAME: what the command is called.
STEPS: the steps of the recording, to tell whether it changes what was already there."
  (handler-case
      (let* ((file-name (command-file-name name))
             (path (and (commands-directory app)
                        (recorded-command-path app file-name))))
        (cond ((null path)
               (i18n:tr "There is no folder for commands."))
              ((find-recorded-command app file-name)
               (i18n:tr "A command with that name already exists."))
              (t
               (ensure-directories-exist path)
               (persist:save-text-file path
                                       (lambda (stream)
                                         (write-string (app-recorded-script app) stream)))
               (scan-recorded-commands app)
               (show-notice app
                            (i18n:tr "Command saved")
                            (command-saved-message file-name
                                                   (recording-touches-existing-p steps)))
               nil)))
    ((or base:edit-refused base:drawing-file-error) (condition)
      (remove #\Newline (princ-to-string condition)))))

(defun ask-recording-command-name (app
                                   steps
                                   &optional problem)
  "Ask the name of the command that a recording is kept as; a name that cannot be used is said and asked again.

APP: the application.
STEPS: the steps of the recording.
PROBLEM: what was wrong with the last answer, or NIL."
  (open-text-prompt app
                    (i18n:tr "Keep as a command")
                    (if problem
                        (format nil "~a ~a" problem (i18n:tr "Type the name of the command."))
                        (i18n:tr "Type the name of the command."))
                    (i18n:tr "My command")
                    (lambda (text)
                      (let ((again (save-recording-as-command app
                                                              text
                                                              steps)))
                        (when again
                          (ask-recording-command-name app
                                                      steps
                                                      again))))))

(defun ask-what-to-do-with-recording (app
                                      steps)
  "Ask whether the recording is to be kept as a command, which the command palette runs, or saved as a script file.

APP: the application.
STEPS: the steps of the recording."
  (open-choice-prompt app
                      (i18n:tr "Recording ready")
                      (i18n:tr "Keep it as a command to run from the command palette, or save it as a script file.")
                      (list (cons (i18n:tr "Keep as a command")
                                  (lambda () (ask-recording-command-name app steps)))
                            (cons (i18n:tr "Save as a script...")
                                  (lambda ()
                                    (open-file-dialog app
                                                      :save
                                                      nil
                                                      :recording)))
                            (cons (i18n:tr "Discard") nil))))

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
           (ask-what-to-do-with-recording app steps)))))

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
