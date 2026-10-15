(in-package #:borba-cad/app)

;;;; Running a script file on the drawing and writing the drawing as a script. A script is a program, so it
;;;; is never run without the user choosing the file and answering a question, which also says where it
;;;; draws: on a new layer of the drawing being edited, or in a new drawing that leaves this one alone. What
;;;; it does is one step of undo, and it is stopped when it takes more than a minute.

(defparameter *script-extensions* (list "lisp")
  "Extensions of the files that the dialog lists when it is about scripts.")

(defconstant +maximum-script-bytes+ (* 2 1024 1024)
  "Largest script file that will be read.")

(defun script-file-path (text)
  "Return the pathname of the script file that TEXT names, with the extension .lisp added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    (first *script-extensions*)
                                    *script-extensions*))

;;; The layer a script draws on

(defun script-layer-name (app
                          path)
  "Return the name of the layer a script draws on: the name of its file, with a number after it when the
drawing already has a layer of that name.

APP: the application.
PATH: the script file."
  (let* ((base (let ((name (pathname-name path)))
                 (subseq name 0 (min (length name) (- dom:+maximum-layer-name-length+ 4)))))
         (name base))
    (loop for number from 2
          while (dom:find-layer-by-name (app-drawing app) name)
          do (setf name (format nil "~a ~d" base number)))
    name))

(defun begin-script-layer (app
                           path)
  "Make a new layer for a script, named for its file, and make it the one new entities go on. Returns the
id of the layer and the id of the layer that was current.

APP: the application.
PATH: the script file."
  (let* ((drawing (app-drawing app))
         (previous (dom:drawing-current-layer-id drawing))
         (layer (dom:add-layer drawing
                               (script-layer-name app path)
                               (dom:palette-color-at (dom:drawing-next-layer-id drawing)))))
    (dom:set-current-layer drawing (dom:layer-id layer))
    (values (dom:layer-id layer) previous)))

(defun end-script-layer (app
                         layer-id
                         previous-id)
  "Take the layer made for a script away when the script left nothing on it, and go back to the layer that
was current.

APP: the application.
LAYER-ID: the layer made for the script.
PREVIOUS-ID: the layer that was current before it."
  (let ((drawing (app-drawing app)))
    (when (and (dom:find-layer drawing layer-id)
               (zerop (dom:layer-entity-count drawing layer-id)))
      (dom:remove-layer drawing layer-id)
      (when (dom:find-layer drawing previous-id)
        (dom:set-current-layer drawing previous-id)))))

;;; Running

(defun finish-script-run (app
                          path
                          text
                          target)
  "Run the script, as one command that can be undone, and write what came of it in the console, which opens
first so that the view the script fits is the one that stays.

APP: the application.
PATH: the script file, for messages.
TEXT: the source of the script.
TARGET: :layer to draw on a new layer of the drawing being edited, or :document when the drawing is already a
new one."
  (set-console-open app t)
  (multiple-value-bind (layer-id previous-id)
      (if (eq target :layer)
          (begin-script-layer app path)
          (values nil nil))
    (let ((result (run-source app
                              text
                              :timeout script:+script-timeout-seconds+
                              :event "script.run")))
      (when layer-id
        (end-script-layer app
                          layer-id
                          previous-id))
      (console-say app
                   :note
                   (i18n:trf "Script ~a" (file-namestring path)))
      (let ((output (script:script-result-output result)))
        (when (plusp (length output))
          (console-say app
                       :output
                       output)))
      (ecase (script:script-result-status result)
        (:ok
         (console-say app
                      :note
                      (i18n:trf "Done: ~d ~:*~[expressions~;expression~:;expressions~] ran."
                                (script:script-result-forms result)))
         (set-message app
                      (i18n:trf "Script ~a ran." (file-namestring path))))
        ((:error :timeout :incomplete)
         (console-say app
                      :error
                      (script:script-result-message result))
         (console-say app
                      :note
                      (i18n:tr "The script stopped, and the entities are as they were."))
         (show-notice app
                      (i18n:trf "~a did not finish" (file-namestring path))
                      (script:script-result-message result)
                      +report-message-lines+))))))

(defun run-script-in-new-drawing (app
                                  path
                                  text)
  "Start a new drawing, after offering to save the one being edited, and run the script in it.

APP: the application.
PATH: the script file.
TEXT: the source of the script."
  (confirm-discard app
                   (lambda ()
                     (reset-document app
                                     (dom:make-drawing)
                                     nil)
                     (finish-script-run app
                                        path
                                        text
                                        :document))))

(defun ask-where-to-run-script (app
                                path
                                text)
  "Ask where the script is to draw: on a new layer of this drawing, or in a new drawing. It is run when the
user answers.

APP: the application.
PATH: the script file.
TEXT: the source of the script."
  (open-choice-prompt app
                      (i18n:trf "Run ~a?" (file-namestring path))
                      (i18n:tr
                       "It can draw on a new layer of this drawing, or in a new drawing. Run only files you trust.")
                      (list (cons (i18n:tr "New layer")
                                  (lambda ()
                                    (finish-script-run app
                                                       path
                                                       text
                                                       :layer)))
                            (cons (i18n:tr "New drawing")
                                  (lambda ()
                                    (run-script-in-new-drawing app
                                                               path
                                                               text)))
                            (cons (i18n:tr "Cancel") nil))))

(defun read-script-file (path)
  "Return the text of a script file, signaling a file error that says why it cannot be run: it is too large,
cannot be read, is not made of complete expressions, or has none.

PATH: the script file."
  (let* ((size (if (eq :file (sbcl:file-kind path))
                   (sbcl:file-size-bytes path)
                   0))
         (text (progn
                 (when (> size +maximum-script-bytes+)
                   (error 'base:drawing-file-io-error
                          :path (uiop:native-namestring path)
                          :reason (i18n:trf "is larger than the ~d MiB limit for a script"
                                            (floor +maximum-script-bytes+ (* 1024 1024)))))
                 (persist:read-file-text path)))
         (count (script:count-source-forms text)))
    (cond ((null count)
           (error 'base:drawing-file-io-error
                  :path (uiop:native-namestring path)
                  :reason (i18n:tr "is not a script: it must be Lisp expressions with their parentheses closed")))
          ((zerop count)
           (error 'base:drawing-file-io-error
                  :path (uiop:native-namestring path)
                  :reason (i18n:tr "is empty: it has no expression to run")))
          (t text))))

(defun run-script-file-in-app (app
                               path-text)
  "Read the script file at PATH-TEXT and ask where it is to draw; it runs when the user answers. On failure
the reason is shown. Returns true when the question was asked.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (let* ((path (resolve-open-path path-text #'script-file-path))
               (text (read-script-file path)))
          (ask-where-to-run-script app
                                   path
                                   text)
          t)
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "script.read"
                             started
                             condition)))))

;;; Writing

(defun export-script-from-app (app
                               path-text)
  "Save the drawing as a script at PATH-TEXT (.lisp is added to a name without an extension) and tell the user
how it went in a modal. On failure the previous file is untouched. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (multiple-value-bind (saved stats)
            (persist:save-text-file (script-file-path path-text)
                                    (lambda (stream)
                                      (write-string (script:drawing-script (app-drawing app)) stream)))
          (obs:record-operation-result (app-telemetry app)
                                       "file.export-script"
                                       (- (sbcl:monotonic-nanoseconds) started)
                                       :info
                                       :bytes (getf stats :bytes)
                                       :entity-count (dom:drawing-entity-count (app-drawing app))
                                       :result :success)
          (show-notice app
                       (i18n:tr "Script exported")
                       (i18n:trf "~a was saved (~a)."
                                 (file-namestring saved)
                                 (base:format-byte-size (getf stats :bytes))))
          t)
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-export-operation app
                               "file.export-script"
                               started
                               condition)))))

(defun operation-run-script (app)
  "Open the dialog where the user finds a script to run.

APP: the application."
  (open-file-dialog app
                    :open
                    nil
                    :script))

(defun operation-export-script (app)
  "Open the dialog where the user picks a folder and a name, and export the drawing there as a script.

APP: the application."
  (open-file-dialog app
                    :save
                    nil
                    :script))
