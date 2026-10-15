(in-package #:borba-cad/app)

;;;; Running a script file on the drawing and writing the drawing as a script. A script is a program, so it
;;;; is never run without the user choosing the file and answering a question that says what it is; what
;;;; it does is one step of undo, and it is stopped when it takes more than a minute.

(defparameter *script-extensions* (list "lisp")
  "Extensions of the files that the dialog lists when it is about scripts.")

(defconstant +maximum-script-bytes+ (* 2 1024 1024)
  "Largest script file that will be read.")

(defconstant +script-preview-lines+ 3
  "How many lines of a script the question before it runs shows.")

(defconstant +script-preview-columns+ 60
  "How many characters of each of those lines are shown.")

(defconstant +script-prompt-lines+ 8
  "Lines the question before a script runs may take.")

(defun script-file-path (text)
  "Return the pathname of the script file that TEXT names, with the extension .lisp added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    (first *script-extensions*)
                                    *script-extensions*))

(defun script-preview (text)
  "Return the first lines of the code of a script, joined, each cut short, for the question before it runs.

TEXT: the source of the script."
  (let* ((lines (remove-if (lambda (line)
                             (zerop (length line)))
                           (mapcar (lambda (line)
                                     (string-trim '(#\Space #\Tab) line))
                                   (split-output-lines (script:strip-comments text)))))
         (shown (subseq lines 0 (min +script-preview-lines+ (length lines)))))
    (format nil "~{~a~^ | ~}"
            (mapcar (lambda (line)
                      (if (> (length line) +script-preview-columns+)
                          (concatenate 'string (subseq line 0 +script-preview-columns+) "...")
                          line))
                    shown))))

(defun script-question (path
                        count
                        text)
  "Return what the question before a script runs says: what the script is, that it is a program, and how
it runs.

PATH: the script file.
COUNT: how many expressions it holds.
TEXT: the source of the script."
  (format nil "~a ~a ~a"
          (i18n:trf "~a holds ~d ~:*~[expressions~;expression~:;expressions~] and begins: ~a."
                    (file-namestring path)
                    count
                    (script-preview text))
          (i18n:tr "A script is a program: it can do anything the program can, and it changes the drawing.")
          (i18n:trf "Run only scripts you trust. It runs as one step you can undo, and stops after ~d seconds."
                    (round script:+script-timeout-seconds+))))

(defun finish-script-run (app
                          path
                          text)
  "Run the script, as one command that can be undone, and write what came of it in the console, which opens
first so that the view the script fits is the one that stays.

APP: the application.
PATH: the script file, for messages.
TEXT: the source of the script."
  (set-console-open app t)
  (let ((result (run-source app
                            text
                            :timeout script:+script-timeout-seconds+
                            :event "script.run")))
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
                              (script:script-result-forms result))))
      ((:error :timeout :incomplete)
       (console-say app
                    :error
                    (script:script-result-message result))
       (console-say app
                    :note
                    (i18n:tr "The script stopped, and the entities are as they were."))))
    (when (eq (script:script-result-status result) :ok)
      (set-message app
                   (i18n:trf "Script ~a ran." (file-namestring path))))))

(defun run-script-file-in-app (app
                               path-text)
  "Read the script file at PATH-TEXT and ask whether to run it, saying what it is and warning that it is a
program. It runs when the user says so. On failure the reason is shown. Returns true when the question was
asked.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (let* ((path (resolve-open-path path-text #'script-file-path))
               (size (if (eq :file (sbcl:file-kind path))
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
                (t
                 (open-choice-prompt app
                                     (i18n:trf "Run ~a?" (file-namestring path))
                                     (script-question path
                                                      count
                                                      text)
                                     (list (cons (i18n:tr "Run script")
                                                 (lambda ()
                                                   (finish-script-run app
                                                                      path
                                                                      text)))
                                           (cons (i18n:tr "Cancel") nil))
                                     +script-prompt-lines+)
                 t)))
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "script.read"
                             started
                             condition)))))

(defun export-script-from-app (app
                               path-text)
  "Save the drawing as a script at PATH-TEXT (.lisp is added to a name without an extension). On failure the
previous file is untouched and the reason is shown. Returns true on success.

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
          (set-message app
                       (i18n:trf "Exported ~a (~a) as a script."
                                 (file-namestring saved)
                                 (base:format-byte-size (getf stats :bytes))))
          t)
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "file.export-script"
                             started
                             condition)))))

(defun operation-run-script (app)
  "Open the dialog where the user finds a script to run on the drawing.

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
