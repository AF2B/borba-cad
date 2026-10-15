(in-package #:borba-cad/script)

;;;; Evaluating what the user types or a script file holds: reading the forms, running them one after the
;;;; other in the user package with the context bound, writing down what they print and return, and
;;;; stopping a command that takes too long. A command that fails leaves the drawing as it was.

(defconstant +default-timeout-seconds+ 10d0
  "Longest time a command typed in the console may run.")

(defconstant +script-timeout-seconds+ 60d0
  "Longest time a script file may run.")

(defconstant +printed-length-limit+ 200
  "Most elements of a list that a printed result shows.")

(defconstant +printed-depth-limit+ 8
  "Most levels of lists that a printed result shows.")

(defconstant +maximum-output-characters+ 200000
  "Most characters a command may print, after which its output is cut.")

(defstruct (script-result (:constructor make-script-result (status
                                                            &key forms
                                                                 values
                                                                 output
                                                                 message)))
  "What came of running some text.

STATUS: :ok, :error (the text is wrong or a command failed, and nothing was changed), :timeout (it took too
long, and nothing was changed) or :incomplete (the text ends inside a form).
FORMS: how many forms were run.
VALUES: for each form run, what it returned, printed, in order.
OUTPUT: what the commands printed.
MESSAGE: for :error and :timeout, what went wrong."
  status
  (forms 0 :type fixnum)
  (values nil :type list)
  (output "" :type string)
  message)

(defun user-package ()
  "Return the package that what the user types is read in."
  (find-package :borba-cad/user))

(defun strip-comments (text)
  "Return TEXT with the Lisp comments taken out, from a semicolon to the end of its line, but not the
semicolons inside strings or written as the character #\;.

TEXT: Lisp source."
  (with-output-to-string (result)
    (let ((in-string nil)
          (escaped nil)
          (index 0)
          (length (length text)))
      (loop while (< index length)
            do (let ((character (char text index)))
                 (cond (escaped
                        (write-char character result)
                        (setf escaped nil))
                       ((and in-string (char= character #\\))
                        (write-char character result)
                        (setf escaped t))
                       ((char= character #\")
                        (write-char character result)
                        (setf in-string (not in-string)))
                       ((and (not in-string) (char= character #\#) (< (1+ index) length)
                             (char= (char text (1+ index)) #\\))
                        (write-string (subseq text index (min length (+ index 3))) result)
                        (incf index 2))
                       ((and (not in-string) (char= character #\;))
                        (loop while (and (< index length) (char/= (char text index) #\Newline))
                              do (incf index))
                        (decf index))
                       (t (write-char character result)))
                 (incf index))))))

(defmacro with-reading-settings (&body body)
  "Run BODY the way the user's text is read: in the user package, with numbers as double-floats, and
without the reader evaluating anything.

BODY: the forms to run."
  `(let ((*package* (user-package))
         (*read-default-float-format* 'double-float)
         (*read-eval* nil))
     ,@body))

(defun read-source-forms (text)
  "Return the forms in TEXT, read in the user package. Signals END-OF-FILE when the text ends inside a form,
and READER-ERROR when it is not Lisp. Nothing is evaluated.

TEXT: Lisp source."
  (with-reading-settings
    (with-input-from-string (stream text)
      (loop for form = (read stream nil stream)
            until (eq form stream)
            collect form))))

(defun count-source-forms (text)
  "Return how many forms TEXT holds, or NIL when it cannot be read.

TEXT: Lisp source."
  (handler-case (length (read-source-forms text))
    (error ()
      nil)))

(defun printed (value)
  "Return a value as the console shows it: printed readably, but cut where it would be very long and safe
for a structure that contains itself.

VALUE: any object."
  (let ((*print-length* +printed-length-limit+)
        (*print-level* +printed-depth-limit+)
        (*print-circle* t)
        (*print-pretty* nil)
        (*print-readably* nil))
    (prin1-to-string value)))

(defun limited-output (stream)
  "Return what a limited stream kept, with an ellipsis on a line of its own when it dropped the rest.

STREAM: the limited output stream the command wrote to."
  (multiple-value-bind (text truncated) (sbcl:limited-output-text stream)
    (if truncated
        (format nil "~a~%..." text)
        text)))

(defun run-forms (forms)
  "Run FORMS in turn and return the printed value of each, as a list.

FORMS: Lisp forms, read."
  (loop for form in forms
        collect (let ((values (multiple-value-list (eval form))))
                  (format nil "~{~a~^ ~}" (mapcar #'printed values)))))

(defun failure-message (condition)
  "Return what went wrong, as one line of text.

CONDITION: the condition that stopped a command."
  (substitute #\Space #\Newline (princ-to-string condition)))

(defun evaluate-source (context
                        text
                        &key (timeout +default-timeout-seconds+))
  "Read the forms in TEXT and run them one after the other against the drawing of CONTEXT, as a single
command: if one fails, or the time runs out, the entities it changed are put back as they were; layers and
settings are not part of that, as in the layers panel. Returns a SCRIPT-RESULT. The context begins by
forgetting what the command before it did, and ends holding what this one did, as its net changes, for the
caller to record.

CONTEXT: the script context to run in.
TEXT: Lisp source, read in the user package.
TIMEOUT: the longest time to run, in seconds."
  (let ((forms (handler-case (read-source-forms text)
                 (end-of-file ()
                   (return-from evaluate-source
                     (make-script-result :incomplete
                                         :message (i18n:tr "The expression is not finished: a parenthesis is open."))))
                 (reader-error (condition)
                   (return-from evaluate-source
                     (make-script-result :error
                                         :message (failure-message condition)))))))
    (context-reset context)
    (let ((output (sbcl:make-limited-output-stream +maximum-output-characters+)))
      (flet ((failed (status message)
               (context-rollback context)
               (make-script-result status
                                   :forms (length forms)
                                   :output (limited-output output)
                                   :message message)))
        (handler-case
            (let ((values
                    (let ((*context* context)
                          (*standard-output* output)
                          (*error-output* output)
                          (*standard-input* (make-string-input-stream ""))
                          (*package* (user-package))
                          (*read-default-float-format* 'double-float)
                          (*read-eval* nil))
                      (sbcl:call-with-timeout timeout
                                              (lambda () (run-forms forms))))))
              (make-script-result :ok
                                  :forms (length forms)
                                  :values values
                                  :output (limited-output output)))
          (sbcl:timeout ()
            (failed :timeout
                    (i18n:trf "It took more than ~d seconds, so it was stopped and nothing was changed."
                              (round timeout))))
          (storage-condition ()
            (failed :error (i18n:tr "It ran out of memory, so it was stopped and nothing was changed.")))
          (error (condition)
            (failed :error (failure-message condition))))))))
