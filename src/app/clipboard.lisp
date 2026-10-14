(in-package #:borba-cad/app)

;;;; The clipboard, and the editing keys that use it. The system clipboard is reached through two
;;;; functions that the window program installs; where there are none (the tests, a program without a
;;;; window) the application keeps the text itself.

(defun clipboard-text (app)
  "Return the text on the clipboard, or NIL when there is none.

APP: the application."
  (let ((reader (app-clipboard-reader app)))
    (if reader
        (funcall reader)
        (app-clipboard-fallback app))))

(defun set-clipboard-text (app
                           text)
  "Put TEXT on the clipboard.

APP: the application.
TEXT: the string to copy."
  (let ((writer (app-clipboard-writer app)))
    (if writer
        (funcall writer text)
        (setf (app-clipboard-fallback app) text))))

(defparameter *line-break-characters* (list #\Newline #\Return #\Tab)
  "Characters that a one-line field turns into a space when text is pasted into it.")

(defun single-line (text)
  "Return TEXT as a line to paste into a one-line field: line breaks and tabs become spaces, a run
of them becomes one, and the ends are trimmed.

TEXT: a string that may have several lines."
  (let ((result (make-string-output-stream))
        (in-break nil))
    (loop for character across text
          do (cond ((member character *line-break-characters*)
                    (setf in-break t))
                   (t
                    (when in-break
                      (write-char #\Space result)
                      (setf in-break nil))
                    (write-char character result))))
    (string-trim " " (get-output-stream-string result))))

(defun edit-with-clipboard (app
                            edit
                            key
                            modifiers)
  "Apply a key to the text of a field: the editing keys, and with Control A (select all), C (copy
the text), X (cut it) and V (paste at the caret, over the text when it is selected). Returns the
new edit state, or NIL when the key does nothing to a field.

APP: the application.
EDIT: the field's edit state.
KEY: key keyword.
MODIFIERS: modifier keys held."
  (if (modifier-p modifiers :control)
      (case key
        (:a (ui:edit-select-all-replace edit))
        (:c (set-clipboard-text app (ui:edit-state-text edit))
         edit)
        (:x (set-clipboard-text app (ui:edit-state-text edit))
         (ui:edit-backspace (ui:edit-select-all-replace edit)))
        (:v (let ((text (clipboard-text app)))
              (and text
                   (ui:edit-insert edit (single-line text)))))
        (t nil))
      (ui:edit-handle-key edit key)))
