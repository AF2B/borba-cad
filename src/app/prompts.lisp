(in-package #:borba-cad/app)

(defconstant +prompt-maximum-text-length+ 512
  "Longest text a prompt field accepts; long enough for file paths.")

(defun open-text-prompt (app
                         title
                         message
                         initial
                         on-accept)
  "Open a modal prompt with a text field, OK and Cancel. ON-ACCEPT runs after the prompt closes.

APP: the application.
TITLE: heading.
MESSAGE: explanation under the heading.
INITIAL: text the field starts with, selected so typing replaces it.
ON-ACCEPT: function of one argument, the typed string."
  (setf (app-prompt app)
        (make-prompt :text
                     title
                     message
                     (ui:edit-select-all-replace
                      (ui:make-edit-state :text initial :maximum-length +prompt-maximum-text-length+))
                     (list (cons (i18n:tr "OK") on-accept) (cons (i18n:tr "Cancel") nil)))
        (app-menu app) nil
        (app-focus app) :prompt)
  (app-request-redraw app))

(defun form-setting (app
                     key
                     default)
  "Return what was last chosen or typed for a line of a form, or DEFAULT.

APP: the application.
KEY: the key of the line.
DEFAULT: what to give when nothing was kept."
  (let ((kept (assoc key (app-form-settings app))))
    (if kept
        (cdr kept)
        default)))

(defun remember-form-settings (app
                               values)
  "Keep what was chosen or typed in a form, as the starting values of the next one.

APP: the application.
VALUES: the alist of the form."
  (setf (app-form-settings app)
        (append values
                (remove-if (lambda (kept) (assoc (car kept) values))
                           (app-form-settings app)))))

(defun form-value (values
                   key)
  "Return the value of one line of a form.

VALUES: the alist of the form.
KEY: the key of the line."
  (cdr (assoc key values)))

(defun make-form-line (spec
                       first-p)
  "Make the line of a form that SPEC describes.

SPEC: (key label initial) for a line with a text field, or (key label initial-key options) for a line of options.
FIRST-P: true for the first line, which has the keyboard and its text selected."
  (destructuring-bind (key label initial &optional options) spec
    (if options
        (make-form-field key
                         label
                         nil
                         options
                         (or (position initial options :key #'car) 0))
        (let ((edit (ui:make-edit-state :text initial
                                        :maximum-length +prompt-maximum-text-length+)))
          (make-form-field key
                           label
                           (if first-p
                               (ui:edit-select-all-replace edit)
                               edit))))))

(defun open-form-prompt (app
                         title
                         message
                         fields
                         on-accept
                         &key info)
  "Open a modal form: a line for each of FIELDS, with a text field or a few options, then OK and Cancel. Tab and
Shift+Tab (or the up and down arrows) move between the lines, Enter is OK. ON-ACCEPT runs when OK is pressed; when it
returns a sentence the form stays open and shows it.

APP: the application.
TITLE: heading.
MESSAGE: explanation under the heading.
FIELDS: list of (key label initial) for a text field, or (key label initial-key options) for a choice, where OPTIONS
is a list of (key . label); the key is what the value is given under, the label what the line asks for, and the
initial value the text the field starts with or the key of the option that starts chosen.
ON-ACCEPT: function of an alist of (key . value), in the order of the fields, where the value is the typed string or
the key of the chosen option; it returns NIL to close the form or a sentence to keep it open.
INFO: function of the same alist that returns a line saying what OK would do, and true as a second value when the
values cannot be used; or NIL."
  (let ((prompt (make-prompt :form
                             title
                             message
                             nil
                             (list (cons (i18n:tr "OK") on-accept)
                                   (cons (i18n:tr "Cancel") nil)))))
    (setf (prompt-fields prompt)
          (loop for spec in fields
                for index from 0
                collect (make-form-line spec (zerop index)))
          (prompt-info prompt) info
          (app-prompt app) prompt
          (app-menu app) nil
          (app-focus app) :prompt)
    (app-request-redraw app)))

(defun form-field-value (field)
  "Return what a line of a form holds: the typed text, or the key of the chosen option.

FIELD: a form field."
  (if (form-field-options field)
      (car (nth (form-field-choice field) (form-field-options field)))
      (ui:edit-state-text (form-field-edit field))))

(defun open-report-prompt (app
                           title
                           message
                           report
                           choices)
  "Open a modal that shows a table of quantities, to read and scroll, with a row of buttons.

APP: the application.
TITLE: heading.
MESSAGE: explanation under the heading.
REPORT: the QUANTITY-REPORT that is shown.
CHOICES: list of (label . function-or-nil), the buttons; the first is chosen by Enter and the last by Escape."
  (let ((prompt (make-prompt :report
                             title
                             message
                             nil
                             choices)))
    (setf (prompt-report prompt) report
          (app-prompt app) prompt
          (app-menu app) nil
          (app-focus app) :prompt)
    (app-request-redraw app)))

(defun form-values (prompt)
  "Return what a form holds, as an alist of the key of each line and its value, top to bottom: the typed text of a line
with a text field, the key of the chosen option of a line of options.

PROMPT: a :form prompt."
  (loop for field in (prompt-fields prompt)
        collect (cons (form-field-key field)
                      (form-field-value field))))

(defun prompt-current-edit (prompt)
  "Return the edit state that typing goes to: that of a :text prompt or of the search of a :palette, or of the active
line of a :form; NIL for a prompt that takes no text.

PROMPT: the prompt."
  (case (prompt-kind prompt)
    ((:text :palette) (prompt-edit prompt))
    (:form (form-field-edit (nth (prompt-active prompt) (prompt-fields prompt))))))

(defun (setf prompt-current-edit) (edit
                                   prompt)
  "Replace the edit state that typing goes to, and forget the sentence a refused OK left.

EDIT: the new edit state.
PROMPT: the prompt."
  (ecase (prompt-kind prompt)
    (:text (setf (prompt-edit prompt) edit))
    (:palette (setf (prompt-edit prompt) edit
                    (prompt-active prompt) 0
                    (prompt-scroll prompt) 0))
    (:form (setf (form-field-edit (nth (prompt-active prompt) (prompt-fields prompt))) edit
                 (prompt-problem prompt) nil)))
  edit)

(defun focus-form-field (app
                         index)
  "Give the keyboard to a line of the open form, with its text selected so that typing replaces it.

APP: the application.
INDEX: position of the line, from 0."
  (let* ((prompt (app-prompt app))
         (fields (prompt-fields prompt)))
    (when (and (< -1 index (length fields)) (/= index (prompt-active prompt)))
      (let ((field (nth index fields)))
        (setf (prompt-active prompt) index)
        (when (form-field-edit field)
          (setf (form-field-edit field) (ui:edit-select-all-replace (form-field-edit field))))))
    (app-request-redraw app)))

(defun move-form-focus (app
                        step)
  "Move the keyboard to the line STEP lines below the one that has it, going round at the ends.

APP: the application.
STEP: 1 for the next line, -1 for the previous."
  (let* ((prompt (app-prompt app))
         (count (length (prompt-fields prompt))))
    (focus-form-field app (mod (+ (prompt-active prompt) step) count))))

(defun cycle-form-option (app
                          index
                          step)
  "Choose the option STEP places along in a line of options, going round at the ends, and give the line the keyboard.

APP: the application.
INDEX: position of the line, from 0.
STEP: 1 for the next option, -1 for the previous."
  (let* ((prompt (app-prompt app))
         (field (nth index (prompt-fields prompt))))
    (focus-form-field app index)
    (when (form-field-options field)
      (setf (form-field-choice field) (mod (+ (form-field-choice field) step)
                                           (length (form-field-options field)))
            (prompt-problem prompt) nil))
    (app-request-redraw app)))

(defun open-choice-prompt (app
                           title
                           message
                           choices
                           &optional message-lines)
  "Open a modal prompt with a row of buttons. Each function runs after the prompt closes.

APP: the application.
TITLE: heading.
MESSAGE: explanation under the heading.
CHOICES: list of (label . function-or-nil); the first is chosen by Enter, the last by Escape.
MESSAGE-LINES: how many lines the message may take, or NIL for the usual three."
  (setf (app-prompt app) (make-prompt :choice
                                      title
                                      message
                                      nil
                                      choices
                                      message-lines)
        (app-menu app) nil
        (app-focus app) :prompt)
  (app-request-redraw app))

(defun show-notice (app
                    title
                    message
                    &optional message-lines)
  "Tell the user something in a modal in the middle of the window, with an OK button: what a long operation
came to, which a line of the status bar is too easy to miss.

APP: the application.
TITLE: heading.
MESSAGE: what happened.
MESSAGE-LINES: how many lines the message may take, or NIL for the usual three."
  (open-choice-prompt app
                      title
                      message
                      (list (cons (i18n:tr "OK") nil))
                      message-lines))

(defun close-prompt (app)
  "Close the modal prompt without running anything.

APP: the application."
  (setf (app-prompt app) nil
        (app-focus app) nil
        (app-edit app) nil)
  (app-request-redraw app))

(defun accept-form (app
                    prompt
                    on-accept)
  "Give the values of a form to ON-ACCEPT. The form closes unless it answers with a sentence, which it shows.

APP: the application.
PROMPT: the :form prompt.
ON-ACCEPT: the accept function of the form."
  (let ((problem (funcall on-accept (form-values prompt))))
    (cond ((stringp problem)
           (setf (prompt-problem prompt) problem)
           (app-request-redraw app))
          ((eq (app-prompt app) prompt)
           (close-prompt app)))))

(defun choose-prompt-button (app
                             index)
  "Close the prompt and run the function of its INDEX-th button. A form is not closed by an OK that it refuses.

APP: the application.
INDEX: position in the prompt's choice list."
  (let* ((prompt (app-prompt app))
         (choice (nth index (prompt-choices prompt)))
         (kind (prompt-kind prompt)))
    (if (and (eq kind :form) choice (cdr choice))
        (accept-form app
                     prompt
                     (cdr choice))
        (let ((typed (and (eq kind :text)
                          (ui:edit-state-text (prompt-edit prompt)))))
          (close-prompt app)
          (when (and choice (cdr choice))
            (if (eq kind :text)
                (funcall (cdr choice) typed)
                (funcall (cdr choice))))))))

(defun accept-prompt (app)
  "Press the first button of the prompt (OK for text prompts).

APP: the application."
  (choose-prompt-button app 0))

(defun cancel-prompt (app)
  "Press the last button of the prompt (Cancel); close it when that button does nothing or there are none.

APP: the application."
  (let ((count (length (prompt-choices (app-prompt app)))))
    (if (zerop count)
        (close-prompt app)
        (choose-prompt-button app (1- count)))))
