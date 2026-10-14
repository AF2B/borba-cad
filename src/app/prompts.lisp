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

(defun open-choice-prompt (app
                           title
                           message
                           choices)
  "Open a modal prompt with a row of buttons. Each function runs after the prompt closes.

APP: the application.
TITLE: heading.
MESSAGE: explanation under the heading.
CHOICES: list of (label . function-or-nil); the first is chosen by Enter, the last by Escape."
  (setf (app-prompt app) (make-prompt :choice
                                      title
                                      message
                                      nil
                                      choices)
        (app-menu app) nil
        (app-focus app) :prompt)
  (app-request-redraw app))

(defun close-prompt (app)
  "Close the modal prompt without running anything.

APP: the application."
  (setf (app-prompt app) nil
        (app-focus app) nil
        (app-edit app) nil)
  (app-request-redraw app))

(defun choose-prompt-button (app
                             index)
  "Close the prompt and run the function of its INDEX-th button.

APP: the application.
INDEX: position in the prompt's choice list."
  (let* ((prompt (app-prompt app))
         (choice (nth index (prompt-choices prompt)))
         (typed (and (eq (prompt-kind prompt) :text)
                     (ui:edit-state-text (prompt-edit prompt)))))
    (close-prompt app)
    (when (and choice (cdr choice))
      (if (eq (prompt-kind prompt) :text)
          (funcall (cdr choice) typed)
          (funcall (cdr choice))))))

(defun accept-prompt (app)
  "Press the first button of the prompt (OK for text prompts).

APP: the application."
  (choose-prompt-button app 0))

(defun cancel-prompt (app)
  "Press the last button of the prompt (Cancel); close it when that button does nothing.

APP: the application."
  (choose-prompt-button app (1- (length (prompt-choices (app-prompt app))))))
