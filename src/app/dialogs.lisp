(in-package #:borba-cad/app)

;;;; The modal prompt and the assembly of every widget of the window, in drawing order.

(defconstant +prompt-width+ 560f0
  "Width of the modal prompt, in pixels.")

(defconstant +dialog-padding+ 20f0
  "Space between the edge of a dialog and its content, in pixels.")

(defconstant +dialog-title-height+ 28f0
  "Height of the title line of a dialog, in pixels.")

(defconstant +dialog-line-height+ 20f0
  "Height of one line of a dialog's message, in pixels.")

(defconstant +dialog-message-lines+ 3
  "Most lines the message of a dialog takes before it is cut with an ellipsis.")

(defconstant +dialog-control-height+ 32f0
  "Height of the field and the buttons of a dialog, in pixels.")

(defconstant +dialog-button-width+ 124f0
  "Width of a dialog button, in pixels.")

(defconstant +dialog-section-gap+ 14f0
  "Space between the parts of a dialog, in pixels.")

(defconstant +dialog-row-gap+ 8f0
  "Space between two lines of a form, in pixels.")

(defconstant +form-label-share+ 0.4f0
  "Share of the width of a form that its labels take; the fields have the rest.")

(defun prompt-height (message-lines
                      field-rows
                      info-p)
  "Return the height of the modal prompt.

MESSAGE-LINES: number of lines the message takes.
FIELD-ROWS: how many lines of text fields the prompt has: none, one for a text prompt, or those of a form.
INFO-P: true when there is a line under the fields that says what OK would do or what is wrong."
  (+ +dialog-padding+
     +dialog-title-height+
     (* message-lines +dialog-line-height+)
     (if (plusp field-rows)
         (+ +dialog-section-gap+
            (* field-rows +dialog-control-height+)
            (* (1- field-rows) +dialog-row-gap+))
         0f0)
     (if info-p
         (+ +dialog-row-gap+ +dialog-line-height+)
         0f0)
     +dialog-section-gap+
     +dialog-control-height+
     +dialog-padding+))

(defun form-info (app
                  prompt)
  "Return what the line under the fields of a form says, and true when it is a problem: the sentence the last OK was
refused with, or what OK would do with the values as they are.

APP: the application.
PROMPT: the :form prompt."
  (declare (ignore app))
  (cond ((prompt-problem prompt)
         (values (prompt-problem prompt) t))
        ((prompt-info prompt)
         (handler-case (funcall (prompt-info prompt) (form-values prompt))
           (base:borba-cad-error (condition)
             (values (princ-to-string condition) t))))))

(defun form-field-widgets (app
                           prompt
                           x
                           y
                           width)
  "Build the lines of a form: the label of each, and its field, which shows the caret when it has the keyboard.

APP: the application.
PROMPT: the :form prompt.
X: left edge of the lines.
Y: top edge of the first line.
WIDTH: width of the lines."
  (let* ((theme (app-theme app))
         (label-width (* width +form-label-share+))
         (field-x (+ x label-width))
         (field-width (- width label-width)))
    (loop for field in (prompt-fields prompt)
          for index from 0
          append (let ((index index)
                       (row-y (+ y (* index (+ +dialog-control-height+ +dialog-row-gap+))))
                       (active (= index (prompt-active prompt))))
                   (list (make-label (list :form-label index)
                                     (form-field-label field)
                                     x
                                     row-y
                                     (- label-width ui:+gap+)
                                     +dialog-control-height+
                                     :color (scene:theme-text-dim theme))
                         (if (form-field-options field)
                             (ui:make-widget :button
                                             (list :prompt-field index)
                                             field-x
                                             row-y
                                             field-width
                                             +dialog-control-height+
                                             :label (cdr (nth (form-field-choice field) (form-field-options field)))
                                             :active active
                                             :action (lambda () (cycle-form-option app
                                                                                   index
                                                                                   1)))
                             (ui:make-widget :field
                                             (list :prompt-field index)
                                             field-x
                                             row-y
                                             field-width
                                             +dialog-control-height+
                                             :label (ui:edit-state-text (form-field-edit field))
                                             :edit (and active (form-field-edit field))
                                             :align :left
                                             :action (lambda () (focus-form-field app index)))))))))

(defun prompt-buttons (app
                       prompt
                       x
                       y)
  "Build the buttons of the modal prompt, right-aligned on the card: the first one is the
primary action.

APP: the application.
PROMPT: the prompt.
X: right edge of the card.
Y: top edge of the buttons."
  (let* ((choices (prompt-choices prompt))
         (count (length choices))
         (total (+ (* count +dialog-button-width+) (* (1- count) ui:+gap+)))
         (start (- x +dialog-padding+ total)))
    (loop for (label) in choices
          for index from 0
          collect (let ((index index))
                    (ui:make-widget :button
                                    (list :prompt-button index)
                                    (+ start (* index (+ +dialog-button-width+ ui:+gap+)))
                                    y
                                    +dialog-button-width+
                                    +dialog-control-height+
                                    :label label
                                    :variant (if (zerop index) :primary :default)
                                    :action (lambda () (choose-prompt-button app index)))))))

(defun prompt-widgets (app)
  "Build the modal prompt: a veil over the window, the dialog card, its message, its field or form and its buttons.

APP: the application."
  (let* ((prompt (app-prompt app))
         (theme (app-theme app))
         (window-width (float (app-window-width app) 1f0))
         (window-height (float (app-window-height app) 1f0))
         (kind (prompt-kind prompt))
         (field-rows (case kind
                       (:text 1)
                       (:form (length (prompt-fields prompt)))
                       (t 0)))
         (inner (- +prompt-width+ (* 2 +dialog-padding+)))
         (lines (ui:wrap-text (prompt-message prompt)
                              inner
                              (or (prompt-message-lines prompt) +dialog-message-lines+)))
         (info (and (eq kind :form)
                    (multiple-value-list (form-info app prompt))))
         (height (prompt-height (length lines)
                                field-rows
                                (and (first info) t)))
         (x (/ (- window-width +prompt-width+) 2))
         (y (/ (- window-height height) 3))
         (content-x (+ x +dialog-padding+))
         (message-y (+ y +dialog-padding+ +dialog-title-height+))
         (field-y (+ message-y (* (length lines) +dialog-line-height+) +dialog-section-gap+))
         (info-y (+ field-y
                    (* field-rows +dialog-control-height+)
                    (* (max 0 (1- field-rows)) +dialog-row-gap+)
                    +dialog-row-gap+))
         (buttons-y (- (+ y height) +dialog-padding+ +dialog-control-height+)))
    (append (list (ui:make-widget :scrim
                                  :scrim
                                  0f0
                                  0f0
                                  window-width
                                  window-height)
                  (ui:make-widget :card
                                  :prompt
                                  x
                                  y
                                  +prompt-width+
                                  height)
                  (make-label :prompt-title
                              (prompt-title prompt)
                              content-x
                              (+ y +dialog-padding+)
                              inner
                              +dialog-title-height+
                              :font :ui-strong
                              :size ui:+heading-font-pixels+
                              :color (scene:theme-text theme)))
            (loop for line in lines
                  for index from 0
                  collect (make-label (if (zerop index)
                                          :prompt-message
                                          (list :prompt-message index))
                                      line
                                      content-x
                                      (+ message-y (* index +dialog-line-height+))
                                      inner
                                      +dialog-line-height+
                                      :color (scene:theme-text-dim theme)))
            (when (eq kind :text)
              (list (ui:make-widget :field
                                    :prompt-field
                                    content-x
                                    field-y
                                    inner
                                    +dialog-control-height+
                                    :label (ui:edit-state-text (prompt-edit prompt))
                                    :edit (prompt-edit prompt))))
            (when (eq kind :form)
              (form-field-widgets app
                                  prompt
                                  content-x
                                  field-y
                                  inner))
            (when (first info)
              (list (make-label :prompt-info
                                (first info)
                                content-x
                                info-y
                                inner
                                +dialog-line-height+
                                :color (if (second info)
                                           (scene:theme-danger theme)
                                           (scene:theme-accent theme)))))
            (prompt-buttons app
                            prompt
                            (+ x +prompt-width+)
                            buttons-y))))

(defun build-widgets (app)
  "Build every widget of the window, in drawing order, from the application state: the side
panel, the title bar, the tool palette, the status bar, the tooltip of the widget under the
pointer, the open menu, the file dialog when one is open, and the modal prompt when one is.

APP: the application."
  (let* ((layout (app-layout app))
         (side (ui:layout-side-panel layout))
         (blocked (and (or (app-prompt app) (app-dialog app) (app-menu app) (app-welcome app)) t))
         (dialog-widgets (and (app-dialog app)
                              (file-dialog-widgets app (and (app-prompt app) t))))
         (base-widgets (append (and (not (app-welcome app))
                                    (empty-hint-widgets app (ui:layout-canvas layout)))
                               (list (ui:make-widget :panel
                                                     :side-panel
                                                     (ui:rect-x side)
                                                     (ui:rect-y side)
                                                     (ui:rect-width side)
                                                     (ui:rect-height side)
                                                     :variant :side))
                               (layers-panel-widgets app
                                                     (ui:layout-layers-panel layout)
                                                     blocked)
                               (inspector-widgets app
                                                  (ui:layout-inspector-panel layout)
                                                  blocked)
                               (zoom-widgets app
                                             (ui:layout-canvas layout)
                                             blocked)
                               (palette-widgets app
                                                (ui:layout-tool-palette layout)
                                                blocked)
                               (console-widgets app blocked)
                               (coordinate-input-widgets app)
                               (toolbar-widgets app blocked)))
         (hovered (ui:widget-at (append base-widgets
                                        dialog-widgets
                                        (status-widgets app
                                                        nil
                                                        blocked))
                                (app-pointer-x app)
                                (app-pointer-y app)))
         (all (append base-widgets
                      (status-widgets app
                                      (and hovered (hint-with-shortcut hovered))
                                      blocked)
                      dialog-widgets
                      (let ((tooltip (tooltip-widget app hovered)))
                        (and tooltip (list tooltip)))
                      (menu-widgets app base-widgets)
                      (welcome-widgets app))))
    (if (app-prompt app)
        (append all (prompt-widgets app))
        all)))
