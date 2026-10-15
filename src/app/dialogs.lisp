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

(defun prompt-height (message-lines
                      text-p)
  "Return the height of the modal prompt.

MESSAGE-LINES: number of lines the message takes.
TEXT-P: true when the prompt has a text field."
  (+ +dialog-padding+
     +dialog-title-height+
     (* message-lines +dialog-line-height+)
     (if text-p
         (+ +dialog-section-gap+ +dialog-control-height+)
         0f0)
     +dialog-section-gap+
     +dialog-control-height+
     +dialog-padding+))

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
  "Build the modal prompt: a veil over the window, the dialog card, its message, its field and
its buttons.

APP: the application."
  (let* ((prompt (app-prompt app))
         (theme (app-theme app))
         (window-width (float (app-window-width app) 1f0))
         (window-height (float (app-window-height app) 1f0))
         (text-p (eq (prompt-kind prompt) :text))
         (inner (- +prompt-width+ (* 2 +dialog-padding+)))
         (lines (ui:wrap-text (prompt-message prompt)
                              inner
                              (or (prompt-message-lines prompt) +dialog-message-lines+)))
         (height (prompt-height (length lines) text-p))
         (x (/ (- window-width +prompt-width+) 2))
         (y (/ (- window-height height) 3))
         (content-x (+ x +dialog-padding+))
         (message-y (+ y +dialog-padding+ +dialog-title-height+))
         (field-y (+ message-y (* (length lines) +dialog-line-height+) +dialog-section-gap+))
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
            (when text-p
              (list (ui:make-widget :field
                                    :prompt-field
                                    content-x
                                    field-y
                                    inner
                                    +dialog-control-height+
                                    :label (ui:edit-state-text (prompt-edit prompt))
                                    :edit (prompt-edit prompt))))
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
         (blocked (and (or (app-prompt app) (app-dialog app) (app-menu app)) t))
         (dialog-widgets (and (app-dialog app)
                              (file-dialog-widgets app (and (app-prompt app) t))))
         (base-widgets (append (empty-hint-widgets app (ui:layout-canvas layout))
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
                               (palette-widgets app
                                                (ui:layout-tool-palette layout)
                                                blocked)
                               (console-widgets app blocked)
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
                      (menu-widgets app base-widgets))))
    (if (app-prompt app)
        (append all (prompt-widgets app))
        all)))
