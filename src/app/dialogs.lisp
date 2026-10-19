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

(defconstant +report-width+ 780f0
  "Width of the modal that shows a table, in pixels, when the window is wide enough.")

(defconstant +report-row-height+ 26f0
  "Height of one row of a table, in pixels.")

(defconstant +report-maximum-rows+ 14
  "Most rows of a table the modal shows at once; the wheel and the keys bring the others.")

(defconstant +report-minimum-rows+ 3
  "Fewest rows of a table the modal shows, however small the window is.")

(defconstant +report-first-column-share+ 0.36f0
  "Share of the width of a table that its first column, the one with the names, takes.")

(defconstant +report-cell-gap+ 14f0
  "Space, in pixels, between the text of a cell of a table and the next column.")

(defconstant +report-window-margin+ 120f0
  "Space, in pixels, that a table leaves above and below its card in a window too small for all its rows.")

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

(defun report-capacity (app
                        prompt
                        message-lines)
  "Return how many rows of the table of a report fit in the window: all of them up to a number, and at least a few.

APP: the application.
PROMPT: the :report prompt.
MESSAGE-LINES: how many lines the message takes."
  (let* ((report (prompt-report prompt))
         (extras (+ +dialog-padding+
                    +dialog-title-height+
                    (* message-lines +dialog-line-height+)
                    +dialog-section-gap+
                    +report-row-height+
                    (if (dom:quantity-report-totals report) +report-row-height+ 0f0)
                    +dialog-section-gap+
                    +dialog-control-height+
                    +dialog-padding+))
         (room (floor (- (float (app-window-height app) 1f0) extras +report-window-margin+)
                      +report-row-height+)))
    (max +report-minimum-rows+
         (min (length (dom:quantity-report-rows report))
              +report-maximum-rows+
              room))))

(defun scroll-report (app
                      step)
  "Scroll the table of the open report by STEP rows, as far as there are rows.

APP: the application.
STEP: how many rows, positive to go down."
  (let* ((prompt (app-prompt app))
         (rows (length (dom:quantity-report-rows (prompt-report prompt))))
         (capacity (report-capacity app
                                    prompt
                                    (length (report-message-lines app prompt)))))
    (setf (prompt-scroll prompt) (max 0 (min (max 0 (- rows capacity))
                                             (+ (prompt-scroll prompt) step))))
    (app-request-redraw app)))

(defun report-card-width (app)
  "Return the width of the card that shows a table: the usual one, or the window less its margins when that is less.

APP: the application."
  (min +report-width+ (- (float (app-window-width app) 1f0) (* 2 +dialog-padding+))))

(defun report-message-lines (app
                             prompt)
  "Return the lines the message of a report is broken into.

APP: the application.
PROMPT: the :report prompt."
  (ui:wrap-text (prompt-message prompt)
                (- (report-card-width app) (* 2 +dialog-padding+))
                +dialog-message-lines+))

(defun report-cell-widgets (app
                            report
                            row
                            x
                            y
                            widths
                            id
                            &key strong)
  "Build the cells of one row of a table.

APP: the application.
REPORT: the report.
ROW: the values of the row, one for each column.
X: left edge of the row.
Y: top edge of the row.
WIDTHS: the width of each column.
ID: what the widgets are called, followed by the position of the column.
STRONG: true for the heading and the totals, which are in bold."
  (let ((theme (app-theme app))
        (unit (display-unit-of app))
        (left x))
    (loop for value in row
          for (nil . type) in (dom:quantity-report-columns report)
          for width in widths
          for index from 0
          collect (prog1 (make-label (list id index)
                                     (if (stringp value)
                                         value
                                         (dom:quantity-cell-text value
                                                                 type
                                                                 unit))
                                     left
                                     y
                                     (- width +report-cell-gap+)
                                     +report-row-height+
                                     :align (if (eq type :text) :left :right)
                                     :font (if strong :ui-strong :ui)
                                     :color (if strong
                                                (scene:theme-text theme)
                                                (scene:theme-text-dim theme)))
                    (incf left width)))))

(defun report-widgets (app)
  "Build the modal that shows a table: a veil over the window, the card, its title and message, the headings, the rows
that are in view, the totals and the buttons.

APP: the application."
  (let* ((prompt (app-prompt app))
         (theme (app-theme app))
         (report (prompt-report prompt))
         (unit (display-unit-of app))
         (columns (dom:quantity-report-columns report))
         (rows (dom:quantity-report-rows report))
         (window-width (float (app-window-width app) 1f0))
         (window-height (float (app-window-height app) 1f0))
         (width (report-card-width app))
         (inner (- width (* 2 +dialog-padding+)))
         (lines (report-message-lines app prompt))
         (capacity (report-capacity app
                                    prompt
                                    (length lines)))
         (scroll (max 0 (min (prompt-scroll prompt) (max 0 (- (length rows) capacity)))))
         (visible (subseq rows scroll (min (length rows) (+ scroll capacity))))
         (first-width (* inner +report-first-column-share+))
         (other-width (/ (- inner first-width) (max 1 (1- (length columns)))))
         (widths (cons first-width (loop repeat (1- (length columns)) collect other-width)))
         (totals (dom:quantity-report-totals report))
         (height (+ +dialog-padding+
                    +dialog-title-height+
                    (* (length lines) +dialog-line-height+)
                    +dialog-section-gap+
                    +report-row-height+
                    (* (length visible) +report-row-height+)
                    (if totals +report-row-height+ 0f0)
                    +dialog-section-gap+
                    +dialog-control-height+
                    +dialog-padding+))
         (x (/ (- window-width width) 2))
         (y (max 8f0 (/ (- window-height height) 3)))
         (content-x (+ x +dialog-padding+))
         (table-y (+ y +dialog-padding+ +dialog-title-height+ (* (length lines) +dialog-line-height+)
                     +dialog-section-gap+))
         (rows-y (+ table-y +report-row-height+))
         (totals-y (+ rows-y (* (length visible) +report-row-height+)))
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
                                  width
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
                                      (+ y +dialog-padding+ +dialog-title-height+ (* index +dialog-line-height+))
                                      inner
                                      +dialog-line-height+
                                      :color (scene:theme-text-dim theme)))
            (report-cell-widgets app
                                 report
                                 (mapcar (lambda (column) (dom:quantity-heading column unit)) columns)
                                 content-x
                                 table-y
                                 widths
                                 :report-heading
                                 :strong t)
            (list (ui:make-widget :separator
                                  :report-rule
                                  content-x
                                  rows-y
                                  inner
                                  1f0))
            (loop for row in visible
                  for index from 0
                  append (report-cell-widgets app
                                              report
                                              row
                                              content-x
                                              (+ rows-y (* index +report-row-height+))
                                              widths
                                              (list :report-row index)))
            (when totals
              (append (list (ui:make-widget :separator
                                            :report-totals-rule
                                            content-x
                                            totals-y
                                            inner
                                            1f0))
                      (report-cell-widgets app
                                           report
                                           totals
                                           content-x
                                           totals-y
                                           widths
                                           :report-total
                                           :strong t)))
            (when (> (length rows) capacity)
              (list (make-label :report-position
                                (i18n:trf "Rows ~d to ~d of ~d"
                                          (1+ scroll)
                                          (+ scroll (length visible))
                                          (length rows))
                                content-x
                                buttons-y
                                (- inner (* 2 +dialog-button-width+) (* 2 ui:+gap+))
                                +dialog-control-height+
                                :color (scene:theme-text-faint theme))))
            (prompt-buttons app
                            prompt
                            (+ x width)
                            buttons-y))))

(defun command-palette-widgets (app)
  "Build the command palette: a veil over the window, a card near the top, the field that is searched in and the
commands that answer it, the chosen one marked.

APP: the application."
  (let* ((prompt (app-prompt app))
         (theme (app-theme app))
         (matches (palette-prompt-matches prompt))
         (capacity (palette-capacity app))
         (scroll (max 0 (min (prompt-scroll prompt) (max 0 (- (length matches) capacity)))))
         (visible (subseq matches scroll (min (length matches) (+ scroll capacity))))
         (window-width (float (app-window-width app) 1f0))
         (window-height (float (app-window-height app) 1f0))
         (width (min +prompt-width+ (- window-width (* 2 +dialog-padding+))))
         (rows (max 1 (length visible)))
         (height (+ +palette-padding+
                    +palette-field-height+
                    +palette-gap+
                    (* rows +palette-row-height+)
                    +palette-padding+))
         (x (/ (- window-width width) 2))
         (y (max +palette-window-margin+ (/ (- window-height height) 6)))
         (content-x (+ x +palette-padding+))
         (inner (- width (* 2 +palette-padding+)))
         (rows-y (+ y +palette-padding+ +palette-field-height+ +palette-gap+)))
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
                                  width
                                  height)
                  (ui:make-widget :field
                                  :palette-field
                                  content-x
                                  (+ y +palette-padding+)
                                  inner
                                  +palette-field-height+
                                  :label (ui:edit-state-text (prompt-edit prompt))
                                  :edit (prompt-edit prompt)
                                  :align :left
                                  :icon :search))
            (if visible
                (loop for entry in visible
                      for index from 0
                      collect (let ((entry entry))
                                (ui:make-widget :row
                                                (list :palette-row index)
                                                content-x
                                                (+ rows-y (* index +palette-row-height+))
                                                inner
                                                +palette-row-height+
                                                :label (palette-entry-label entry)
                                                :align :left
                                                :shortcut (palette-entry-shortcut entry)
                                                :hint (palette-entry-hint entry)
                                                :active (= (+ scroll index) (prompt-active prompt))
                                                :action (lambda () (run-palette-entry app entry))
                                                :trailing (if (palette-entry-shortcut entry)
                                                              (+ +palette-shortcut-gap+
                                                                 (ui:text-width-pixels (palette-entry-shortcut entry)
                                                                                       ui:+small-font-pixels+))
                                                              0f0))))
                (list (make-label :palette-empty
                                  (i18n:tr "No command answers that.")
                                  content-x
                                  rows-y
                                  inner
                                  +palette-row-height+
                                  :color (scene:theme-text-faint theme)))))))

(defun prompt-widgets (app)
  "Build the modal prompt: a veil over the window, the dialog card, its message, its field, form or table and its
buttons.

APP: the application."
  (case (prompt-kind (app-prompt app))
    (:report (return-from prompt-widgets (report-widgets app)))
    (:palette (return-from prompt-widgets (command-palette-widgets app))))
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
