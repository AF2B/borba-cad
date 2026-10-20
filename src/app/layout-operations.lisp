(in-package #:borba-cad/app)

;;;; The layout: the view of the sheets that are printed, in which the canvas shows a sheet of paper with the drawing
;;;; seen through its viewports, as it will be printed. Entering it puts the view of the sheet where the view of the
;;;; drawing was, so that the wheel, the middle button and the zoom buttons work on the sheet as they do on the drawing,
;;;; and leaving it puts the view of the drawing back. What is kept of the layout is in the drawing, with its sheets;
;;;; what is here is the state of looking at it: which sheet, which viewport, and what has been worked out to draw it.

(defconstant +minimum-viewport-size+ 5d0
  "Smallest width or height, in millimetres of paper, that a viewport made with the pointer may have.")

(defconstant +layout-handle-pixels+ 9d0
  "How near the pointer must be to a corner of the selected viewport, in pixels, to take it.")

(defconstant +layout-fit-margin-pixels+ 70d0
  "Space, in pixels, kept around a sheet when the layout is fitted to the window, which has room for its buttons.")

(defconstant +default-viewport-scale+ 0.02d0
  "The scale of a viewport made on a drawing that has nothing to show, 1:50.")

(defparameter *standard-scales*
  '(10 5 2 1 1/2 1/5 1/10 1/20 1/25 1/50 1/75 1/100 1/125 1/150 1/200 1/250 1/500 1/1000 1/2000 1/5000)
  "The scales that drawings are usually printed at, as the millimetres of paper for a millimetre of the drawing, the
largest first.")

(defstruct (layout-state (:constructor make-layout-state (model-viewport)))
  "The state of looking at the sheets.

MODEL-VIEWPORT: the view of the drawing that was in force when the layout was opened, which is put back when it is left.
SHEET-INDEX: the position of the sheet that is shown, from 0.
SELECTED: the position of the selected viewport on that sheet, or NIL.
ARMING: true when the next drag with the pointer makes a viewport.
DRAG: what the pointer is doing: NIL, or a list that begins with :create, :move or :resize.
PREVIEW: the viewport, or the corners of the rectangle, that the drag has got to, or NIL.
ENTITIES: the entities that are drawn through the viewports, worked out for the revision that ENTITIES-REVISION says.
ENTITIES-REVISION: the revision of the drawing that ENTITIES was worked out for, or -1.
ITEMS: hash table from a viewport to what was worked out to draw it, as a cons of its plot items and how many
entities it shows."
  model-viewport
  (sheet-index 0 :type fixnum)
  (selected nil)
  (arming nil)
  (drag nil)
  (preview nil)
  (entities nil :type list)
  (entities-revision -1 :type fixnum)
  (items (make-hash-table :test 'eq) :type hash-table))

(defun layout-mode-p (app)
  "Return true while the sheets are shown in place of the drawing.

APP: the application."
  (and (app-sheet-view app) t))

(defun layout-sheet (app)
  "Return the sheet that is shown, or NIL when the layout is not open.

APP: the application."
  (and (app-sheet-view app)
       (dom:sheet-at (app-drawing app) (layout-state-sheet-index (app-sheet-view app)))))

;;; What is drawn

(defun layout-entities (app)
  "Return the entities that are drawn through the viewports, worked out again when the drawing has changed since they
were. A change to the sheets alone does not count, because the layout keeps its work up to date for those.

APP: the application."
  (let ((state (app-sheet-view app))
        (revision (dom:drawing-revision (app-drawing app))))
    (unless (= revision (layout-state-entities-revision state))
      (setf (layout-state-entities state) (plot:visible-entities (app-drawing app))
            (layout-state-entities-revision state) revision)
      (clrhash (layout-state-items state)))
    (layout-state-entities state)))

(defun layout-settings ()
  "Return the plot settings that the layout is drawn with on the screen: the colors of the layers, as in a PDF in
color."
  (plot:make-plot-settings :monochrome nil))

(defun layout-viewport-items (app
                              viewport)
  "Return what draws a viewport, as the plot items and how many entities it shows, working it out when that has not
been done for this viewport since the drawing changed.

APP: the application.
VIEWPORT: the viewport."
  (let* ((state (app-sheet-view app))
         (entities (layout-entities app))
         (cached (gethash viewport (layout-state-items state))))
    (unless cached
      (multiple-value-bind (items count) (plot:viewport-items (app-drawing app)
                                                              entities
                                                              viewport
                                                              (layout-settings))
        (setf cached (cons items count)
              (gethash viewport (layout-state-items state)) cached)))
    cached))

(defun commit-sheets (app
                      change)
  "Change the sheets of the drawing with CHANGE, keeping what the layout has worked out of the drawing, which this
does not change, and say that the next frame is different.

APP: the application.
CHANGE: function of no arguments that changes the sheets, which may signal EDIT-REFUSED."
  (let* ((state (app-sheet-view app))
         (current (and state (= (layout-state-entities-revision state)
                                (dom:drawing-revision (app-drawing app))))))
    (funcall change)
    (when current
      (setf (layout-state-entities-revision state) (dom:drawing-revision (app-drawing app))))
    (app-request-redraw app)))

(defun report-sheet-refusal (app
                             thunk)
  "Run THUNK, which changes the sheets, and say in the status bar why it could not when it was refused. Returns true
when it was done.

APP: the application.
THUNK: function of no arguments."
  (handler-case (progn (commit-sheets app thunk)
                       t)
    (base:borba-cad-error (condition)
      (report-error app
                    condition
                    "layout")
      nil)))

;;; Standard scales

(defun standard-scale-within (ratio)
  "Return the largest of the usual scales that is no larger than RATIO, or RATIO when it is smaller than all of them.

RATIO: millimetres of paper for a millimetre of the drawing."
  (let ((found (find-if (lambda (standard) (<= standard (* ratio (+ 1 1d-9)))) *standard-scales*)))
    (if found
        (float found 1d0)
        ratio)))

(defun viewport-showing (viewport
                         bounds)
  "Return a viewport like another that shows some bounds of the drawing, at the largest of the usual scales at which
all of them show.

VIEWPORT: the viewport.
BOUNDS: the part of the drawing to show."
  (dom:viewport-with viewport
                     :center-x (geo:bounds-center-x bounds)
                     :center-y (geo:bounds-center-y bounds)
                     :scale (standard-scale-within (plot:fit-ratio bounds
                                                                   (dom:viewport-width viewport)
                                                                   (dom:viewport-height viewport)
                                                                   0d0))))

(defun drawing-bounds (app)
  "Return the bounds of everything that is shown, or NIL when there is nothing.

APP: the application."
  (let ((entities (plot:visible-entities (app-drawing app))))
    (and entities
         (plot:entities-bounds entities))))

(defun selection-bounds (app)
  "Return the bounds of the selected entities, or NIL when none is selected.

APP: the application."
  (let ((entities (selected-entities app)))
    (and entities
         (plot:entities-bounds entities))))

;;; Opening and leaving

(defun fit-layout-view (app)
  "Fit the view to the whole of the sheet that is shown.

APP: the application."
  (let ((sheet (layout-sheet app)))
    (when sheet
      (multiple-value-bind (width height) (plot:sheet-size sheet)
        (setf (app-viewport app)
              (view:fit-viewport-to-bounds (app-viewport app)
                                           (geo:make-bounds 0d0
                                                            0d0
                                                            width
                                                            height)
                                           :margin-pixels +layout-fit-margin-pixels+))))
    (app-request-redraw app)))

(defun first-sheet-for (app)
  "Return a sheet for a drawing that has none, with a viewport that shows all of the drawing.

APP: the application."
  (let* ((blank (dom:make-sheet :name (i18n:trf "Sheet ~d" 1)))
         (bounds (drawing-bounds app))
         (viewport (plot:default-viewport blank
                                          (if bounds (geo:bounds-center-x bounds) 0d0)
                                          (if bounds (geo:bounds-center-y bounds) 0d0)
                                          +default-viewport-scale+)))
    (dom:sheet-with blank
                    :viewports (list (if bounds
                                         (viewport-showing viewport bounds)
                                         viewport)))))

(defun operation-open-layout (app)
  "Show the sheets in place of the drawing, making a first sheet when the drawing has none, which shows all of the
drawing.

APP: the application."
  (unless (layout-mode-p app)
    (when (and (app-tool app) (tool-in-progress-p (app-tool app)))
      (tool-cancel (app-tool app) app))
    (clear-selection app)
    (setf (app-sheet-view app) (make-layout-state (app-viewport app)))
    (when (zerop (dom:sheet-count (app-drawing app)))
      (report-sheet-refusal app
                            (lambda ()
                              (dom:add-sheet (app-drawing app) (first-sheet-for app)))))
    (activate-tool app (make-instance 'layout-tool))
    (fit-layout-view app)
    (set-message app (i18n:tr "Layout: the sheets as they will be printed."))))

(defun operation-close-layout (app)
  "Show the drawing again, in the view it was in when the layout was opened.

APP: the application."
  (when (layout-mode-p app)
    (let ((model-viewport (layout-state-model-viewport (app-sheet-view app))))
      (setf (app-sheet-view app) nil)
      (activate-tool app (make-tool :select))
      (setf (app-viewport app) (fit-viewport-to-window app model-viewport))
      (refresh-cursor app)
      (app-request-redraw app))))

(defun operation-toggle-layout (app)
  "Open the layout, or leave it when it is open.

APP: the application."
  (if (layout-mode-p app)
      (operation-close-layout app)
      (operation-open-layout app)))

;;; Sheets

(defun select-sheet (app
                     index)
  "Show another sheet.

APP: the application.
INDEX: its position, from 0."
  (let ((state (app-sheet-view app)))
    (when (and state (< -1 index (dom:sheet-count (app-drawing app))))
      (setf (layout-state-sheet-index state) index
            (layout-state-selected state) nil
            (layout-state-arming state) nil
            (layout-state-drag state) nil
            (layout-state-preview state) nil)
      (fit-layout-view app))))

(defun operation-next-sheet (app
                             step)
  "Show the sheet STEP places after the one shown, going round at the ends.

APP: the application.
STEP: 1 for the next, -1 for the one before."
  (let ((count (dom:sheet-count (app-drawing app))))
    (when (and (layout-mode-p app) (plusp count))
      (select-sheet app (mod (+ (layout-state-sheet-index (app-sheet-view app)) step) count)))))

(defun operation-add-sheet (app)
  "Add a sheet after the last, on the paper of the one shown and with its title block, without viewports, and show it.

APP: the application."
  (let* ((drawing (app-drawing app))
         (shown (layout-sheet app))
         (sheet (dom:make-sheet :name (i18n:trf "Sheet ~d" (1+ (dom:sheet-count drawing)))
                                :paper (if shown (dom:sheet-paper shown) :a3)
                                :landscape (if shown (dom:sheet-landscape shown) t)
                                :fields (and shown
                                             (remove :title (dom:sheet-fields shown) :key #'car)))))
    (when (report-sheet-refusal app
                                (lambda () (dom:add-sheet drawing sheet)))
      (select-sheet app (1- (dom:sheet-count drawing)))
      (set-message app (i18n:tr "Sheet added. Use New viewport to put the drawing on it.")))))

(defun delete-shown-sheet (app)
  "Take away the sheet that is shown, and leave the layout when it was the last.

APP: the application."
  (let ((index (layout-state-sheet-index (app-sheet-view app))))
    (when (report-sheet-refusal app
                                (lambda () (dom:remove-sheet (app-drawing app) index)))
      (if (zerop (dom:sheet-count (app-drawing app)))
          (operation-close-layout app)
          (select-sheet app (min index (1- (dom:sheet-count (app-drawing app)))))))))

(defun operation-delete-sheet (app)
  "Ask whether to take away the sheet that is shown, and do it when the answer is yes.

APP: the application."
  (let ((sheet (layout-sheet app)))
    (when sheet
      (open-choice-prompt app
                          (i18n:tr "Delete the sheet")
                          (i18n:trf "Delete ~a, with its viewports? This cannot be undone."
                                    (dom:sheet-name sheet))
                          (list (cons (i18n:tr "Delete")
                                      (lambda () (delete-shown-sheet app)))
                                (cons (i18n:tr "Cancel") nil))))))

(defparameter *sheet-paper-names*
  '((:a4 . "A4") (:a3 . "A3") (:a2 . "A2") (:a1 . "A1") (:a0 . "A0") (:letter . "Letter"))
  "The papers a sheet may be on, as the form that asks for one names them.")

(defun sheet-properties-values (sheet
                                values)
  "Return the sheet that a form of the properties of a sheet describes, signaling EDIT-REFUSED when it makes no sense.

SHEET: the sheet being changed.
VALUES: the alist of the form."
  (dom:sheet-with sheet
                  :name (form-value values :name)
                  :paper (form-value values :paper)
                  :landscape (eq (form-value values :orientation) :landscape)
                  :fields (loop for key in dom:*title-field-keys*
                                collect (cons key (form-value values key)))))

(defun operation-sheet-properties (app)
  "Ask for the name, the paper and the title block of the sheet that is shown.

APP: the application."
  (let ((sheet (layout-sheet app)))
    (when sheet
      (open-form-prompt app
                        (i18n:tr "Sheet")
                        (i18n:tr "The name and the paper, and what the title block says.")
                        (list (list :name
                                    (i18n:tr "Name")
                                    (dom:sheet-name sheet))
                              (list :paper
                                    (i18n:tr "Paper")
                                    (dom:sheet-paper sheet)
                                    *sheet-paper-names*)
                              (list :orientation
                                    (i18n:tr "Lies")
                                    (if (dom:sheet-landscape sheet) :landscape :portrait)
                                    (list (cons :landscape (i18n:tr "Wide (landscape)"))
                                          (cons :portrait (i18n:tr "Tall (portrait)"))))
                              (list :project
                                    (i18n:tr "Project")
                                    (dom:sheet-field sheet :project))
                              (list :title
                                    (i18n:tr "Title")
                                    (dom:sheet-field sheet :title))
                              (list :author
                                    (i18n:tr "Author")
                                    (dom:sheet-field sheet :author))
                              (list :date
                                    (i18n:tr "Date")
                                    (dom:sheet-field sheet :date))
                              (list :revision
                                    (i18n:tr "Revision")
                                    (dom:sheet-field sheet :revision)))
                        (lambda (values)
                          (handler-case
                              (let ((index (layout-state-sheet-index (app-sheet-view app)))
                                    (changed (sheet-properties-values sheet values)))
                                (commit-sheets app
                                               (lambda ()
                                                 (dom:replace-sheet (app-drawing app)
                                                                    index
                                                                    changed)))
                                (fit-layout-view app)
                                nil)
                            (base:borba-cad-error (condition)
                              (remove #\Newline (princ-to-string condition)))))))))

;;; Viewports

(defun selected-viewport (app)
  "Return the selected viewport, or NIL.

APP: the application."
  (let ((state (app-sheet-view app))
        (sheet (layout-sheet app)))
    (and state
         sheet
         (layout-state-selected state)
         (nth (layout-state-selected state) (dom:sheet-viewports sheet)))))

(defun select-viewport (app
                        position)
  "Select a viewport of the sheet that is shown, or none.

APP: the application.
POSITION: its position on the sheet, from 0, or NIL."
  (setf (layout-state-selected (app-sheet-view app)) position)
  (app-request-redraw app))

(defun replace-selected-viewport (app
                                  viewport)
  "Put a viewport in place of the selected one. Returns true when it was done.

APP: the application.
VIEWPORT: the new viewport."
  (let ((index (layout-state-sheet-index (app-sheet-view app)))
        (position (layout-state-selected (app-sheet-view app))))
    (report-sheet-refusal app
                          (lambda ()
                            (dom:replace-viewport (app-drawing app)
                                                  index
                                                  position
                                                  viewport)))))

(defun add-viewport-to-shown-sheet (app
                                    viewport)
  "Put a viewport on the sheet that is shown and select it. Returns true when it was done.

APP: the application.
VIEWPORT: the viewport."
  (let ((index (layout-state-sheet-index (app-sheet-view app)))
        (position nil))
    (when (report-sheet-refusal app
                                (lambda ()
                                  (setf position (dom:add-viewport (app-drawing app)
                                                                   index
                                                                   viewport))))
      (select-viewport app position)
      t)))

(defun viewport-from-corners (app
                              x1
                              y1
                              x2
                              y2)
  "Return the viewport that fills a rectangle of the sheet and shows all of the drawing at one of the usual scales, or
NIL when the rectangle is too small.

APP: the application.
X1: x of one corner on the sheet.
Y1: y of that corner.
X2: x of the opposite corner.
Y2: y of that corner."
  (let ((width (abs (- x2 x1)))
        (height (abs (- y2 y1))))
    (when (and (>= width +minimum-viewport-size+) (>= height +minimum-viewport-size+))
      (let* ((bounds (drawing-bounds app))
             (viewport (dom:make-viewport :x (min x1 x2)
                                          :y (min y1 y2)
                                          :width width
                                          :height height
                                          :center-x (if bounds (geo:bounds-center-x bounds) 0d0)
                                          :center-y (if bounds (geo:bounds-center-y bounds) 0d0)
                                          :scale +default-viewport-scale+)))
        (if bounds
            (viewport-showing viewport bounds)
            viewport)))))

(defun operation-arm-viewport (app)
  "Make the next drag with the pointer, on the sheet, put a viewport there; a second choice takes it back.

APP: the application."
  (let ((state (app-sheet-view app)))
    (when state
      (setf (layout-state-arming state) (not (layout-state-arming state))
            (layout-state-drag state) nil
            (layout-state-preview state) nil)
      (app-request-redraw app))))

(defun operation-delete-viewport (app)
  "Take the selected viewport off the sheet.

APP: the application."
  (let ((state (app-sheet-view app)))
    (when (and state (layout-state-selected state))
      (let ((index (layout-state-sheet-index state))
            (position (layout-state-selected state)))
        (when (report-sheet-refusal app
                                    (lambda ()
                                      (dom:remove-viewport (app-drawing app)
                                                           index
                                                           position)))
          (select-viewport app nil))))))

(defun resized-viewport (viewport
                         corner
                         x
                         y)
  "Return a viewport like another with one corner moved to a point and the opposite corner where it was, which keeps
what it shows where it is on the sheet, or NIL when that would leave it too small.

VIEWPORT: the viewport.
CORNER: which corner is dragged: 0 for the lower left, 1 for the lower right, 2 for the upper right, 3 for the upper
left.
X: x of the point on the sheet.
Y: y of the point."
  (let* ((left (dom:viewport-x viewport))
         (bottom (dom:viewport-y viewport))
         (right (+ left (dom:viewport-width viewport)))
         (top (+ bottom (dom:viewport-height viewport)))
         (fixed-x (if (member corner '(0 3)) right left))
         (fixed-y (if (member corner '(0 1)) top bottom))
         (new-left (min fixed-x x))
         (new-right (max fixed-x x))
         (new-bottom (min fixed-y y))
         (new-top (max fixed-y y))
         (width (- new-right new-left))
         (height (- new-top new-bottom)))
    (when (and (>= width +minimum-viewport-size+) (>= height +minimum-viewport-size+))
      (let ((scale (dom:viewport-scale viewport)))
        (dom:viewport-with viewport
                           :x new-left
                           :y new-bottom
                           :width width
                           :height height
                           :center-x (+ (dom:viewport-center-x viewport)
                                        (/ (- (+ new-left (/ width 2))
                                              (+ left (/ (dom:viewport-width viewport) 2)))
                                           scale))
                           :center-y (+ (dom:viewport-center-y viewport)
                                        (/ (- (+ new-bottom (/ height 2))
                                              (+ bottom (/ (dom:viewport-height viewport) 2)))
                                           scale)))))))

(defun viewport-corners (viewport)
  "Return the corners of a viewport on the sheet as a list of (x . y) conses, the lower left first and going round
counter-clockwise, the order that RESIZED-VIEWPORT numbers them in.

VIEWPORT: the viewport."
  (let ((x (dom:viewport-x viewport))
        (y (dom:viewport-y viewport))
        (right (+ (dom:viewport-x viewport) (dom:viewport-width viewport)))
        (top (+ (dom:viewport-y viewport) (dom:viewport-height viewport))))
    (list (cons x y) (cons right y) (cons right top) (cons x top))))

(defun viewport-form-showing (app
                              viewport
                              what)
  "Return the viewport that a choice of the form says: the same, or one that shows all of the drawing or the selection.

APP: the application.
VIEWPORT: the viewport with the numbers of the form.
WHAT: :keep, :all or :selection."
  (let ((bounds (ecase what
                  (:keep nil)
                  (:all (drawing-bounds app))
                  (:selection (selection-bounds app)))))
    (cond ((eq what :keep) viewport)
          ((null bounds)
           (error 'base:edit-refused
                  :reason (if (eq what :all)
                              (i18n:tr "There is nothing to show.")
                              (i18n:tr "Nothing is selected."))))
          (t (viewport-showing viewport bounds)))))

(defun operation-edit-viewport (app)
  "Ask what the selected viewport shows: the whole drawing, the selection, or a scale and a center.

APP: the application."
  (let ((viewport (selected-viewport app)))
    (when viewport
      (open-form-prompt app
                        (i18n:tr "Viewport")
                        (i18n:tr "What this window on the drawing shows, and at what scale. A scale is written 1:50.")
                        (list (list :show
                                    (i18n:tr "Show")
                                    :keep
                                    (list (cons :keep (i18n:tr "What the numbers below say"))
                                          (cons :all (i18n:tr "All of the drawing"))
                                          (cons :selection (i18n:tr "The selection"))))
                              (list :scale
                                    (i18n:tr "Scale")
                                    (plot:scale-text (dom:viewport-scale viewport)))
                              (list :center-x
                                    (i18n:tr "Center, across")
                                    (length-text app (dom:viewport-center-x viewport)))
                              (list :center-y
                                    (i18n:tr "Center, up")
                                    (length-text app (dom:viewport-center-y viewport))))
                        (lambda (values)
                          (handler-case
                              (let* ((ratio (or (plot:parse-plot-scale (form-value values :scale))
                                                (dom:viewport-scale viewport)))
                                     (numbers (dom:viewport-with viewport
                                                                 :scale ratio
                                                                 :center-x (form-length app
                                                                                        values
                                                                                        :center-x)
                                                                 :center-y (form-length app
                                                                                        values
                                                                                        :center-y)))
                                     (shown (viewport-form-showing app
                                                                   numbers
                                                                   (form-value values :show))))
                                (and (replace-selected-viewport app shown)
                                     nil))
                            (base:borba-cad-error (condition)
                              (remove #\Newline (princ-to-string condition)))))))))

;;; Printing the sheets

(defun plot-colors-question ()
  "Return the question of whether to print in black or in color."
  (i18n:tr "Print in black, or in the color of each layer? Very light colors are printed black."))

(defun sheets-pdf-title ()
  "Return the heading of the questions of the export of the sheets."
  (i18n:tr "Export the sheets as PDF"))

(defun replaced-text (replaced)
  "Return what is said of the characters that the PDF font could not show, or NIL when there were none.

REPLACED: how many there were."
  (and (plusp replaced)
       (i18n:trf "~d characters could not be written in the PDF font and became question marks."
                 replaced)))

(defun export-sheets-from-app (app
                               path-text)
  "Save the sheets as a PDF file at PATH-TEXT (.pdf is added to a name without an extension), a page for each. Returns
true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds))
        (settings (plot:make-plot-settings :monochrome (form-setting app
                                                                     :sheets-monochrome
                                                                     nil))))
    (handler-case
        (multiple-value-bind (octets plans replaced) (plot:render-sheets-pdf (app-drawing app) settings)
          (multiple-value-bind (saved stats) (persist:save-binary-file (pdf-file-path path-text) octets)
            (obs:record-operation-result (app-telemetry app)
                                         "file.export-sheets"
                                         (- (sbcl:monotonic-nanoseconds) started)
                                         :info
                                         :bytes (getf stats :bytes)
                                         :entity-count (reduce #'+ plans :key #'plot:plot-plan-entity-count)
                                         :result :success)
            (show-notice app
                         (i18n:tr "PDF exported")
                         (format nil "~a~@[ ~a~]"
                                 (i18n:trf "~a was saved (~a), with ~d ~:*~[sheets~;sheet~:;sheets~]."
                                           (file-namestring saved)
                                           (base:format-byte-size (getf stats :bytes))
                                           (length plans))
                                 (replaced-text replaced))
                         +report-message-lines+)
            t))
      ((or base:drawing-file-error base:invalid-input base:export-refused) (condition)
        (fail-export-operation app
                               "file.export-sheets"
                               started
                               condition)))))

(defun operation-export-sheets (app)
  "Ask whether to print in black or in color, then where to save the sheets of the drawing as a PDF file.

APP: the application."
  (flet ((choose (monochrome)
           (lambda ()
             (remember-form-settings app (list (cons :sheets-monochrome monochrome)))
             (open-file-dialog app
                               :save
                               nil
                               :sheets))))
    (if (zerop (dom:sheet-count (app-drawing app)))
        (set-message app
                     (i18n:tr "There are no sheets yet: open the layout, Ctrl+L, and a sheet is made.")
                     :error)
        (open-choice-prompt app
                            (sheets-pdf-title)
                            (plot-colors-question)
                            (list (cons (i18n:tr "Black") (choose t))
                                  (cons (i18n:tr "Layer colors") (choose nil))
                                  (cons (i18n:tr "Cancel") nil))))))

(defun operation-show-next-sheet (app)
  "Show the sheet after the one shown.

APP: the application."
  (operation-next-sheet app 1))

(defun operation-show-previous-sheet (app)
  "Show the sheet before the one shown.

APP: the application."
  (operation-next-sheet app -1))
