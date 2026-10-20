(in-package #:borba-cad/app)

;;;; The tool of the layout, and what the layout draws. A click on a viewport selects it, a drag moves it (what it shows
;;;; goes with it) or, from a corner, resizes it (what it shows stays where it is on the sheet); New viewport makes the
;;;; next drag put one on the sheet; Delete takes it away, Enter asks what it shows, and the arrows nudge it. The sheet
;;;; is drawn as it will be printed: white paper, the drawing through each viewport, the frame and the title block.

(defconstant +nudge-millimeters+ 1d0
  "How far an arrow key moves the selected viewport, in millimetres of paper.")

(defconstant +large-nudge-millimeters+ 10d0
  "How far an arrow key moves it with Shift held.")

(defclass layout-tool (tool) ()
  (:documentation "Selects, moves and resizes the viewports of the sheet that is shown, and makes new ones."))

(defmethod tool-kind ((tool layout-tool)) :layout)

(defmethod tool-prompt ((tool layout-tool) app)
  (let ((state (app-sheet-view app)))
    (cond ((null state) "")
          ((layout-state-arming state)
           (i18n:tr "New viewport: drag a rectangle on the sheet (Esc cancels)"))
          ((selected-viewport app)
           (i18n:tr "Drag the viewport to move it, a corner to resize it; Enter edits it, Delete takes it away"))
          (t
           (i18n:tr "Layout: click a viewport to select it, or choose New viewport")))))

(defun layout-hit-corner (app
                          viewport
                          x
                          y)
  "Return the number of the corner of a viewport that a point is on, or NIL.

APP: the application.
VIEWPORT: the viewport.
X: x of the point on the sheet.
Y: y of the point."
  (let ((reach (view:pixels->world-length (app-viewport app) +layout-handle-pixels+)))
    (loop for corner in (viewport-corners viewport)
          for number from 0
          when (and (<= (abs (- x (car corner))) reach) (<= (abs (- y (cdr corner))) reach))
            return number)))

(defun layout-hit-viewport (app
                            x
                            y)
  "Return the position of the viewport of the shown sheet that a point is in, the topmost of those that hold it, or NIL.

APP: the application.
X: x of the point on the sheet.
Y: y of the point."
  (let ((viewports (dom:sheet-viewports (layout-sheet app))))
    (loop for position from (1- (length viewports)) downto 0
          for viewport = (nth position viewports)
          when (geo:bounds-contains-point-p (plot:viewport-rectangle viewport)
                                            x
                                            y)
            return position)))

(defmethod tool-pointer-down ((tool layout-tool) app cursor)
  (let ((state (app-sheet-view app))
        (x (cursor-raw-x cursor))
        (y (cursor-raw-y cursor)))
    (cond ((layout-state-arming state)
           (setf (layout-state-drag state) (list :create x y)
                 (layout-state-preview state) (list x y x y)))
          (t
           (let* ((selected (selected-viewport app))
                  (corner (and selected (layout-hit-corner app
                                                           selected
                                                           x
                                                           y)))
                  (position (and (null corner) (layout-hit-viewport app
                                                                    x
                                                                    y))))
             (cond (corner
                    (setf (layout-state-drag state) (list :resize corner selected)))
                   (position
                    (select-viewport app position)
                    (setf (layout-state-drag state) (list :move x y (selected-viewport app))))
                   (t (select-viewport app nil))))))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool layout-tool) app cursor)
  (let* ((state (app-sheet-view app))
         (drag (layout-state-drag state))
         (x (cursor-raw-x cursor))
         (y (cursor-raw-y cursor)))
    (when drag
      (setf (layout-state-preview state)
            (ecase (first drag)
              (:create (list (second drag) (third drag) x y))
              (:move (destructuring-bind (start-x start-y original) (rest drag)
                       (dom:viewport-moved original
                                           (- x start-x)
                                           (- y start-y))))
              (:resize (destructuring-bind (corner original) (rest drag)
                         (or (resized-viewport original
                                               corner
                                               x
                                               y) original)))))
      (app-request-redraw app))))

(defmethod tool-pointer-up ((tool layout-tool) app cursor)
  (declare (ignore cursor))
  (let* ((state (app-sheet-view app))
         (drag (layout-state-drag state))
         (preview (layout-state-preview state)))
    (setf (layout-state-drag state) nil
          (layout-state-preview state) nil)
    (when drag
      (ecase (first drag)
        (:create
         (let ((viewport (and preview (apply #'viewport-from-corners app preview))))
           (cond (viewport
                  (setf (layout-state-arming state) nil)
                  (add-viewport-to-shown-sheet app viewport))
                 (t
                  (set-message app
                               (i18n:tr "That is too small for a viewport. Drag a larger rectangle.")
                               :error)))))
        ((:move :resize)
         (when (and preview (dom:viewport-p preview) (not (eq preview (car (last drag)))))
           (replace-selected-viewport app preview)))))
    (app-request-redraw app)))

(defun nudge-selected-viewport (app
                                dx
                                dy)
  "Move the selected viewport on the sheet, with what it shows.

APP: the application.
DX: how far to the right, in millimetres of paper.
DY: how far up."
  (let ((viewport (selected-viewport app)))
    (when viewport
      (replace-selected-viewport app
                                 (dom:viewport-moved viewport
                                                     dx
                                                     dy)))))

(defmethod tool-key ((tool layout-tool) app key modifiers)
  (let* ((state (app-sheet-view app))
         (step (if (modifier-p modifiers :shift) +large-nudge-millimeters+ +nudge-millimeters+))
         (selected (selected-viewport app)))
    (case key
      ((:delete :backspace)
       (when selected
         (operation-delete-viewport app)
         t))
      (:enter
       (when selected
         (operation-edit-viewport app)
         t))
      (:escape
       (cond ((or (layout-state-drag state) (layout-state-arming state))
              (tool-cancel tool app)
              t)
             (selected
              (select-viewport app nil)
              t)))
      (:left (and selected (progn (nudge-selected-viewport app
                                                           (- step)
                                                           0d0) t)))
      (:right (and selected (progn (nudge-selected-viewport app
                                                            step
                                                            0d0) t)))
      (:up (and selected (progn (nudge-selected-viewport app
                                                         0d0
                                                         step) t)))
      (:down (and selected (progn (nudge-selected-viewport app
                                                           0d0
                                                           (- step)) t))))))

(defmethod tool-cancel ((tool layout-tool) app)
  (let ((state (app-sheet-view app)))
    (when state
      (setf (layout-state-drag state) nil
            (layout-state-preview state) nil
            (layout-state-arming state) nil))
    (app-request-redraw app)))

(defmethod tool-in-progress-p ((tool layout-tool))
  nil)

;;; Drawing the sheet

(defparameter *paper-color* (scene:make-rgba 1f0
                                             1f0
                                             1f0
                                             1f0)
  "The color of the paper, which is white in every theme, as it is when printed.")

(defparameter *paper-shadow-color* (scene:make-rgba 0f0
                                                    0f0
                                                    0f0
                                                    0.35f0)
  "The color of the shadow under the paper.")

(defconstant +paper-shadow-pixels+ 5f0
  "How far the shadow of the paper is offset, in pixels.")

(defconstant +thick-line-pixels+ 1.6d0
  "A line at least this thick on the screen is drawn as a stroke with a body, not as a hairline.")

(defconstant +smallest-text-pixels+ 3d0
  "Text smaller than this on the screen is drawn as a line, as the drawing does.")

(defconstant +handle-size-pixels+ 7f0
  "Side of the square at each corner of the selected viewport, in pixels.")

(defun item-color (color)
  "Return a plot color as an RGBA.

COLOR: a list of red, green and blue, each from 0 to 1."
  (scene:make-rgba (float (first color) 1f0)
                   (float (second color) 1f0)
                   (float (third color) 1f0)
                   1f0))

(defun layout-screen (viewport
                      x
                      y
                      dx
                      dy)
  "Return the pixel position of a point of the sheet, moved by DX and DY on the sheet, as two single-floats.

VIEWPORT: the view of the sheet.
X: x on the sheet.
Y: y on the sheet.
DX: how far the point is moved, in millimetres of paper.
DY: how far."
  (multiple-value-bind (sx sy) (view:world->screen viewport
                                                   (+ x dx)
                                                   (+ y dy))
    (values (float sx 1f0) (float sy 1f0))))

(defun draw-plot-stroke (list
                         viewport
                         stroke
                         dx
                         dy)
  "Draw a stroke of a plot on the screen.

LIST: the draw list.
VIEWPORT: the view of the sheet.
STROKE: the plot stroke.
DX: how far it is moved on the sheet.
DY: how far."
  (let* ((points (plot:plot-stroke-points stroke))
         (path (if (plot:plot-stroke-closed stroke)
                   (append points (list (first points)))
                   points))
         (color (item-color (plot:plot-stroke-color stroke)))
         (thickness (* (or (plot:plot-stroke-width stroke) 0d0) (view:viewport-scale viewport))))
    (loop for (from to) on path
          while to
          do (multiple-value-bind (x1 y1) (layout-screen viewport
                                                         (car from)
                                                         (cdr from)
                                                         dx
                                                         dy)
               (multiple-value-bind (x2 y2) (layout-screen viewport
                                                           (car to)
                                                           (cdr to)
                                                           dx
                                                           dy)
                 (if (>= thickness +thick-line-pixels+)
                     (scene:add-thick-line list
                                           x1
                                           y1
                                           x2
                                           y2
                                           (float thickness 1f0)
                                           color)
                     (scene:add-line list
                                     x1
                                     y1
                                     x2
                                     y2
                                     color)))))))

(defun draw-plot-fill (list
                       viewport
                       fill
                       dx
                       dy)
  "Draw a filled polygon of a plot on the screen.

LIST: the draw list.
VIEWPORT: the view of the sheet.
FILL: the plot fill.
DX: how far it is moved on the sheet.
DY: how far."
  (scene:add-polygon list
                     (loop for (x . y) in (plot:plot-fill-points fill)
                           collect (multiple-value-bind (sx sy) (layout-screen viewport
                                                                               x
                                                                               y
                                                                               dx
                                                                               dy)
                                     (cons sx sy)))
                     (item-color (plot:plot-fill-color fill))))

(defun draw-plot-text (list
                       viewport
                       text
                       dx
                       dy)
  "Draw a text of a plot on the screen, as a line when it is too small to read.

LIST: the draw list.
VIEWPORT: the view of the sheet.
TEXT: the plot text.
DX: how far it is moved on the sheet.
DY: how far."
  (let ((size (* (plot:plot-text-height text) (view:viewport-scale viewport)))
        (color (item-color (plot:plot-text-color text))))
    (multiple-value-bind (x y) (layout-screen viewport
                                              (plot:plot-text-x text)
                                              (plot:plot-text-y text)
                                              dx
                                              dy)
      (if (< size +smallest-text-pixels+)
          (let ((length (* (length (plot:plot-text-string text)) 0.6d0 size)))
            (scene:add-line list
                            x
                            y
                            (+ x (float (* length (cos (plot:plot-text-angle text))) 1f0))
                            (- y (float (* length (sin (plot:plot-text-angle text))) 1f0))
                            color))
          (scene:add-text list
                          (plot:plot-text-string text)
                          x
                          y
                          (float size 1f0)
                          color
                          :angle (float (plot:plot-text-angle text) 1f0))))))

(defun draw-plot-items (list
                        viewport
                        items
                        dx
                        dy)
  "Draw the items of a plot on the screen.

LIST: the draw list.
VIEWPORT: the view of the sheet.
ITEMS: the strokes, fills and texts.
DX: how far they are moved on the sheet.
DY: how far."
  (dolist (item items)
    (etypecase item
      (plot:plot-stroke (draw-plot-stroke list
                                          viewport
                                          item
                                          dx
                                          dy))
      (plot:plot-fill (draw-plot-fill list
                                      viewport
                                      item
                                      dx
                                      dy))
      (plot:plot-text (draw-plot-text list
                                      viewport
                                      item
                                      dx
                                      dy)))))

(defun draw-paper (list
                   viewport
                   width
                   height)
  "Draw the sheet of paper with its shadow.

LIST: the draw list.
VIEWPORT: the view of the sheet.
WIDTH: width of the sheet in millimetres.
HEIGHT: its height."
  (multiple-value-bind (left top) (layout-screen viewport
                                                 0d0
                                                 height
                                                 0d0
                                                 0d0)
    (multiple-value-bind (right bottom) (layout-screen viewport
                                                       width
                                                       0d0
                                                       0d0
                                                       0d0)
      (scene:add-filled-rectangle list
                                  (+ left +paper-shadow-pixels+)
                                  (+ top +paper-shadow-pixels+)
                                  (- right left)
                                  (- bottom top)
                                  *paper-shadow-color*)
      (scene:add-filled-rectangle list
                                  left
                                  top
                                  (- right left)
                                  (- bottom top)
                                  *paper-color*))))

(defun draw-selection-frame (list
                             viewport
                             selected
                             theme)
  "Draw the border of the selected viewport in the accent color, with a square at each corner to take hold of.

LIST: the draw list.
VIEWPORT: the view of the sheet.
SELECTED: the viewport that is selected.
THEME: the theme."
  (let ((color (scene:theme-accent theme))
        (corners (viewport-corners selected)))
    (loop for (from to) on (append corners (list (first corners)))
          while to
          do (multiple-value-bind (x1 y1) (layout-screen viewport
                                                         (car from)
                                                         (cdr from)
                                                         0d0
                                                         0d0)
               (multiple-value-bind (x2 y2) (layout-screen viewport
                                                           (car to)
                                                           (cdr to)
                                                           0d0
                                                           0d0)
                 (scene:add-thick-line list
                                       x1
                                       y1
                                       x2
                                       y2
                                       2f0
                                       color))))
    (dolist (corner corners)
      (multiple-value-bind (x y) (layout-screen viewport
                                                (car corner)
                                                (cdr corner)
                                                0d0
                                                0d0)
        (scene:add-filled-rectangle list
                                    (- x (/ +handle-size-pixels+ 2))
                                    (- y (/ +handle-size-pixels+ 2))
                                    +handle-size-pixels+
                                    +handle-size-pixels+
                                    color)))))

(defun draw-rubber-rectangle (list
                              viewport
                              corners
                              theme)
  "Draw the rectangle that a drag is making.

LIST: the draw list.
VIEWPORT: the view of the sheet.
CORNERS: a list of x1, y1, x2 and y2 on the sheet.
THEME: the theme."
  (destructuring-bind (x1 y1 x2 y2) corners
    (multiple-value-bind (left top) (layout-screen viewport
                                                   (min x1 x2)
                                                   (max y1 y2)
                                                   0d0
                                                   0d0)
      (multiple-value-bind (right bottom) (layout-screen viewport
                                                         (max x1 x2)
                                                         (min y1 y2)
                                                         0d0
                                                         0d0)
        (scene:add-rectangle-outline list
                                     left
                                     top
                                     (- right left)
                                     (- bottom top)
                                     (scene:theme-accent theme))))))

(defun build-layout-scene (app
                           list)
  "Fill the draw list with the sheet that is shown: the paper, what is seen through each viewport, the frame and the
title block, and the selection or the drag in progress. Returns a SCENE-RESULT.

APP: the application.
LIST: the draw list, already reset."
  (let* ((state (app-sheet-view app))
         (sheet (layout-sheet app))
         (viewport (app-viewport app))
         (theme (app-theme app))
         (drag (layout-state-drag state))
         (preview (layout-state-preview state))
         (drawn 0))
    (multiple-value-bind (width height) (plot:sheet-size sheet)
      (draw-paper list
                  viewport
                  width
                  height))
    (dolist (shown (dom:sheet-viewports sheet))
      (cond ((and drag (eq (first drag) :move) (eq shown (car (last drag))) preview)
             (let ((items (layout-viewport-items app shown)))
               (incf drawn (cdr items))
               (draw-plot-items list
                                viewport
                                (car items)
                                (- (dom:viewport-x preview) (dom:viewport-x shown))
                                (- (dom:viewport-y preview) (dom:viewport-y shown)))))
            ((and drag (eq (first drag) :resize) (eq shown (car (last drag))) preview)
             (multiple-value-bind (items count) (plot:viewport-items (app-drawing app)
                                                                     (layout-entities app)
                                                                     preview
                                                                     (layout-settings))
               (incf drawn count)
               (draw-plot-items list
                                viewport
                                items
                                0d0
                                0d0)))
            (t
             (let ((items (layout-viewport-items app shown)))
               (incf drawn (cdr items))
               (draw-plot-items list
                                viewport
                                (car items)
                                0d0
                                0d0)))))
    (draw-plot-items list
                     viewport
                     (plot:title-block-items sheet
                                             (layout-state-sheet-index state)
                                             (dom:sheet-count (app-drawing app)))
                     0d0
                     0d0)
    (let ((selected (selected-viewport app)))
      (when selected
        (draw-selection-frame list
                              viewport
                              (if (and preview (dom:viewport-p preview) drag (member (first drag) '(:move :resize)))
                                  preview
                                  selected)
                              theme)))
    (when (and drag (eq (first drag) :create) preview)
      (draw-rubber-rectangle list
                             viewport
                             preview
                             theme))
    (scene:make-scene-result drawn 0)))

;;; The buttons of the layout

(defconstant +layout-button-height+ 28f0
  "Height of a button of the layout, in pixels.")

(defconstant +layout-button-padding+ 12f0
  "Space on each side of the label of a button of the layout, in pixels.")

(defconstant +layout-button-gap+ 6f0
  "Space between two buttons of the layout, in pixels.")

(defconstant +layout-margin+ 10f0
  "Space between the buttons of the layout and the edge of the canvas, in pixels.")

(defun layout-button-rows (app)
  "Return the buttons of the layout as two rows of (id label action enabled active hint), where an id that is NIL is
not a button but the name of the sheet that is shown.

APP: the application."
  (let* ((state (app-sheet-view app))
         (count (dom:sheet-count (app-drawing app)))
         (sheet (layout-sheet app))
         (selected (and (selected-viewport app) t)))
    (list (list (list :layout-model (i18n:tr "Model") (lambda () (operation-close-layout app)) t nil
                      (i18n:tr "Go back to the drawing"))
                (list :layout-previous "<" (lambda () (operation-next-sheet app -1)) (> count 1) nil
                      (i18n:tr "The sheet before"))
                (list nil (format nil "~a  ~d/~d" (dom:sheet-name sheet) (1+ (layout-state-sheet-index state)) count)
                      nil t nil nil)
                (list :layout-next ">" (lambda () (operation-next-sheet app 1)) (> count 1) nil
                      (i18n:tr "The sheet after"))
                (list :layout-add-sheet (i18n:tr "New sheet") (lambda () (operation-add-sheet app)) t nil
                      (i18n:tr "Add a sheet after the last"))
                (list :layout-sheet (i18n:tr "Sheet...") (lambda () (operation-sheet-properties app)) t nil
                      (i18n:tr "The name, the paper and the title block of this sheet"))
                (list :layout-delete-sheet (i18n:tr "Delete sheet") (lambda () (operation-delete-sheet app)) t nil
                      (i18n:tr "Take this sheet away")))
          (list (list :layout-new-viewport (i18n:tr "New viewport") (lambda () (operation-arm-viewport app)) t
                      (layout-state-arming state)
                      (i18n:tr "Drag a rectangle on the sheet to put a window on the drawing there"))
                (list :layout-viewport (i18n:tr "Viewport...") (lambda () (operation-edit-viewport app)) selected nil
                      (i18n:tr "What the selected viewport shows, and the scale"))
                (list :layout-delete-viewport (i18n:tr "Delete viewport") (lambda () (operation-delete-viewport app))
                      selected nil
                      (i18n:tr "Take the selected viewport off the sheet"))
                (list :layout-export (i18n:tr "Export PDF...") (lambda () (operation-export-sheets app)) t nil
                      (i18n:tr "Save the sheets as a PDF file, a page for each"))))))

(defun layout-widgets (app
                       canvas
                       blocked)
  "Build the buttons of the layout, in two rows in the top left corner of the canvas: going back to the drawing, moving
between the sheets and changing them, and the viewports of the sheet that is shown.

APP: the application.
CANVAS: the canvas rectangle.
BLOCKED: true while a modal covers them."
  (let ((theme (app-theme app))
        (widgets '())
        (y (+ (ui:rect-y canvas) +layout-margin+)))
    (dolist (row (layout-button-rows app))
      (let ((x (+ (ui:rect-x canvas) +layout-margin+)))
        (dolist (spec row)
          (destructuring-bind (id label action enabled active hint) spec
            (let ((width (+ (ui:text-width-pixels label) (* 2 +layout-button-padding+))))
              (push (if id
                        (ui:make-widget :button
                                        id
                                        x
                                        y
                                        width
                                        +layout-button-height+
                                        :label label
                                        :action action
                                        :enabled enabled
                                        :active active
                                        :blocked blocked
                                        :hint hint
                                        :variant :default)
                        (make-label :layout-sheet-name
                                    label
                                    x
                                    y
                                    width
                                    +layout-button-height+
                                    :align :center
                                    :color (scene:theme-text theme)))
                    widgets)
              (incf x (+ width +layout-button-gap+))))))
      (incf y (+ +layout-button-height+ +layout-button-gap+)))
    (nreverse widgets)))
