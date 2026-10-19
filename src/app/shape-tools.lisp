(in-package #:borba-cad/app)

;;;; Shapes in the application: the popover that the Shapes button of the palette opens, with a grid of the shapes of
;;;; one group, four across and four down at a time, scrolled by the wheel; the tool that places the shape that was
;;;; clicked; and the way an SVG file becomes a shape of the user's. A shape is placed as a block, which the drawing
;;;; defines the first time, so the drawing holds it and needs nothing else to be opened anywhere.

(defconstant +shape-tile-size+ 68f0
  "Side of the tile that shows a shape, in pixels.")

(defconstant +shape-tile-gap+ 8f0
  "Space between two tiles, in pixels.")

(defconstant +shapes-padding+ 12f0
  "Space between the edge of the popover and what is in it, in pixels.")

(defconstant +shapes-header-height+ 30f0
  "Height of the row with the title of the popover and its close button, in pixels.")

(defconstant +shapes-selector-height+ 30f0
  "Height of the row that says which group is shown and steps to the next and the one before, in pixels.")

(defconstant +shapes-footer-height+ 30f0
  "Height of the row at the foot of the popover, in pixels.")

(defconstant +shapes-scroll-room+ 8f0
  "Room at the right of the grid for the indicator of the scroll, in pixels.")

(defconstant +shapes-nominal-size+ 100d0
  "How large a shape is as it comes, in millimetres: the larger of its sides.")

;;; What the user has added

(defun user-shapes-directory (app)
  "Return the folder where the shapes that the user added are kept, in the state directory, or NIL when there is none.

APP: the application."
  (let ((state (base:config-state-directory (app-config app))))
    (and state
         (merge-pathnames "shapes/" (uiop:ensure-directory-pathname state)))))

(defun load-user-shapes (app)
  "Read the folder of the shapes of the user again, and remember what is in it.

APP: the application."
  (multiple-value-bind (areas problems) (shapes:load-user-areas (user-shapes-directory app))
    (setf (app-user-areas app) areas
          (app-shape-problems app) problems
          (app-user-areas-loaded app) t)))

(defun shape-areas (app)
  "Return the areas of shapes: those that come with the program and those the user added.

APP: the application."
  (unless (app-user-areas-loaded app)
    (load-user-shapes app))
  (shapes:all-areas (app-user-areas app)))

(defun shape-groups (app)
  "Return every group of shapes, as conses of its area and itself, in the order they are shown.

APP: the application."
  (loop for area in (shape-areas app)
        append (loop for group in (shapes:area-groups area)
                     collect (cons area group))))

(defun current-shape-group (app)
  "Return the group that the popover shows, as a cons of its area and itself, or NIL when there is none.

APP: the application."
  (let ((groups (shape-groups app)))
    (and groups
         (nth (min (app-shapes-group app) (1- (length groups))) groups))))

;;; Placing a shape

(defclass shape-tool (insert-tool)
  ((title :initarg :title :reader shape-tool-title))
  (:documentation "Places a shape of the library, as the insert tool places a block: a click for where, a second click
or a typed angle for the turn, F to flip it. A number typed before the first click is the size, which is how large the
larger side of the shape is, in the display unit."))

(defmethod tool-kind ((tool shape-tool)) :shape)

(defmethod tool-prompt ((tool shape-tool) app)
  (if (insert-point tool)
      (i18n:trf "Shape ~a: click to turn it, or Enter to leave it straight; type the angle in degrees"
                (shape-tool-title tool))
      (i18n:trf "Shape ~a: click where to place it, F flips it, or type the size (now ~a)"
                (shape-tool-title tool)
                (length-text app (* +shapes-nominal-size+ (insert-scale-factor tool))))))

(defmethod tool-typed-number ((tool shape-tool) app number)
  (if (insert-point tool)
      (call-next-method)
      (let ((scale (/ (base:unit->millimeters (float number 1d0) (display-unit-of app))
                      +shapes-nominal-size+)))
        (if (and (plusp scale) (< (/ dom:+maximum-insert-scale+) scale dom:+maximum-insert-scale+))
            (progn (setf (insert-scale-factor tool) scale)
                   (app-request-redraw app))
            (set-message app
                         (i18n:tr "The size must be above zero.")
                         :error))
        t)))

(defun place-shape (app
                    shape)
  "Make the shape the one to place: the drawing defines its block when it has none, the popover closes and the tool that
places it takes over.

APP: the application.
SHAPE: a shape of the library."
  (let ((name (shapes:shape-block-name shape)))
    (handler-case
        (progn
          (unless (dom:find-block (app-drawing app) name)
            (dom:define-block (app-drawing app) (shapes:shape-block-definition shape)))
          (close-menu app)
          (activate-tool app
                         (make-instance 'shape-tool
                                        :block-name name
                                        :title (shapes:shape-title shape)))
          (setf (app-last-block-name app) name))
      (base:borba-cad-error (condition)
        (report-error app
                      condition
                      "shape")))))

;;; The popover

(defun shapes-popover-rect (app)
  "Return where the popover is, as x, y, width and height: beside the palette, under the title bar.

APP: the application."
  (declare (ignore app))
  (values (+ ui:+tool-palette-width+ +menu-distance+)
          (+ ui:+toolbar-height+ +menu-distance+)
          (+ (* 2 +shapes-padding+)
             (* +shape-columns+ +shape-tile-size+)
             (* (1- +shape-columns+) +shape-tile-gap+)
             +shapes-scroll-room+)
          (+ (* 2 +shapes-padding+)
             +shapes-header-height+
             +shapes-selector-height+
             +shape-tile-gap+
             (* +shape-visible-rows+ +shape-tile-size+)
             (* (1- +shape-visible-rows+) +shape-tile-gap+)
             +shape-tile-gap+
             +shapes-footer-height+)))

(defun shapes-popover-contains-p (app
                                  x
                                  y)
  "Return true when the popover is open and the point is on it.

APP: the application.
X: window x.
Y: window y."
  (and (app-menu app)
       (eq :shapes (menu-kind (app-menu app)))
       (multiple-value-bind (left top width height) (shapes-popover-rect app)
         (and (<= left x (+ left width))
              (<= top y (+ top height))))))

(defun shape-row-count (group)
  "Return how many rows of tiles a group takes.

GROUP: a group of shapes, or NIL."
  (if group
      (ceiling (length (shapes:group-shapes group)) +shape-columns+)
      0))

(defun clamp-shape-scroll (app)
  "Keep the scroll of the popover between the first row and the last that can be the first in view.

APP: the application."
  (let ((entry (current-shape-group app)))
    (setf (app-shape-scroll app)
          (max 0 (min (app-shape-scroll app)
                      (max 0 (- (shape-row-count (cdr entry)) +shape-visible-rows+)))))))

(defun scroll-shapes (app
                      rows)
  "Scroll the grid of the popover by a number of rows.

APP: the application.
ROWS: how many rows, positive for later ones."
  (incf (app-shape-scroll app) rows)
  (clamp-shape-scroll app)
  (app-request-redraw app))

(defun step-shape-group (app
                         direction)
  "Show the next group of shapes, or the one before, going round, from the first row.

APP: the application.
DIRECTION: 1 for the next, -1 for the one before."
  (let ((count (length (shape-groups app))))
    (when (plusp count)
      (setf (app-shapes-group app) (mod (+ (min (app-shapes-group app) (1- count)) direction) count)
            (app-shape-scroll app) 0)
      (app-request-redraw app))))

(defun shape-group-title (entry)
  "Return the name of a group as the selector shows it: the title of its area and of the group, in the language of the
interface.

ENTRY: a cons of an area and a group."
  (let ((area (shapes:area-title (car entry)))
        (group (shapes:group-title (cdr entry))))
    (if (string= area group)
        (i18n:tr group)
        (format nil "~a / ~a" (i18n:tr area) (i18n:tr group)))))

(defun shape-tile-widgets (app
                           entry
                           grid-x
                           grid-y)
  "Build the tiles of the shapes of a group that are in view.

APP: the application.
ENTRY: a cons of an area and a group.
GRID-X: x of the left edge of the grid.
GRID-Y: y of the top edge of the grid."
  (let* ((group (cdr entry))
         (area (car entry))
         (all (shapes:group-shapes group))
         (first (* +shape-columns+ (app-shape-scroll app)))
         (last (min (length all) (+ first (* +shape-columns+ +shape-visible-rows+))))
         (visible (subseq all (min first (length all)) last)))
    (loop for shape in visible
          for index from 0
          collect (let ((shape shape))
                    (ui:make-widget :button
                                    (list :shape-tile
                                          (shapes:area-id area)
                                          (shapes:group-id group)
                                          (shapes:shape-id shape))
                                    (+ grid-x (* (mod index +shape-columns+) (+ +shape-tile-size+ +shape-tile-gap+)))
                                    (+ grid-y (* (floor index +shape-columns+) (+ +shape-tile-size+ +shape-tile-gap+)))
                                    +shape-tile-size+
                                    +shape-tile-size+
                                    :variant :tile
                                    :data (shapes:shape-preview-polylines shape)
                                    :hint (shapes:shape-title shape)
                                    :action (lambda () (place-shape app shape)))))))

(defun shapes-popover-widgets (app)
  "Build the popover of the shapes: its card, its title, the selector of the group, the grid of tiles in view, the
indicator of the scroll and the foot with how many shapes there are and the button that adds an SVG file.

APP: the application."
  (clamp-shape-scroll app)
  (multiple-value-bind (left top width height) (shapes-popover-rect app)
    (let* ((theme (app-theme app))
           (entry (current-shape-group app))
           (inner-width (- width (* 2 +shapes-padding+)))
           (selector-y (+ top +shapes-padding+ +shapes-header-height+))
           (grid-y (+ selector-y +shapes-selector-height+ +shape-tile-gap+))
           (grid-x (+ left +shapes-padding+))
           (footer-y (- (+ top height) +shapes-padding+ +shapes-footer-height+))
           (count (if entry (length (shapes:group-shapes (cdr entry))) 0))
           (widgets (list (ui:make-widget :card
                                          :shapes-card
                                          left
                                          top
                                          width
                                          height)
                          (make-label :shapes-title
                                      (i18n:tr "Shapes")
                                      grid-x
                                      (+ top +shapes-padding+)
                                      (- inner-width +shapes-header-height+)
                                      +shapes-header-height+
                                      :font :ui-strong
                                      :size ui:+heading-font-pixels+
                                      :color (scene:theme-text theme))
                          (make-icon-button :shapes-close
                                            :close
                                            (+ grid-x (- inner-width +shapes-header-height+))
                                            (+ top +shapes-padding+)
                                            :action (lambda () (close-menu app))
                                            :hint (i18n:tr "Close")
                                            :variant :ghost
                                            :size +shapes-header-height+)
                          (make-icon-button :shapes-previous
                                            :chevron-left
                                            grid-x
                                            selector-y
                                            :action (lambda () (step-shape-group app -1))
                                            :hint (i18n:tr "The group before")
                                            :variant :ghost
                                            :size +shapes-selector-height+)
                          (make-label :shapes-group
                                      (if entry
                                          (shape-group-title entry)
                                          (i18n:tr "No shapes"))
                                      (+ grid-x +shapes-selector-height+)
                                      selector-y
                                      (- inner-width (* 2 +shapes-selector-height+))
                                      +shapes-selector-height+
                                      :align :center
                                      :color (scene:theme-text-dim theme))
                          (make-icon-button :shapes-next
                                            :chevron-right
                                            (+ grid-x (- inner-width +shapes-selector-height+))
                                            selector-y
                                            :action (lambda () (step-shape-group app 1))
                                            :hint (i18n:tr "The group after")
                                            :variant :ghost
                                            :size +shapes-selector-height+))))
      (append widgets
              (and entry
                   (shape-tile-widgets app
                                       entry
                                       grid-x
                                       grid-y))
              (let ((thumb (scrollbar-widget :shapes-scrollbar
                                             (+ left width +shapes-padding+ (- +shapes-scroll-room+))
                                             grid-y
                                             (+ grid-y (* +shape-visible-rows+ +shape-tile-size+)
                                                (* (1- +shape-visible-rows+) +shape-tile-gap+))
                                             (shape-row-count (cdr entry))
                                             +shape-visible-rows+
                                             (app-shape-scroll app)
                                             theme)))
                (and thumb (list thumb)))
              (list (make-label :shapes-count
                                (i18n:trf "~d ~:*~[shapes~;shape~:;shapes~]" count)
                                grid-x
                                footer-y
                                (- inner-width 110f0)
                                +shapes-footer-height+
                                :size ui:+small-font-pixels+
                                :color (scene:theme-text-faint theme))
                    (ui:make-widget :button
                                    :shapes-add
                                    (+ grid-x (- inner-width 110f0))
                                    footer-y
                                    110f0
                                    +shapes-footer-height+
                                    :label (i18n:tr "Add SVG...")
                                    :icon :plus
                                    :variant :ghost
                                    :hint (i18n:tr "Add a shape from an SVG file")
                                    :action (lambda () (operation-import-shape app))
                                    :size ui:+small-font-pixels+))))))

(defun operation-toggle-shapes (app)
  "Open the popover of the shapes, or close it when it is open.

APP: the application."
  (if (and (app-menu app) (eq :shapes (menu-kind (app-menu app))))
      (close-menu app)
      (progn
        (load-user-shapes app)
        (setf (app-menu app) (make-menu :shapes
                                        :shapes
                                        0))
        (app-request-redraw app))))

;;; Shapes from SVG files

(defun operation-import-shape (app)
  "Open the dialog where the user picks an SVG file to add as a shape.

APP: the application."
  (close-menu app)
  (open-file-dialog app
                    :open
                    nil
                    :shape))

(defun import-shape-into-app (app
                              path-text)
  "Add the SVG file at PATH-TEXT to the shapes of the user: it is read and checked, copied into the folder of shapes,
and the popover opens on it. On failure the reason is shown. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds))
        (directory (user-shapes-directory app)))
    (handler-case
        (let* ((path (resolve-open-path path-text #'svg-file-path))
               (name (pathname-name path)))
          (when (null directory)
            (error 'base:invalid-input
                   :text path-text
                   :reason (i18n:tr "there is no folder to keep shapes in")))
          (shapes:read-svg-shape (persist:read-file-text path)
                                 name
                                 name)
          (ensure-directories-exist directory)
          (uiop:copy-file path (merge-pathnames (file-namestring path) directory))
          (load-user-shapes app)
          (let ((position (position-if (lambda (entry) (string= "my-shapes" (shapes:area-id (car entry))))
                                       (shape-groups app))))
            (when position
              (setf (app-shapes-group app) position
                    (app-shape-scroll app) 0)))
          (setf (app-menu app) (make-menu :shapes
                                          :shapes
                                          0))
          (set-message app
                       (i18n:trf "Shape ~a added." name))
          t)
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "shape.import"
                             started
                             condition)))))
