(in-package #:borba-cad/domain)

;;;; The sheets of a drawing, which is what is printed: each is a page of a given paper, with windows on the drawing
;;;; (viewports), each showing a part of it at a scale of its own, and a title block of fields. The drawing holds
;;;; them in order, they are saved with it, and they are not steps of undo, as the layers are not. A sheet is a value:
;;;; changing one makes another, and the drawing holds the new list.

(defparameter *sheet-papers* '(:a4 :a3 :a2 :a1 :a0 :letter)
  "The papers a sheet may be on, which the plot module gives sizes to.")

(defparameter *title-field-keys* '(:project :title :author :date :revision)
  "The fields of the title block that the user writes, in the order they are asked and kept.")

(defconstant +maximum-sheets+ 200
  "Most sheets a drawing may have.")

(defconstant +maximum-viewports+ 64
  "Most viewports a sheet may have.")

(defconstant +maximum-title-field-length+ 120
  "Longest text a field of the title block may hold.")

(defconstant +maximum-sheet-name-length+ 64
  "Longest name a sheet may have.")

(defconstant +maximum-viewport-scale+ 1d6
  "Largest scale of a viewport, in paper per drawing, and the reciprocal of the smallest.")

(defconstant +maximum-sheet-coordinate+ 1d5
  "Farthest, in millimetres, from the corner of a sheet that a viewport may reach.")

(defstruct (viewport (:constructor %make-viewport (x
                                                   y
                                                   width
                                                   height
                                                   center-x
                                                   center-y
                                                   scale))
                     (:copier nil))
  "A window on the drawing, put on a sheet.

X: x of its lower left corner on the sheet, in millimetres from the sheet's lower left corner.
Y: y of that corner.
WIDTH: width on the sheet.
HEIGHT: height on the sheet.
CENTER-X: x of the point of the drawing at the center of the window.
CENTER-Y: y of that point.
SCALE: millimetres of paper for a millimetre of the drawing."
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (width 1d0 :type double-float :read-only t)
  (height 1d0 :type double-float :read-only t)
  (center-x 0d0 :type double-float :read-only t)
  (center-y 0d0 :type double-float :read-only t)
  (scale 1d0 :type double-float :read-only t))

(defun make-viewport (&key x
                           y
                           width
                           height
                           center-x
                           center-y
                           scale)
  "Create a viewport. Signals EDIT-REFUSED, with the reason, when it makes no sense.

X: x of its lower left corner on the sheet.
Y: y of that corner.
WIDTH: width on the sheet, above zero.
HEIGHT: height on the sheet, above zero.
CENTER-X: x of the point of the drawing at its center.
CENTER-Y: y of that point.
SCALE: millimetres of paper for a millimetre of the drawing, above zero."
  (dolist (value (list x y width height center-x center-y scale))
    (unless (and (realp value) (< (abs value) base:+maximum-coordinate-magnitude+))
      (refuse-edit "A viewport is made of numbers of a reasonable size.")))
  (unless (and (base:positive-length-p (float width 1d0)) (base:positive-length-p (float height 1d0)))
    (refuse-edit "A viewport must have a width and a height above zero."))
  (unless (< (/ +maximum-viewport-scale+) scale +maximum-viewport-scale+)
    (refuse-edit "The scale of a viewport is out of range."))
  (unless (and (<= (abs x) +maximum-sheet-coordinate+) (<= (abs y) +maximum-sheet-coordinate+)
               (<= (+ (abs x) width) +maximum-sheet-coordinate+) (<= (+ (abs y) height) +maximum-sheet-coordinate+))
    (refuse-edit "The viewport is too far from the sheet."))
  (%make-viewport (float x 1d0)
                  (float y 1d0)
                  (float width 1d0)
                  (float height 1d0)
                  (float center-x 1d0)
                  (float center-y 1d0)
                  (float scale 1d0)))

(defun viewport-moved (viewport
                       dx
                       dy)
  "Return a viewport like another, put DX and DY farther on the sheet.

VIEWPORT: the viewport.
DX: how far to the right, in millimetres.
DY: how far up."
  (make-viewport :x (+ (viewport-x viewport) dx)
                 :y (+ (viewport-y viewport) dy)
                 :width (viewport-width viewport)
                 :height (viewport-height viewport)
                 :center-x (viewport-center-x viewport)
                 :center-y (viewport-center-y viewport)
                 :scale (viewport-scale viewport)))

(defun viewport-with (viewport
                      &key (x (viewport-x viewport))
                           (y (viewport-y viewport))
                           (width (viewport-width viewport))
                           (height (viewport-height viewport))
                           (center-x (viewport-center-x viewport))
                           (center-y (viewport-center-y viewport))
                           (scale (viewport-scale viewport)))
  "Return a viewport like another with some of its numbers changed.

VIEWPORT: the viewport.
X: new x on the sheet.
Y: new y on the sheet.
WIDTH: new width on the sheet.
HEIGHT: new height on the sheet.
CENTER-X: new x of the drawing at its center.
CENTER-Y: new y of the drawing at its center.
SCALE: new scale."
  (make-viewport :x x
                 :y y
                 :width width
                 :height height
                 :center-x center-x
                 :center-y center-y
                 :scale scale))

(defun viewport-window (viewport)
  "Return the part of the drawing that a viewport shows, as bounds in the drawing.

VIEWPORT: the viewport."
  (let ((half-width (/ (viewport-width viewport) (* 2 (viewport-scale viewport))))
        (half-height (/ (viewport-height viewport) (* 2 (viewport-scale viewport)))))
    (geo:make-bounds (- (viewport-center-x viewport) half-width)
                     (- (viewport-center-y viewport) half-height)
                     (+ (viewport-center-x viewport) half-width)
                     (+ (viewport-center-y viewport) half-height))))

;;; Sheets

(defstruct (sheet (:constructor %make-sheet (name
                                             paper
                                             landscape
                                             fields
                                             viewports))
                  (:copier nil))
  "A page that is printed.

NAME: what it is called in the list of sheets.
PAPER: a keyword of *SHEET-PAPERS*.
LANDSCAPE: true when the paper lies, with its long side across.
FIELDS: the title block, as an alist from the keys of *TITLE-FIELD-KEYS* to text, with every key in it.
VIEWPORTS: the viewports, the first at the bottom."
  (name "" :type string :read-only t)
  (paper :a3 :type keyword :read-only t)
  (landscape t :read-only t)
  (fields nil :type list :read-only t)
  (viewports nil :type list :read-only t))

(defun clean-title-fields (fields)
  "Return a title block with every field in it, in order, with what FIELDS has and an empty text for the rest. Signals
EDIT-REFUSED when a text is too long or has a line break.

FIELDS: an alist from keys to text, or NIL."
  (loop for key in *title-field-keys*
        collect (let ((text (or (cdr (assoc key fields)) "")))
                  (unless (stringp text)
                    (refuse-edit "A field of the title block is text."))
                  (when (> (length text) +maximum-title-field-length+)
                    (refuse-edit "A field of the title block has at most ~d characters." +maximum-title-field-length+))
                  (when (find-if (lambda (character) (< (char-code character) 32)) text)
                    (refuse-edit "A field of the title block cannot have a line break."))
                  (cons key (string-trim " " text)))))

(defun make-sheet (&key (name "Sheet")
                        (paper :a3)
                        (landscape t)
                        fields
                        viewports)
  "Create a sheet. Signals EDIT-REFUSED, with the reason, when it makes no sense.

NAME: what it is called, not empty.
PAPER: a keyword of *SHEET-PAPERS*.
LANDSCAPE: true for paper lying down.
FIELDS: the title block, an alist from keys to text; the keys it lacks are empty.
VIEWPORTS: list of viewports."
  (let ((clean-name (string-trim " " name)))
    (when (zerop (length clean-name))
      (refuse-edit "A sheet needs a name."))
    (when (> (length clean-name) +maximum-sheet-name-length+)
      (refuse-edit "A sheet name has at most ~d characters." +maximum-sheet-name-length+))
    (unless (member paper *sheet-papers*)
      (refuse-edit "That is not a paper size; use one of A4, A3, A2, A1, A0 or Letter."))
    (when (> (length viewports) +maximum-viewports+)
      (refuse-edit "A sheet may have at most ~d viewports." +maximum-viewports+))
    (unless (every #'viewport-p viewports)
      (refuse-edit "The viewports of a sheet are viewports."))
    (%make-sheet clean-name
                 paper
                 (and landscape t)
                 (clean-title-fields fields)
                 (copy-list viewports))))

(defun sheet-with (sheet
                   &key (name (sheet-name sheet))
                        (paper (sheet-paper sheet))
                        (landscape (sheet-landscape sheet))
                        (fields (sheet-fields sheet))
                        (viewports (sheet-viewports sheet)))
  "Return a sheet like another with some of it changed.

SHEET: the sheet.
NAME: the new name.
PAPER: the new paper.
LANDSCAPE: whether the paper lies.
FIELDS: the new title block, in whole, or the old one with some fields changed when it is an alist.
VIEWPORTS: the new list of viewports."
  (make-sheet :name name
              :paper paper
              :landscape landscape
              :fields (let ((merged (copy-alist (sheet-fields sheet))))
                        (dolist (entry fields merged)
                          (setf (cdr (assoc (car entry) merged)) (cdr entry))))
              :viewports viewports))

(defun sheet-field (sheet
                    key)
  "Return what a field of the title block says.

SHEET: the sheet.
KEY: a key of *TITLE-FIELD-KEYS*."
  (or (cdr (assoc key (sheet-fields sheet))) ""))

;;; The sheets of a drawing

(defun sheet-count (drawing)
  "Return how many sheets the drawing has.

DRAWING: the drawing."
  (length (drawing-sheets drawing)))

(defun sheet-at (drawing
                 index)
  "Return the sheet at a position, from 0, or NIL when there is none.

DRAWING: the drawing.
INDEX: its position."
  (nth index (drawing-sheets drawing)))

(defun set-sheets (drawing
                   sheets)
  "Replace the sheets of the drawing, which counts as a change to it. Signals EDIT-REFUSED when there are too many.

DRAWING: the drawing.
SHEETS: the list of sheets, in order."
  (when (> (length sheets) +maximum-sheets+)
    (refuse-edit "A drawing may have at most ~d sheets." +maximum-sheets+))
  (setf (drawing-sheets drawing) (copy-list sheets))
  (touch-drawing drawing)
  sheets)

(defun add-sheet (drawing
                  sheet)
  "Put a sheet after the others. Returns its position.

DRAWING: the drawing.
SHEET: the sheet."
  (set-sheets drawing (append (drawing-sheets drawing) (list sheet)))
  (1- (sheet-count drawing)))

(defun replace-sheet (drawing
                      index
                      sheet)
  "Put a sheet in place of the one at a position. Signals EDIT-REFUSED when there is none there.

DRAWING: the drawing.
INDEX: the position.
SHEET: the new sheet."
  (unless (< -1 index (sheet-count drawing))
    (refuse-edit "There is no sheet ~d." (1+ index)))
  (set-sheets drawing (substitute sheet (nth index (drawing-sheets drawing)) (drawing-sheets drawing) :count 1))
  sheet)

(defun remove-sheet (drawing
                     index)
  "Take away the sheet at a position. Signals EDIT-REFUSED when there is none there.

DRAWING: the drawing.
INDEX: the position."
  (unless (< -1 index (sheet-count drawing))
    (refuse-edit "There is no sheet ~d." (1+ index)))
  (set-sheets drawing (append (subseq (drawing-sheets drawing) 0 index)
                              (subseq (drawing-sheets drawing) (1+ index))))
  nil)

(defun add-viewport (drawing
                     index
                     viewport)
  "Put a viewport on a sheet, above the others. Returns the position of the viewport on the sheet.

DRAWING: the drawing.
INDEX: the position of the sheet.
VIEWPORT: the viewport."
  (let ((sheet (sheet-at drawing index)))
    (unless sheet
      (refuse-edit "There is no sheet ~d." (1+ index)))
    (replace-sheet drawing
                   index
                   (sheet-with sheet :viewports (append (sheet-viewports sheet) (list viewport))))
    (1- (length (sheet-viewports (sheet-at drawing index))))))

(defun replace-viewport (drawing
                         index
                         position
                         viewport)
  "Put a viewport in place of another on a sheet.

DRAWING: the drawing.
INDEX: the position of the sheet.
POSITION: the position of the viewport on it.
VIEWPORT: the new viewport."
  (let ((sheet (sheet-at drawing index)))
    (unless (and sheet (< -1 position (length (sheet-viewports sheet))))
      (refuse-edit "There is no such viewport."))
    (replace-sheet drawing
                   index
                   (sheet-with sheet
                               :viewports (substitute viewport
                                                      (nth position (sheet-viewports sheet))
                                                      (sheet-viewports sheet)
                                                      :count 1)))
    viewport))

(defun remove-viewport (drawing
                        index
                        position)
  "Take a viewport away from a sheet.

DRAWING: the drawing.
INDEX: the position of the sheet.
POSITION: the position of the viewport on it."
  (let ((sheet (sheet-at drawing index)))
    (unless (and sheet (< -1 position (length (sheet-viewports sheet))))
      (refuse-edit "There is no such viewport."))
    (replace-sheet drawing
                   index
                   (sheet-with sheet
                               :viewports (append (subseq (sheet-viewports sheet) 0 position)
                                                  (subseq (sheet-viewports sheet) (1+ position)))))
    nil))
