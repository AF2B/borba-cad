(in-package #:borba-cad/domain)

;;;; The underlays of a drawing: pictures (a PNG or JPEG file, or a page of a PDF) that sit under the drawing to be
;;;; traced over. An underlay keeps the file it came from, whole, so that a drawing carries it, and where the picture
;;;; stands: the world position of its lower left corner, how many millimetres a pixel is, how far it is turned, how
;;;; opaque it is and whether it shows. It is a value: changing one makes another, and the drawing holds the new
;;;; list. Underlays are saved with the drawing, are not printed or plotted, and are not steps of undo, as the layers
;;;; and the sheets are not. Calibrating an underlay is changing its scale and turn so that two points of the picture
;;;; stand a known distance apart.

(defparameter *underlay-kinds* '(:png :jpeg :pdf)
  "The kinds of file an underlay may come from.")

(defconstant +maximum-underlays+ 8
  "Most underlays a drawing may have.")

(defconstant +maximum-underlay-bytes+ (* 40 1024 1024)
  "Most bytes of picture files the underlays of a drawing may hold together, so that the drawing file stays one that
can be read back.")

(defconstant +maximum-underlay-name-length+ 64
  "Longest name an underlay may have.")

(defconstant +maximum-underlay-scale+ 1d6
  "Largest number of millimetres a pixel of an underlay may be.")

(defconstant +minimum-underlay-scale+ 1d-6
  "Smallest number of millimetres a pixel of an underlay may be.")

(defconstant +maximum-underlay-side+ 65535
  "Longest side of an underlay, in pixels.")

(defconstant +maximum-underlay-page+ 100000
  "Highest page number an underlay may name.")

(defconstant +half-turn-degrees+ 180d0
  "Degrees in half a turn.")

(deftype octet-vector ()
  "A plain vector of bytes."
  '(simple-array (unsigned-byte 8) (*)))

(defstruct (underlay (:constructor %make-underlay (id
                                                   name
                                                   kind
                                                   data
                                                   page
                                                   dpi
                                                   pixel-width
                                                   pixel-height
                                                   x
                                                   y
                                                   rotation
                                                   scale
                                                   opacity
                                                   visible))
                     (:copier nil))
  "A picture under the drawing.

ID: the number that tells it from the other underlays of the drawing; zero until the drawing has it.
NAME: what it is called, as a file name usually is.
KIND: :png, :jpeg or :pdf.
DATA: the bytes of the file it came from.
PAGE: which page, for a PDF; one for the others.
DPI: the resolution a PDF page is drawn at, or NIL when the program chooses it.
PIXEL-WIDTH: width of the picture, in pixels.
PIXEL-HEIGHT: height of the picture, in pixels.
X: x in the drawing, in millimetres, of the lower left corner of the picture.
Y: y of that corner.
ROTATION: degrees the picture is turned counterclockwise about that corner.
SCALE: millimetres of the drawing for a pixel of the picture.
OPACITY: from 0 (clear) to 1 (opaque).
VISIBLE: whether it shows."
  (id 0 :type fixnum :read-only t)
  (name "" :type string :read-only t)
  (kind :png :type keyword :read-only t)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octet-vector :read-only t)
  (page 1 :type fixnum :read-only t)
  (dpi nil :type (or null double-float) :read-only t)
  (pixel-width 1 :type fixnum :read-only t)
  (pixel-height 1 :type fixnum :read-only t)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (rotation 0d0 :type double-float :read-only t)
  (scale 1d0 :type double-float :read-only t)
  (opacity 1d0 :type double-float :read-only t)
  (visible t :type boolean :read-only t))

(defun normalize-degrees (angle)
  "Return an angle in degrees brought into the range above -180 and up to 180.

ANGLE: the angle."
  (let ((reduced (mod angle (* 2 +half-turn-degrees+))))
    (if (> reduced +half-turn-degrees+)
        (- reduced (* 2 +half-turn-degrees+))
        reduced)))

(defun make-underlay (&key (id 0)
                           (name "Underlay")
                           kind
                           data
                           (page 1)
                           dpi
                           pixel-width
                           pixel-height
                           (x 0)
                           (y 0)
                           (rotation 0)
                           (scale 1)
                           (opacity 0.6d0)
                           (visible t))
  "Create an underlay. Signals EDIT-REFUSED, with the reason, when it makes no sense.

ID: its number in the drawing, or zero.
NAME: what it is called, not empty.
KIND: :png, :jpeg or :pdf.
DATA: the bytes of the file.
PAGE: which page, from one.
DPI: the resolution of a PDF page, or NIL.
PIXEL-WIDTH: width of the picture in pixels.
PIXEL-HEIGHT: height of the picture in pixels.
X: x of the lower left corner of the picture, in millimetres.
Y: y of that corner.
ROTATION: degrees counterclockwise.
SCALE: millimetres a pixel stands for.
OPACITY: from 0 to 1.
VISIBLE: true to show it."
  (let ((clean-name (string-trim " " name)))
    (when (zerop (length clean-name))
      (refuse-edit "An underlay needs a name."))
    (when (> (length clean-name) +maximum-underlay-name-length+)
      (refuse-edit "An underlay name has at most ~d characters." +maximum-underlay-name-length+))
    (unless (member kind *underlay-kinds*)
      (refuse-edit "An underlay is a PNG or JPEG picture or a page of a PDF."))
    (unless (and (typep data 'octet-vector) (plusp (length data)))
      (refuse-edit "An underlay needs a picture."))
    (unless (and (integerp pixel-width) (integerp pixel-height)
                 (<= 1 pixel-width +maximum-underlay-side+) (<= 1 pixel-height +maximum-underlay-side+))
      (refuse-edit "An underlay needs a picture of a size the program reads."))
    (unless (and (integerp page) (<= 1 page +maximum-underlay-page+))
      (refuse-edit "The page of an underlay is a number from one."))
    (unless (and (every #'realp (list x y rotation scale opacity))
                 (every (lambda (value) (< (abs value) base:+maximum-coordinate-magnitude+)) (list x y rotation)))
      (refuse-edit "An underlay is made of numbers of a reasonable size."))
    (when dpi
      (unless (and (realp dpi) (< 0 dpi base:+maximum-coordinate-magnitude+))
        (refuse-edit "The resolution of an underlay is a number above zero.")))
    (unless (<= +minimum-underlay-scale+ scale +maximum-underlay-scale+)
      (refuse-edit "The scale of an underlay is out of range."))
    (unless (<= 0 opacity 1)
      (refuse-edit "The opacity of an underlay is from 0 to 1."))
    (%make-underlay id
                    clean-name
                    kind
                    data
                    page
                    (and dpi (float dpi 1d0))
                    pixel-width
                    pixel-height
                    (float x 1d0)
                    (float y 1d0)
                    (normalize-degrees (float rotation 1d0))
                    (float scale 1d0)
                    (float opacity 1d0)
                    (and visible t))))

(defun underlay-with (underlay
                      &key (id (underlay-id underlay))
                           (name (underlay-name underlay))
                           (x (underlay-x underlay))
                           (y (underlay-y underlay))
                           (rotation (underlay-rotation underlay))
                           (scale (underlay-scale underlay))
                           (opacity (underlay-opacity underlay))
                           (visible (underlay-visible underlay)))
  "Return an underlay like another with some of it changed. The picture stays.

UNDERLAY: the underlay.
ID: the new number.
NAME: the new name.
X: new x of the lower left corner.
Y: new y of that corner.
ROTATION: new turn, in degrees.
SCALE: new millimetres a pixel stands for.
OPACITY: new opacity.
VISIBLE: new visibility."
  (make-underlay :id id
                 :name name
                 :kind (underlay-kind underlay)
                 :data (underlay-data underlay)
                 :page (underlay-page underlay)
                 :dpi (underlay-dpi underlay)
                 :pixel-width (underlay-pixel-width underlay)
                 :pixel-height (underlay-pixel-height underlay)
                 :x x
                 :y y
                 :rotation rotation
                 :scale scale
                 :opacity opacity
                 :visible visible))

(defun underlay-with-picture (underlay
                              &key page
                                   (dpi (underlay-dpi underlay))
                                   pixel-width
                                   pixel-height
                                   (scale (underlay-scale underlay)))
  "Return an underlay like another that shows another page, or the same page drawn at another resolution, or whose
picture turned out to be of another size than was thought, standing where the other stood.

UNDERLAY: the underlay.
PAGE: the new page, or NIL for the same.
DPI: the new resolution of a PDF page.
PIXEL-WIDTH: the width of the new picture, or NIL for the same.
PIXEL-HEIGHT: the height of the new picture, or NIL for the same.
SCALE: the new millimetres a pixel stands for."
  (make-underlay :id (underlay-id underlay)
                 :name (underlay-name underlay)
                 :kind (underlay-kind underlay)
                 :data (underlay-data underlay)
                 :page (or page (underlay-page underlay))
                 :dpi dpi
                 :pixel-width (or pixel-width (underlay-pixel-width underlay))
                 :pixel-height (or pixel-height (underlay-pixel-height underlay))
                 :x (underlay-x underlay)
                 :y (underlay-y underlay)
                 :rotation (underlay-rotation underlay)
                 :scale scale
                 :opacity (underlay-opacity underlay)
                 :visible (underlay-visible underlay)))

;;; Where the picture stands

(defun underlay-width (underlay)
  "Return the width of the picture in the drawing, in millimetres.

UNDERLAY: the underlay."
  (* (underlay-pixel-width underlay) (underlay-scale underlay)))

(defun underlay-height (underlay)
  "Return the height of the picture in the drawing, in millimetres.

UNDERLAY: the underlay."
  (* (underlay-pixel-height underlay) (underlay-scale underlay)))

(defun underlay-world-point (underlay
                             column
                             row)
  "Return, as two values, the point of the drawing where a point of the picture stands.

UNDERLAY: the underlay.
COLUMN: the position along the width of the picture, in pixels from its left edge.
ROW: the position down the picture, in pixels from its top edge."
  (let* ((scale (underlay-scale underlay))
         (local-x (* column scale))
         (local-y (* (- (underlay-pixel-height underlay) row) scale))
         (angle (* (underlay-rotation underlay) (/ pi +half-turn-degrees+)))
         (cosine (cos angle))
         (sine (sin angle)))
    (values (+ (underlay-x underlay) (- (* local-x cosine) (* local-y sine)))
            (+ (underlay-y underlay) (+ (* local-x sine) (* local-y cosine))))))

(defun underlay-picture-point (underlay
                               x
                               y)
  "Return, as two values, the point of the picture, in pixels from its left and top edges, that stands at a point of
the drawing.

UNDERLAY: the underlay.
X: x of the point of the drawing.
Y: y of that point."
  (let* ((angle (* (underlay-rotation underlay) (/ pi +half-turn-degrees+)))
         (cosine (cos angle))
         (sine (sin angle))
         (dx (- x (underlay-x underlay)))
         (dy (- y (underlay-y underlay)))
         (local-x (+ (* dx cosine) (* dy sine)))
         (local-y (- (* dy cosine) (* dx sine)))
         (scale (underlay-scale underlay)))
    (values (/ local-x scale)
            (- (underlay-pixel-height underlay) (/ local-y scale)))))

(defun underlay-corners (underlay)
  "Return the four corners of the picture in the drawing, as a list of (x . y), from the lower left one going counter-
clockwise as the picture is seen.

UNDERLAY: the underlay."
  (let ((width (underlay-pixel-width underlay))
        (height (underlay-pixel-height underlay)))
    (loop for (column row) in (list (list 0 height) (list width height) (list width 0) (list 0 0))
          collect (multiple-value-bind (x y)
                      (underlay-world-point underlay
                                            column
                                            row)
                    (cons x y)))))

(defun underlay-bounds (underlay)
  "Return the box in the drawing that holds the picture.

UNDERLAY: the underlay."
  (let ((corners (underlay-corners underlay)))
    (geo:make-bounds (reduce #'min corners :key #'car)
                     (reduce #'min corners :key #'cdr)
                     (reduce #'max corners :key #'car)
                     (reduce #'max corners :key #'cdr))))

;;; Calibrating

(defparameter *calibration-alignments* '(:keep :horizontal :vertical :angle)
  "How a calibration may turn the picture: leave it, make the line between the two points level or upright, or make it
point in a given direction.")

(defun calibrated-underlay (underlay
                            ax
                            ay
                            bx
                            by
                            distance
                            &key (alignment :keep)
                                 (angle 0))
  "Return the underlay scaled, and turned if asked, so that the two points of the drawing where its picture stood
under the pointer, A and B, stand DISTANCE apart. The picture is scaled and turned about A, which stays where it is.
Signals EDIT-REFUSED when the points are one or the distance is not above zero.

UNDERLAY: the underlay.
AX: x of the first point, in the drawing.
AY: y of the first point.
BX: x of the second point.
BY: y of the second point.
DISTANCE: how far apart the points really are, in millimetres.
ALIGNMENT: :KEEP to leave the turn, :HORIZONTAL or :VERTICAL to turn the line between the points to the nearest level or
upright position, :ANGLE to make it point in the direction ANGLE.
ANGLE: that direction, in degrees counterclockwise from the right, for :ANGLE."
  (unless (every #'realp (list ax ay bx by distance angle))
    (refuse-edit "A calibration is made of numbers."))
  (let* ((ax (float ax 1d0))
         (ay (float ay 1d0))
         (bx (float bx 1d0))
         (by (float by 1d0))
         (distance (float distance 1d0))
         (angle (float angle 1d0))
         (current (sqrt (+ (expt (- bx ax) 2) (expt (- by ay) 2)))))
    (unless (base:positive-length-p current)
      (refuse-edit "Pick two different points to calibrate with."))
    (unless (base:positive-length-p distance)
      (refuse-edit "The distance between the points must be above zero."))
    (unless (member alignment *calibration-alignments*)
      (refuse-edit "A calibration keeps the turn, makes the line level or upright, or gives it an angle."))
    (let* ((factor (/ distance current))
           (direction (atan (- by ay) (- bx ax)))
           (target (ecase alignment
                     (:keep direction)
                     (:horizontal (if (<= (abs direction) (/ pi 2)) 0d0 pi))
                     (:vertical (if (>= direction 0) (/ pi 2) (- (/ pi 2))))
                     (:angle (* angle (/ pi +half-turn-degrees+)))))
           (turn (- target direction))
           (cosine (cos turn))
           (sine (sin turn))
           (dx (- (underlay-x underlay) ax))
           (dy (- (underlay-y underlay) ay)))
      (underlay-with underlay
                     :x (+ ax (* factor (- (* dx cosine) (* dy sine))))
                     :y (+ ay (* factor (+ (* dx sine) (* dy cosine))))
                     :rotation (+ (underlay-rotation underlay) (* turn (/ +half-turn-degrees+ pi)))
                     :scale (* (underlay-scale underlay) factor)))))

;;; The underlays of a drawing

(defun underlay-count (drawing)
  "Return how many underlays the drawing has.

DRAWING: the drawing."
  (length (drawing-underlays drawing)))

(defun find-underlay (drawing
                      id)
  "Return the underlay of a drawing that has a number, or NIL.

DRAWING: the drawing.
ID: the number."
  (find id (drawing-underlays drawing) :key #'underlay-id))

(defun underlays-total-bytes (underlays)
  "Return how many bytes of picture files a list of underlays holds.

UNDERLAYS: the list."
  (reduce #'+ underlays :key (lambda (underlay) (length (underlay-data underlay)))))

(defun set-underlays (drawing
                      underlays)
  "Replace the underlays of the drawing, which counts as a change to it. Signals EDIT-REFUSED when there are too many,
their pictures are too large together, or two have one number.

DRAWING: the drawing.
UNDERLAYS: the list of underlays, in order, from the one at the bottom."
  (when (> (length underlays) +maximum-underlays+)
    (refuse-edit "A drawing may have at most ~d underlays." +maximum-underlays+))
  (when (> (underlays-total-bytes underlays) +maximum-underlay-bytes+)
    (refuse-edit "The pictures under a drawing may take at most ~d MiB together."
                 (floor +maximum-underlay-bytes+ (* 1024 1024))))
  (unless (= (length underlays) (length (remove-duplicates underlays :key #'underlay-id)))
    (refuse-edit "Two underlays have the same number."))
  (setf (drawing-underlays drawing) (copy-list underlays)
        (drawing-next-underlay-id drawing) (max (drawing-next-underlay-id drawing)
                                                (1+ (reduce #'max underlays :key #'underlay-id :initial-value 0))))
  (touch-drawing drawing)
  underlays)

(defun add-underlay (drawing
                     underlay)
  "Put an underlay on top of the others, giving it the next number. Returns the underlay as it is kept.

DRAWING: the drawing.
UNDERLAY: the underlay."
  (let ((numbered (underlay-with underlay :id (drawing-next-underlay-id drawing))))
    (set-underlays drawing (append (drawing-underlays drawing) (list numbered)))
    numbered))

(defun replace-underlay (drawing
                         underlay)
  "Put an underlay in place of the one of the same number. Signals EDIT-REFUSED when there is none.

DRAWING: the drawing.
UNDERLAY: the new underlay."
  (unless (find-underlay drawing (underlay-id underlay))
    (refuse-edit "There is no underlay ~d." (underlay-id underlay)))
  (set-underlays drawing
                 (mapcar (lambda (existing)
                           (if (= (underlay-id existing) (underlay-id underlay))
                               underlay
                               existing))
                         (drawing-underlays drawing)))
  underlay)

(defun remove-underlay (drawing
                        id)
  "Take away the underlay that has a number. Signals EDIT-REFUSED when there is none.

DRAWING: the drawing.
ID: the number."
  (unless (find-underlay drawing id)
    (refuse-edit "There is no underlay ~d." id))
  (set-underlays drawing (remove id (drawing-underlays drawing) :key #'underlay-id))
  nil)

(defun underlays-bounds (drawing)
  "Return the box that holds every underlay that shows, or NIL when none does.

DRAWING: the drawing."
  (let ((bounds nil))
    (dolist (underlay (drawing-underlays drawing) bounds)
      (when (underlay-visible underlay)
        (setf bounds (if bounds
                         (geo:bounds-union bounds (underlay-bounds underlay))
                         (underlay-bounds underlay)))))))
