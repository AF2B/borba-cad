(in-package #:borba-cad/tests)

(def-suite shape-library
  :description "The library of shapes: curves cut into segments, SVG files read as outlines, the acanthus leaves and
the blocks they are placed as."
  :in all)

(in-suite shape-library)

;;; Helpers

(defun path-points (text)
  "Return the points of the first contour of the SVG path TEXT.

TEXT: the data of a path."
  (shapes:contour-points (first (shapes:parse-svg-path text))))

(defun points-close-p (expected
                       actual
                       &optional (tolerance 1d-6))
  "Return true when two points agree to a tolerance.

EXPECTED: a cons of x and y.
ACTUAL: a cons of x and y.
TOLERANCE: how far apart they may be."
  (and (< (abs (- (car expected) (car actual))) tolerance)
       (< (abs (- (cdr expected) (cdr actual))) tolerance)))

(defun segments-cross-p (a
                         b
                         c
                         d)
  "Return true when the segment from A to B and the segment from C to D cross each other properly, not just touch.

A: a cons of x and y.
B: a cons of x and y.
C: a cons of x and y.
D: a cons of x and y."
  (flet ((side (p q r)
           (- (* (- (car q) (car p)) (- (cdr r) (cdr p)))
              (* (- (cdr q) (cdr p)) (- (car r) (car p))))))
    (let ((d1 (side a b c))
          (d2 (side a b d))
          (d3 (side c d a))
          (d4 (side c d b)))
      (and (< (* d1 d2) 0d0)
           (< (* d3 d4) 0d0)))))

(defun contour-self-crossings (contour)
  "Return how many pairs of the segments of a closed contour cross each other, which a simple outline never does.

CONTOUR: a contour."
  (let* ((points (coerce (shapes:contour-points contour) 'vector))
         (count (length points))
         (crossings 0))
    (dotimes (i count)
      (loop for j from (+ i 2) below count
            do (unless (and (zerop i) (= j (1- count)))
                 (when (segments-cross-p (aref points i)
                                         (aref points (mod (1+ i) count))
                                         (aref points j)
                                         (aref points (mod (1+ j) count)))
                   (incf crossings)))))
    crossings))

;;; Numbers and curves

(test a-number-of-a-path-is-read-in-every-form-it-comes-in
  (flet ((numbers (text)
           (shapes::read-svg-numbers text)))
    (is (equal '(10d0 -5d0) (numbers "10-5")) "a minus is a new number")
    (is (equal '(1.5d0 0.5d0) (numbers "1.5.5")) "a second point is a new number")
    (is (equal '(100d0) (numbers "1e2")))
    (is (equal '(-0.05d0) (numbers "-.5e-1")))
    (is (equal '(1d0 2d0 3d0) (numbers "1, 2 ,3")))
    (is (null (numbers "abc")))
    (is (equal '(5d0) (numbers "5.")))))

(test a-cubic-curve-is-cut-so-that-it-stays-near-the-true-curve
  (let* ((p0 (cons 0d0 0d0))
         (p1 (cons 0d0 50d0))
         (p2 (cons 50d0 100d0))
         (p3 (cons 100d0 100d0))
         (points (shapes:flatten-cubic p0 p1 p2 p3 0.02d0))
         (deviation 0d0))
    (is (points-close-p p3 (car (last points))) "it ends where the curve does")
    (is (> (length points) 8))
    (let ((previous p0))
      (dolist (point points)
        (let ((middle (cons (/ (+ (car previous) (car point)) 2) (/ (+ (cdr previous) (cdr point)) 2))))
          (loop for step from 0 to 4000
                minimize (let* ((u (/ step 4000d0))
                                (v (- 1 u))
                                (x (+ (* v v v (car p0)) (* 3 v v u (car p1)) (* 3 v u u (car p2)) (* u u u (car p3))))
                                (y (+ (* v v v (cdr p0)) (* 3 v v u (cdr p1)) (* 3 v u u (cdr p2)) (* u u u (cdr p3)))))
                           (sqrt (+ (expt (- x (car middle)) 2) (expt (- y (cdr middle)) 2))))
                  into nearest
                finally (setf deviation (max deviation nearest))))
        (setf previous point)))
    (is (< deviation 0.045d0) "no segment strays far from the curve: ~f" deviation)))

;;; Paths

(test a-path-of-straight-lines-is-read-in-either-coordinates
  (is (equal '((0d0 . 0d0) (10d0 . 0d0) (10d0 . 10d0))
             (path-points "M0 0 L10 0 L10 10")))
  (is (equal '((10d0 . 10d0) (15d0 . 10d0) (15d0 . 15d0))
             (path-points "m10 10 l5 0 0 5")) "relative, and a command repeated by its numbers")
  (is (equal '((0d0 . 0d0) (10d0 . 0d0) (10d0 . 10d0) (0d0 . 10d0))
             (path-points "M0,0 H10 V10 H0")))
  (is (equal '((0d0 . 0d0) (10d0 . 0d0) (20d0 . 0d0))
             (path-points "M0 0 10 0 20 0")) "numbers after a move are lines"))

(test a-closed-subpath-is-a-closed-contour-and-the-next-one-begins-where-it-did
  (let ((contours (shapes:parse-svg-path "M0 0 L10 0 L10 10 Z L0 -5 L5 -5")))
    (is (= 2 (length contours)))
    (is (shapes:contour-closed (first contours)))
    (is (= 3 (length (shapes:contour-points (first contours)))) "the closing point is not repeated")
    (is (not (shapes:contour-closed (second contours))))
    (is (points-close-p '(0d0 . 0d0) (first (shapes:contour-points (second contours)))) "it begins at the start")))

(test two-subpaths-are-two-contours
  (is (= 2 (length (shapes:parse-svg-path "M0 0 L1 0 L1 1 M5 5 L6 5 L6 6")))))

(test curves-in-a-path-end-where-they-say
  (flet ((last-point (text)
           (car (last (path-points text)))))
    (is (points-close-p '(30d0 . 0d0) (last-point "M0 0 C0 10 30 10 30 0")))
    (is (points-close-p '(40d0 . 0d0) (last-point "M0 0 C0 10 30 10 30 0 S 50 -10 40 0")) "a smooth curve")
    (is (points-close-p '(20d0 . 0d0) (last-point "M0 0 Q10 10 20 0")))
    (is (points-close-p '(40d0 . 0d0) (last-point "M0 0 Q10 10 20 0 T40 0")) "a smooth quadratic curve")))

(test an-arc-is-cut-along-the-true-circle
  (let ((points (path-points "M0 0 A10 10 0 0 1 20 0")))
    (is (> (length points) 20))
    (is (points-close-p '(20d0 . 0d0) (car (last points))))
    (is (every (lambda (point) (< (abs (- 10d0 (sqrt (+ (expt (- (car point) 10d0) 2) (expt (cdr point) 2))))) 1d-6))
               points)
        "every point is ten from the middle")
    (is (every (lambda (point) (<= (cdr point) 1d-9)) points)
        "the sweep flag sends it up the page, which in a file is toward smaller y")
    (is (every (lambda (point) (>= (cdr point) -1d-9))
               (path-points "M0 0 A10 10 0 0 0 20 0"))
        "and the other flag the other way")))

(test the-flags-of-an-arc-need-no-space
  (let ((points (path-points "M0 0a10 10 0 0120 0")))
    (is (points-close-p '(20d0 . 0d0) (car (last points))))))

(test a-radius-too-small-is-made-as-large-as-the-ends-need
  (let ((points (path-points "M0 0 A1 1 0 0 1 20 0")))
    (is (points-close-p '(20d0 . 0d0) (car (last points))))
    (is (< (abs (- 10d0 (loop for point in points maximize (abs (cdr point))))) 1d-6) "it becomes the half circle")))

(test data-that-is-not-a-path-is-refused-and-does-not-hang
  (signals base:invalid-input (shapes:parse-svg-path "L 1 2"))
  (signals base:invalid-input (shapes:parse-svg-path "M 1"))
  (signals base:invalid-input (shapes:parse-svg-path "M0 0 Z 5"))
  (signals base:invalid-input (shapes:parse-svg-path "M0 0 L 1 x"))
  (is (null (shapes:parse-svg-path ""))))

;;; Files

(defparameter *square-svg*
  "<?xml version=\"1.0\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<!-- a comment <path d=\"M0 0 L1 1\"/> that is not drawn -->
<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\">
  <defs><path id=\"unused\" d=\"M0 0 L9 9\"/></defs>
  <g transform=\"translate(10,20)\">
    <rect x=\"0\" y=\"0\" width=\"40\" height=\"20\"/>
    <path d=\"M0 0 L40 20\" display=\"none\"/>
  </g>
</svg>"
  "A file with a declaration, a comment, definitions that are not drawn, a hidden path, and a rectangle in a group.")

(test a-file-is-read-as-the-outlines-it-draws
  (let ((contours (shapes:read-svg-contours *square-svg*)))
    (is (= 1 (length contours)) "the comment, the definitions and the hidden path are not drawn")
    (let ((points (shapes:contour-points (first contours))))
      (is (shapes:contour-closed (first contours)))
      (is (points-close-p '(10d0 . -20d0) (first points)) "the transform of the group, and y pointing up")
      (is (points-close-p '(50d0 . -40d0) (third points))))))

(test the-elements-that-are-read-are-the-ones-that-draw-outlines
  (flet ((count-contours (text)
           (length (shapes:read-svg-contours (format nil "<svg>~a</svg>" text)))))
    (is (= 1 (count-contours "<polygon points=\"0,0 10,0 10,10\"/>")))
    (is (= 1 (count-contours "<polyline points=\"0,0 10,0 10,10\"/>")))
    (is (= 1 (count-contours "<line x1=\"0\" y1=\"0\" x2=\"5\" y2=\"5\"/>")))
    (is (= 1 (count-contours "<circle cx=\"5\" cy=\"5\" r=\"3\"/>")))
    (is (= 1 (count-contours "<ellipse cx=\"5\" cy=\"5\" rx=\"3\" ry=\"2\"/>")))
    (is (= 0 (count-contours "<text x=\"1\" y=\"2\">hello</text>")))
    (is (= 0 (count-contours "<g display=\"none\"><rect width=\"1\" height=\"1\"/></g>")) "a hidden group")
    (is (= 1 (count-contours "<g><g><rect width=\"1\" height=\"1\"/></g></g>")))
    (is (= 0 (count-contours "<rect width=\"0\" height=\"5\"/>")))))

(test transforms-compose-and-move-the-points
  (let ((contours (shapes:read-svg-contours
                   (concatenate 'string
                                "<svg><g transform=\"translate(10 0)\"><g transform=\"scale(2)\">"
                                "<polyline points=\"1,1 2,1\"/></g></g></svg>"))))
    (let ((points (shapes:contour-points (first contours))))
      (is (points-close-p '(12d0 . -2d0) (first points)))
      (is (points-close-p '(14d0 . -2d0) (second points)))))
  (let ((rotated (shapes:read-svg-contours "<svg><polyline transform=\"rotate(90)\" points=\"1,0 2,0\"/></svg>")))
    (let ((points (shapes:contour-points (first rotated))))
      (is (points-close-p '(0d0 . -1d0) (first points)) "a quarter turn, with y pointing up"))))

(test a-file-is-a-shape-that-is-100-units-across-its-larger-side-with-its-base-on-the-origin
  (let ((shape (shapes:read-svg-shape *square-svg* "x" "Square")))
    (is (approx= 100d0 (shapes:shape-width shape)))
    (is (approx= 50d0 (shapes:shape-height shape)))
    (multiple-value-bind (min-x min-y max-x max-y) (shapes::shape-bounds shape)
      (declare (ignore max-y))
      (is (approx= 0d0 min-y 1d-9))
      (is (approx= 50d0 max-x))
      (is (approx= -50d0 min-x)))))

(test a-file-that-has-nothing-to-make-a-shape-of-is-refused
  (signals base:invalid-input (shapes:read-svg-shape "<svg><text>hello</text></svg>" "x" "x"))
  (signals base:invalid-input (shapes:read-svg-shape "" "x" "x"))
  (signals base:invalid-input (shapes:read-svg-shape "<svg><path d=\"M0 0\"/></svg>" "x" "x")))

(test a-file-that-is-too-large-is-refused
  (signals base:invalid-input
    (shapes:read-svg-contours (make-string (1+ shapes::+maximum-svg-bytes+) :initial-element #\Space))))

;;; The folder of the user

(defun write-svg (path
                  text)
  "Write an SVG file, making the folders it is in.

PATH: the file.
TEXT: what it holds."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out)))

(test the-folders-of-the-user-are-areas-and-groups-of-shapes
  (with-temporary-directory (directory)
    (write-svg (merge-pathnames "loose.svg" directory) "<svg><rect width=\"4\" height=\"2\"/></svg>")
    (write-svg (merge-pathnames "Marcenaria/Portas/b.svg" directory) "<svg><circle cx=\"1\" cy=\"1\" r=\"1\"/></svg>")
    (write-svg (merge-pathnames "Marcenaria/Portas/a.svg" directory) "<svg><rect width=\"4\" height=\"2\"/></svg>")
    (write-svg (merge-pathnames "Marcenaria/solta.svg" directory) "<svg><rect width=\"1\" height=\"2\"/></svg>")
    (write-svg (merge-pathnames "Marcenaria/Portas/broken.svg" directory) "<svg><text>none</text></svg>")
    (write-svg (merge-pathnames "Marcenaria/Portas/notes.txt" directory) "not a shape")
    (multiple-value-bind (areas problems) (shapes:load-user-areas directory)
      (is (equal '("my-shapes" "Marcenaria") (mapcar #'shapes:area-id areas)))
      (let ((woodwork (second areas)))
        (is (equal '("Marcenaria" "Marcenaria/Portas") (mapcar #'shapes:group-id (shapes:area-groups woodwork)))
            "the files in the area, then the groups")
        (let ((doors (second (shapes:area-groups woodwork))))
          (is (equal '("a" "b") (mapcar #'shapes:shape-name (shapes:group-shapes doors)))
              "in the order of their names")))
      (is (= 1 (length problems)) "the file that has no outline is listed")
      (is (string= "broken.svg" (car (first problems))))
      (is (plusp (length (cdr (first problems))))))))

(test no-folder-is-no-user-shapes
  (is (null (shapes:load-user-areas nil)))
  (with-temporary-directory (directory)
    (is (null (shapes:load-user-areas (merge-pathnames "nothing-here/" directory))))
    (is (null (shapes:load-user-areas directory)))))

;;; The acanthus leaves

(defun acanthus-group ()
  "Return the group of acanthus leaves of the catalog."
  (first (shapes:area-groups (first (shapes:built-in-areas)))))

(test the-catalog-has-an-area-of-ironwork-with-a-group-of-acanthus-leaves
  (let ((areas (shapes:built-in-areas)))
    (is (equal '("ironwork") (mapcar #'shapes:area-id areas)))
    (is (string= "Ironwork" (shapes:area-title (first areas))))
    (is (equal '("acanthus") (mapcar #'shapes:group-id (shapes:area-groups (first areas)))))
    (is (string= "Acanthus leaves" (shapes:group-title (acanthus-group))))
    (is (= 20 (length (shapes:group-shapes (acanthus-group)))) "more than a 4 by 4 grid shows at once")
    (let ((ids (mapcar #'shapes:shape-id (shapes:group-shapes (acanthus-group)))))
      (is (= 20 (length (remove-duplicates ids :test #'string=))) "every shape has an id of its own"))))

(test every-leaf-is-100-units-high-with-its-base-on-the-origin
  (dolist (shape (shapes:group-shapes (acanthus-group)))
    (multiple-value-bind (min-x min-y max-x max-y) (shapes::shape-bounds shape)
      (declare (ignore min-x max-x))
      (is (approx= 0d0 min-y 1d-9) "~a" (shapes:shape-id shape))
      (is (approx= 100d0 max-y 1d-6) "~a" (shapes:shape-id shape)))))

(test the-edge-of-every-leaf-is-a-simple-outline-that-crosses-itself-nowhere
  (dolist (shape (shapes:group-shapes (acanthus-group)))
    (let ((outlines (remove-if-not #'shapes:contour-closed (shapes:shape-contours shape))))
      (is (plusp (length outlines)) "~a has an edge" (shapes:shape-id shape))
      (dolist (outline outlines)
        (is (zerop (contour-self-crossings outline)) "the edge of ~a crosses itself" (shapes:shape-id shape))))))

(test the-name-of-a-leaf-is-translated-with-its-number
  (let ((shape (find "acanthus-03" (shapes:group-shapes (acanthus-group)) :key #'shapes:shape-id :test #'string=)))
    (is (string= "Acanthus leaf 3" (shapes:shape-title shape)))
    (is (string= "Acanthus leaf 3" (shapes:shape-block-name shape)) "the block keeps the English name")
    (i18n:with-language (:pt-br)
      (is (string= "Folha de acanto 3" (shapes:shape-title shape))))))

(test a-shape-is-found-by-its-area-its-group-and-its-id
  (let ((areas (shapes:all-areas)))
    (is (shapes:find-shape areas "ironwork" "acanthus" "acanthus-07"))
    (is (null (shapes:find-shape areas "ironwork" "acanthus" "acanthus-99")))
    (is (null (shapes:find-shape areas "woodwork" "acanthus" "acanthus-07")))))

(test the-areas-of-the-user-are-added-after-those-of-the-program-and-join-one-of-the-same-name
  (let* ((user-group (shapes:make-group "mine" "Mine" (list (shapes:make-shape "m1" "One" '()))))
         (areas (shapes:all-areas (list (shapes:make-area "ironwork" "Ironwork" (list user-group))
                                        (shapes:make-area "wood" "Wood" '())))))
    (is (equal '("ironwork" "wood") (mapcar #'shapes:area-id areas)))
    (is (equal '("acanthus" "mine") (mapcar #'shapes:group-id (shapes:area-groups (first areas)))))
    (is (= 1 (length (shapes:area-groups (first (shapes:built-in-areas)))))
        "the areas of the program are not changed")))

;;; From a shape to a block

(test a-shape-is-a-block-of-polylines
  (dolist (shape (shapes:group-shapes (acanthus-group)))
    (let ((definition (shapes:shape-block-definition shape)))
      (is (string= (shapes:shape-block-name shape) (dom:block-definition-name definition)))
      (is (= (length (shapes:shape-contours shape)) (length (dom:block-definition-entities definition)))
          "a polyline for every contour")
      (is (every (lambda (entity) (eq :polyline (dom:entity-kind entity)))
                 (dom:block-definition-entities definition))))))

(test a-shape-is-placed-in-a-drawing-as-a-block-that-is-100-units-high
  (let ((drawing (dom:make-drawing))
        (shape (first (shapes:group-shapes (acanthus-group)))))
    (dom:define-block drawing (shapes:shape-block-definition shape))
    (let* ((changes (dom:insert-changes drawing (shapes:shape-block-name shape) 500d0 200d0 :scale 2d0))
           (placed (dom:change-after (first changes)))
           (bounds (dom:entity-bounds placed)))
      (is (approx= 200d0 (geo:bounds-height bounds) 1d-6) "twice 100")
      (is (approx= 200d0 (geo:bounds-min-y bounds) 1d-6) "the base is where it was put"))))

(test a-contour-with-points-on-top-of-each-other-or-too-few-is-left-out
  (let* ((shape (shapes:make-shape "t" "Test"
                                   (list (shapes:make-contour (list (cons 0d0 0d0) (cons 0d0 0d0)) nil)
                                         (shapes:make-contour (list (cons 0d0 0d0) (cons 10d0 0d0) (cons 10d0 0d0)
                                                                    (cons 10d0 10d0)) nil)
                                         (shapes:make-contour (list (cons 0d0 0d0) (cons 1d0 1d0) (cons 2d0 0d0)
                                                                    (cons 0d0 0d0)) t))))
         (definition (shapes:shape-block-definition shape)))
    (is (= 2 (length (dom:block-definition-entities definition))) "the first has one point once the copy is dropped")))

;;; In the program

(defun shapes-app (&optional directory)
  "Return an application whose state is kept in DIRECTORY, when there is one.

DIRECTORY: a folder, or NIL."
  (let ((config (quiet-config)))
    (when directory
      (setf (base:config-state-directory config) directory))
    (let ((application (app:make-app config)))
      (set-test-view application)
      application)))

(defun shape-tiles (application)
  "Return the tiles of the shapes that the popover shows.

APPLICATION: the application."
  (remove-if-not (lambda (widget)
                   (and (consp (ui:widget-id widget)) (eq :shape-tile (first (ui:widget-id widget)))))
                 (app:build-widgets application)))

(defun tile-ids (application)
  "Return the ids of the shapes that the tiles of the popover show, in order.

APPLICATION: the application."
  (mapcar (lambda (widget) (fourth (ui:widget-id widget))) (shape-tiles application)))

(defun open-shapes (application)
  "Open the popover of the shapes with the button of the palette.

APPLICATION: the application."
  (click-widget application :shapes))

(test the-shapes-button-opens-a-popover-with-four-rows-of-four-tiles
  (let ((application (shapes-app)))
    (is (find-widget application :shapes) "the palette has the button")
    (is (null (shape-tiles application)) "nothing is open")
    (open-shapes application)
    (is (find-widget application :shapes-card))
    (is (= 16 (length (shape-tiles application))))
    (is (equal '("acanthus-01" "acanthus-02" "acanthus-03" "acanthus-04")
               (subseq (tile-ids application) 0 4)))
    (is (string= "Ironwork / Acanthus leaves" (ui:widget-label (find-widget application :shapes-group))))
    (is (string= "20 shapes" (ui:widget-label (find-widget application :shapes-count))))
    (is (ui:widget-active (find-widget application :shapes)) "the button lights up while it is open")
    (is (find-widget application :shapes-scrollbar) "there are more rows than the four")
    (click-widget application :shapes)
    (is (null (shape-tiles application)) "the button closes it too")))

(test every-tile-shows-the-shape-it-places-and-says-its-name
  (let ((application (shapes-app)))
    (open-shapes application)
    (dolist (tile (shape-tiles application))
      (is (plusp (length (ui:widget-data tile))) "a picture")
      (is (eq :tile (ui:widget-variant tile))))
    (is (string= "Acanthus leaf 1" (ui:widget-hint (first (shape-tiles application)))))
    (i18n:with-language (:pt-br)
      (is (string= "Folha de acanto 1" (ui:widget-hint (first (shape-tiles application))))))))

(test the-wheel-the-arrows-and-the-page-keys-scroll-the-grid-by-rows
  (let ((application (shapes-app)))
    (open-shapes application)
    (let* ((card (find-widget application :shapes-card))
           (x (float (+ (ui:widget-x card) 30) 1d0))
           (y (float (+ (ui:widget-y card) 100) 1d0)))
      (send application (app:make-wheel-event -1d0 x y))
      (is (= 1 (app:app-shape-scroll application)) "a notch down shows the next row")
      (is (equal '("acanthus-05") (list (first (tile-ids application)))))
      (send application (app:make-wheel-event -5d0 x y))
      (is (= 1 (app:app-shape-scroll application)) "it stops at the last row that can be first")
      (is (= 16 (length (shape-tiles application))))
      (send application (app:make-wheel-event 3d0 x y))
      (is (zerop (app:app-shape-scroll application)))
      (press application :down)
      (is (= 1 (app:app-shape-scroll application)))
      (press application :up)
      (is (zerop (app:app-shape-scroll application)))
      (press application :page-down)
      (is (= 1 (app:app-shape-scroll application)) "a page goes as far as there is"))))

(test the-popover-stays-open-for-a-click-on-it-and-closes-for-a-click-elsewhere-or-escape
  (let ((application (shapes-app)))
    (open-shapes application)
    (let* ((card (find-widget application :shapes-card))
           (x (float (+ (ui:widget-x card) (ui:widget-width card) -4) 1d0))
           (y (float (+ (ui:widget-y card) 200) 1d0)))
      (send application
            (app:make-pointer-event :move nil x y)
            (app:make-pointer-event :down :left x y)
            (app:make-pointer-event :up :left x y))
      (is (find-widget application :shapes-card) "a click on the card that is not on a tile does nothing")
      (is (null (entities-of application))))
    (let ((x 600d0) (y 300d0))
      (send application
            (app:make-pointer-event :move nil x y)
            (app:make-pointer-event :down :left x y)
            (app:make-pointer-event :up :left x y))
      (is (null (find-widget application :shapes-card)) "a click on the drawing closes it"))
    (open-shapes application)
    (press application :escape)
    (is (null (find-widget application :shapes-card)))))

(test a-tile-makes-the-shape-the-one-to-place
  (let ((application (shapes-app)))
    (open-shapes application)
    (click-widget application '(:shape-tile "ironwork" "acanthus" "acanthus-03"))
    (is (null (find-widget application :shapes-card)) "the popover gives the canvas back")
    (is (eq :shape (app:tool-kind (app:app-tool application))))
    (is (dom:find-block (app:app-drawing application) "Acanthus leaf 3") "the drawing defines the block")
    (is (search "Acanthus leaf 3" (app:tool-prompt (app:app-tool application) application)))
    (is (search "100 mm" (app:tool-prompt (app:app-tool application) application)))
    (is (not (ui:widget-active (find-widget application :draw))) "it is not a drawing tool")
    (is (ui:widget-active (find-widget application :shapes)))))

(test a-shape-is-placed-where-it-is-clicked-as-large-as-asked-and-one-step-of-undo
  (let ((application (shapes-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (open-shapes application)
    (click-widget application '(:shape-tile "ironwork" "acanthus" "acanthus-01"))
    (type-text application "250")
    (press application :enter)
    (is (search "250 mm" (app:tool-prompt (app:app-tool application) application)))
    (click application 100 200)
    (press application :enter)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (let* ((placed (first (entities-of application)))
           (bounds (dom:entity-bounds placed)))
      (is (eq :insert (dom:entity-kind placed)))
      (is (approx= 250d0 (geo:bounds-height bounds) 1d-6) "the shape is 100 high, scaled to 250")
      (is (approx= 200d0 (geo:bounds-min-y bounds) 1d-6) "its base is where it was clicked"))
    (is (eq :shape (app:tool-kind (app:app-tool application))) "the tool stays for the next one")
    (app:operation-undo application)
    (is (zerop (dom:drawing-entity-count (app:app-drawing application))))
    (is (dom:find-block (app:app-drawing application) "Acanthus leaf 1") "the block stays defined")))

(test a-shape-can-be-flipped-and-turned
  (let ((application (shapes-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (open-shapes application)
    (click-widget application '(:shape-tile "ironwork" "acanthus" "acanthus-08"))
    (press application :f)
    (click application 0 0)
    (type-text application "90")
    (press application :enter)
    (let ((placed (first (entities-of application))))
      (is (dom:insert-flipped placed))
      (is (approx= (/ pi 2) (dom:insert-rotation placed) 1d-9) "a number typed after the click is the turn"))
    (press application :f)
    (click application 500 0)
    (press application :enter)
    (is (not (dom:insert-flipped (first (last (entities-of application))))) "F again flips it back")))

(test a-size-that-is-not-above-zero-is-refused
  (let ((application (shapes-app)))
    (open-shapes application)
    (click-widget application '(:shape-tile "ironwork" "acanthus" "acanthus-01"))
    (type-text application "0")
    (press application :enter)
    (is (search "above zero" (message-text application)))
    (is (eq :error (cdr (app:app-message application))))))

(test a-placed-shape-is-a-block-that-explodes-into-its-outlines-and-spreads
  (let ((application (shapes-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (open-shapes application)
    (click-widget application '(:shape-tile "ironwork" "acanthus" "acanthus-02"))
    (click application 0 0)
    (press application :enter)
    (app:operation-set-tool application :select)
    (click application 0 40)
    (is (= 1 (hash-table-count (app:app-selection application))))
    (app:operation-explode-selection application)
    (is (every (lambda (entity) (eq :polyline (dom:entity-kind entity))) (entities-of application)))
    (is (> (length (entities-of application)) 1) "the outline, the midrib and the veins")))

;;; Groups and the shapes of the user

(defun two-folders-of-shapes (directory)
  "Write two groups of SVG files under DIRECTORY/shapes/.

DIRECTORY: the state directory."
  (write-svg (merge-pathnames "shapes/Woodwork/Doors/a.svg" directory) "<svg><rect width=\"4\" height=\"2\"/></svg>")
  (write-svg (merge-pathnames "shapes/Woodwork/Doors/b.svg" directory) "<svg><circle cx=\"1\" cy=\"1\" r=\"1\"/></svg>")
  (write-svg (merge-pathnames "shapes/Woodwork/Windows/c.svg" directory)
             "<svg><polygon points=\"0,0 4,0 4,3\"/></svg>"))

(test the-arrows-step-from-group-to-group-and-go-round
  (with-temporary-directory (directory)
    (two-folders-of-shapes directory)
    (let ((application (shapes-app directory)))
      (open-shapes application)
      (is (string= "Ironwork / Acanthus leaves" (ui:widget-label (find-widget application :shapes-group))))
      (click-widget application :shapes-next)
      (is (string= "Woodwork / Doors" (ui:widget-label (find-widget application :shapes-group))))
      (is (equal '("Woodwork/Doors/a.svg" "Woodwork/Doors/b.svg") (tile-ids application)))
      (is (string= "2 shapes" (ui:widget-label (find-widget application :shapes-count))))
      (click-widget application :shapes-next)
      (is (string= "Woodwork / Windows" (ui:widget-label (find-widget application :shapes-group))))
      (is (string= "1 shape" (ui:widget-label (find-widget application :shapes-count))))
      (click-widget application :shapes-next)
      (is (string= "Ironwork / Acanthus leaves" (ui:widget-label (find-widget application :shapes-group))) "round")
      (click-widget application :shapes-previous)
      (is (string= "Woodwork / Windows" (ui:widget-label (find-widget application :shapes-group)))))))

(test a-shape-of-the-user-is-placed-like-any-other
  (with-temporary-directory (directory)
    (two-folders-of-shapes directory)
    (let ((application (shapes-app directory)))
      (dom:set-snap-to-grid (app:app-drawing application) nil)
      (open-shapes application)
      (click-widget application :shapes-next)
      (click-widget application '(:shape-tile "Woodwork" "Woodwork/Doors" "Woodwork/Doors/b.svg"))
      (is (dom:find-block (app:app-drawing application) "b"))
      (click application 0 0)
      (press application :enter)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application)))))))

(test an-svg-file-is-added-to-the-shapes-and-the-popover-opens-on-it
  (with-temporary-directory (directory)
    (with-temporary-directory (elsewhere)
      (let ((application (shapes-app directory))
            (file (merge-pathnames "gate.svg" elsewhere)))
        (write-svg file "<svg><path d=\"M0 0 L40 0 L40 20 L0 20 Z\"/></svg>")
        (is (app:import-shape-into-app application (uiop:native-namestring file)))
        (is (probe-file (merge-pathnames "shapes/gate.svg" directory)) "it is copied to the folder of shapes")
        (is (find-widget application :shapes-card) "the popover is open")
        (is (string= "My shapes" (ui:widget-label (find-widget application :shapes-group))))
        (is (equal '("gate.svg") (tile-ids application)))
        (is (search "Shape gate added" (message-text application)))
        (let ((again (shapes-app directory)))
          (open-shapes again)
          (click-widget again :shapes-next)
          (is (equal '("gate.svg") (tile-ids again)) "the next run has it too"))))))

(test a-file-that-cannot-be-a-shape-says-why-and-adds-nothing
  (with-temporary-directory (directory)
    (let ((application (shapes-app directory))
          (file (merge-pathnames "words.svg" directory)))
      (write-svg file "<svg><text>hello</text></svg>")
      (is (null (app:import-shape-into-app application (uiop:native-namestring file))))
      (is (eq :error (cdr (app:app-message application))))
      (is (null (probe-file (merge-pathnames "shapes/words.svg" directory))))
      (is (null (app:import-shape-into-app application
                                           (uiop:native-namestring (merge-pathnames "no.svg" directory)))))))
  (let ((application (shapes-app)))
    (is (null (app:import-shape-into-app application "whatever.svg")) "with nowhere to keep it")))

(test the-add-button-opens-the-dialog-for-svg-files
  (let ((application (shapes-app)))
    (open-shapes application)
    (click-widget application :shapes-add)
    (is (null (find-widget application :shapes-card)))
    (is (string= "Add a shape from an SVG file" (dialog-title-text application)))))

(test the-popover-and-the-tool-speak-every-language
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (shapes-app)))
        (open-shapes application)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the popover in ~a" language)
        (is (null (untranslated-texts (list (ui:widget-hint (find-widget application :shapes-add))
                                            (ui:widget-hint (find-widget application :shapes-next))
                                            (ui:widget-hint (first (shape-tiles application))))
                                      language)))
        (click-widget application '(:shape-tile "ironwork" "acanthus" "acanthus-01"))
        (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language))
            "the prompt in ~a" language)
        (type-text application "0")
        (press application :enter)
        (is (null (untranslated-texts (list (message-text application)) language)) "the error in ~a" language)))))
