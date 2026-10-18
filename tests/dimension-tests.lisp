(in-package #:borba-cad/tests)

(def-suite round-dimensions
  :description "The radius, diameter and angle dimensions: layout, entities, file, tools, console and languages."
  :in all)

(in-suite round-dimensions)

;;; Helpers

(defun a-radial-dimension (&key (id 1)
                                (center-x 0d0)
                                (center-y 0d0)
                                (radius 1000d0)
                                (angle 0d0)
                                diameter
                                (text-height 100d0)
                                (unit :millimeter)
                                (precision 0))
  "Return a radius dimension, or a diameter one with DIAMETER, of a circle at the origin unless said."
  (dom:make-radial-dimension :id id
                             :layer-id 0
                             :center-x center-x
                             :center-y center-y
                             :radius radius
                             :angle angle
                             :diameter diameter
                             :text-height text-height
                             :unit unit
                             :precision precision))

(defun an-angle-dimension (&key (id 1)
                                (vertex-x 0d0)
                                (vertex-y 0d0)
                                (start-angle 0d0)
                                (end-angle (/ pi 2))
                                (radius 1000d0)
                                (text-height 100d0)
                                (precision 1))
  "Return an angle dimension of a right angle at the origin unless said."
  (dom:make-angular-dimension :id id
                              :layer-id 0
                              :vertex-x vertex-x
                              :vertex-y vertex-y
                              :start-angle start-angle
                              :end-angle end-angle
                              :radius radius
                              :text-height text-height
                              :precision precision))

(defun close-p (expected
                actual
                &optional (tolerance 1d-6))
  "Return true when two numbers agree to TOLERANCE."
  (< (abs (- expected actual)) tolerance))

(defun layout-points (layout)
  "Return every point of the strokes and arrowheads of an annotation layout, as (x . y) conses."
  (let ((points '()))
    (dolist (array (list (geo:annotation-layout-segments layout) (geo:annotation-layout-arrowheads layout)))
      (loop for index from 0 below (length array) by 2
            do (push (cons (aref array index) (aref array (1+ index))) points)))
    points))

;;; The layout of a radius dimension

(test a-radius-leader-runs-from-the-center-through-the-circle-to-a-tail-that-carries-the-text
  (let* ((layout (geo:compute-radial-dimension-layout 0d0 0d0 1000d0 0d0 nil 100d0 5))
         (segments (geo:annotation-layout-segments layout))
         (arrows (geo:annotation-layout-arrowheads layout)))
    (is (= 4 (length segments)) "one stroke")
    (is (and (close-p 0d0 (aref segments 0)) (close-p 0d0 (aref segments 1))) "from the center")
    (is (close-p 1400d0 (aref segments 2)) "to the edge, the gap, the text and the gap")
    (is (= 6 (length arrows)) "one arrowhead")
    (is (and (close-p 1000d0 (aref arrows 0)) (close-p 0d0 (aref arrows 1))) "its tip is on the circle")
    (is (close-p 900d0 (aref arrows 2)) "and it points outward")
    (is (close-p 0d0 (geo:annotation-layout-text-angle layout)))
    (is (close-p 1050d0 (geo:annotation-layout-text-x layout)) "the text starts after the gap")
    (is (plusp (geo:annotation-layout-text-y layout)) "above the leader")
    (is (close-p 300d0 (geo:annotation-layout-text-width layout)))))

(test text-on-a-leader-that-points-left-still-reads-left-to-right
  (let* ((layout (geo:compute-radial-dimension-layout 0d0 0d0 1000d0 pi nil 100d0 5)))
    (is (close-p 0d0 (geo:annotation-layout-text-angle layout)) "not upside down")
    (is (close-p -1350d0 (geo:annotation-layout-text-x layout))
        "it starts at the far end of the tail and runs back toward the circle")
    (is (close-p -1400d0 (aref (geo:annotation-layout-segments layout) 2)))))

(test text-on-a-slanted-leader-follows-it
  (let* ((layout (geo:compute-radial-dimension-layout 0d0 0d0 1000d0 (/ pi 4) nil 100d0 5)))
    (is (close-p (/ pi 4) (geo:annotation-layout-text-angle layout)))
    (let ((flipped (geo:compute-radial-dimension-layout 0d0 0d0 1000d0 (* 5 (/ pi 4)) nil 100d0 5)))
      (is (close-p (/ pi 4) (geo:annotation-layout-text-angle flipped)) "turned over to read upward"))))

(test a-diameter-line-crosses-the-center-with-an-arrow-at-each-end-and-the-text-above-it
  (let* ((layout (geo:compute-radial-dimension-layout 0d0 0d0 1000d0 0d0 t 100d0 5))
         (segments (geo:annotation-layout-segments layout))
         (arrows (geo:annotation-layout-arrowheads layout)))
    (is (equalp #(-1000d0 0d0 1000d0 0d0) (map 'vector (lambda (value) (+ 0d0 (fround value))) segments)))
    (is (= 12 (length arrows)))
    (is (and (close-p 1000d0 (aref arrows 0)) (close-p -1000d0 (aref arrows 6))) "tips on the circle")
    (is (and (close-p 900d0 (aref arrows 2)) (close-p -900d0 (aref arrows 8))) "pointing outward")
    (is (close-p -150d0 (geo:annotation-layout-text-x layout)) "centered")
    (is (plusp (geo:annotation-layout-text-y layout)))))

(test arrows-that-do-not-fit-stand-outside-pointing-in
  (let* ((layout (geo:compute-radial-dimension-layout 0d0 0d0 40d0 0d0 nil 100d0 4))
         (arrows (geo:annotation-layout-arrowheads layout)))
    (is (and (close-p 40d0 (aref arrows 0)) (> (aref arrows 2) 40d0)) "the base is outside the circle")
    (let* ((diameter (geo:compute-radial-dimension-layout 0d0 0d0 40d0 0d0 t 100d0 4))
           (both (geo:annotation-layout-arrowheads diameter)))
      (is (> (aref both 2) 40d0))
      (is (< (aref both 8) -40d0)))))

(test the-bounds-of-a-layout-hold-every-part
  (dolist (layout (list (geo:compute-radial-dimension-layout 10d0 20d0 500d0 0.7d0 nil 80d0 5)
                        (geo:compute-radial-dimension-layout 10d0 20d0 500d0 3.9d0 t 80d0 6)
                        (geo:compute-angular-dimension-layout 5d0 5d0 0.2d0 4d0 700d0 80d0 5)))
    (let ((bounds (geo:annotation-layout-bounds layout)))
      (dolist (point (layout-points layout))
        (is (geo:bounds-contains-point-p bounds (car point) (cdr point)))))))

;;; The layout of an angle dimension

(test an-angle-dimension-is-two-arms-and-an-arc-between-them
  (let* ((layout (geo:compute-angular-dimension-layout 0d0 0d0 0d0 (/ pi 2) 1000d0 100d0 5))
         (segments (geo:annotation-layout-segments layout))
         (arrows (geo:annotation-layout-arrowheads layout)))
    (is (= (+ 8 (* 4 23)) (length segments)) "two extension lines and 23 segments of four degrees or less")
    (is (and (close-p 40d0 (aref segments 0)) (close-p 1060d0 (aref segments 2)))
        "the first extension line starts near the vertex and runs a little past the arc")
    (is (and (close-p 1000d0 (aref segments 8)) (close-p 0d0 (aref segments 9))) "the arc starts on the first arm")
    (let ((last (- (length segments) 4)))
      (is (and (close-p 0d0 (aref segments (+ last 2))) (close-p 1000d0 (aref segments (+ last 3))))
          "and ends on the second"))
    (loop for index from 8 below (length segments) by 2
          do (is (close-p 1000d0 (geo:distance 0d0 0d0 (aref segments index) (aref segments (1+ index))))
                 "every point of the arc is on it"))
    (is (= 12 (length arrows)))
    (is (and (close-p 1000d0 (aref arrows 0)) (close-p 100d0 (aref arrows 3))) "the first arrow points along the arc")
    (is (and (close-p 0d0 (aref arrows 6)) (close-p 1000d0 (aref arrows 7))) "the second is at the other end")
    (is (close-p 100d0 (aref arrows 8)) "and turns back along the arc")))

(test the-text-of-an-angle-sits-beyond-the-middle-of-the-arc-and-is-level
  (let* ((layout (geo:compute-angular-dimension-layout 0d0 0d0 0d0 (/ pi 2) 1000d0 100d0 5))
         (width (geo:annotation-layout-text-width layout))
         (center-x (+ (geo:annotation-layout-text-x layout) (/ width 2)))
         (center-y (+ (geo:annotation-layout-text-y layout)
                      (* 0.5d0 100d0 (- geo:+text-ascent-ratio+ geo:+text-descent-ratio+)))))
    (is (close-p 0d0 (geo:annotation-layout-text-angle layout)))
    (is (close-p center-x center-y 1d-6) "on the bisector")
    (is (> (geo:distance 0d0 0d0 center-x center-y) 1000d0) "beyond the arc")))

(test an-angle-that-passes-through-zero-sweeps-the-short-way
  (let* ((layout (geo:compute-angular-dimension-layout 0d0 0d0 6d0 0.5d0 1000d0 100d0 5))
         (segments (geo:annotation-layout-segments layout))
         (last (- (length segments) 4)))
    (is (= (+ 8 (* 4 12)) (length segments)) "0.78 radians is 12 segments")
    (is (close-p (cos 0.5d0) (/ (aref segments (+ last 2)) 1000d0)))
    (is (close-p (sin 0.5d0) (/ (aref segments (+ last 3)) 1000d0)))))

(test a-big-angle-has-a-segment-for-every-four-degrees
  (let ((layout (geo:compute-angular-dimension-layout 0d0 0d0 0d0 (* 3 (/ pi 2)) 1000d0 100d0 6)))
    (is (= (+ 8 (* 4 68)) (length (geo:annotation-layout-segments layout))) "270 degrees is 68 segments")))

;;; Picking and crossing by layout

(test strokes-and-text-are-pickable
  (let ((layout (geo:compute-radial-dimension-layout 0d0 0d0 1000d0 0d0 nil 100d0 5)))
    (is (close-p 0d0 (geo:annotation-pick-distance layout 500d0 0d0)) "on the leader")
    (is (close-p 0d0 (geo:annotation-pick-distance layout 1100d0 50d0)) "on the text")
    (is (close-p 300d0 (geo:annotation-pick-distance layout 500d0 300d0)) "away from everything")
    (is (geo:annotation-touches-bounds-p layout (geo:make-bounds 400d0 -10d0 600d0 10d0)))
    (is (geo:annotation-touches-bounds-p layout (geo:make-bounds 1100d0 20d0 1200d0 60d0)) "over the text")
    (is (not (geo:annotation-touches-bounds-p layout (geo:make-bounds 400d0 200d0 600d0 300d0))))))

;;; The entities

(test a-radius-dimension-shows-r-and-the-radius-a-diameter-one-shows-the-sign-and-twice-that
  (is (string= "R1000" (dom:radial-dimension-text (a-radial-dimension))))
  (is (string= (format nil "~a2000" (string (code-char 216)))
               (dom:radial-dimension-text (a-radial-dimension :diameter t))))
  (is (string= "R1.25" (dom:radial-dimension-text (a-radial-dimension :radius 1250d0 :unit :meter :precision 2))))
  (is (eq :radial-dimension (dom:entity-kind (a-radial-dimension))))
  (is (geo:bounds-contains-bounds-p (dom:entity-bounds (a-radial-dimension))
                                    (geo:annotation-layout-bounds (dom:radial-dimension-layout (a-radial-dimension))))))

(test a-radial-dimension-keeps-its-angle-reduced-and-its-flag-a-boolean
  (is (close-p (- (* 2 pi) 0.5d0) (dom:radial-dimension-angle (a-radial-dimension :angle -0.5d0))))
  (is (eq t (dom:radial-dimension-diameter (a-radial-dimension :diameter :yes))))
  (is (eq nil (dom:radial-dimension-diameter (a-radial-dimension)))))

(test a-radial-dimension-refuses-what-cannot-be-drawn
  (signals-invalid-entity :radial-dimension (a-radial-dimension :radius 0d0))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :text-height 0d0))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :unit :furlong))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :precision -1))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :precision (1+ dom:+maximum-dimension-precision+)))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :center-x (* 2 base:+maximum-coordinate-magnitude+)))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :center-x (- base:+maximum-coordinate-magnitude+ 10d0)
                                                                :radius 1000d0))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :angle 1d9))
  (signals-invalid-entity :radial-dimension (a-radial-dimension :id 0)))

(test an-angle-dimension-shows-the-angle-in-degrees
  (is (string= (format nil "90.0~a" (string (code-char 176))) (dom:angular-dimension-text (an-angle-dimension))))
  (is (string= (format nil "45~a" (string (code-char 176)))
               (dom:angular-dimension-text (an-angle-dimension :end-angle (/ pi 4) :precision 0))))
  (is (close-p (/ pi 2) (dom:angular-dimension-sweep (an-angle-dimension))))
  (is (close-p (* 3 (/ pi 2)) (dom:angular-dimension-sweep (an-angle-dimension :start-angle (/ pi 2) :end-angle 0d0)))
      "counter-clockwise from the first arm to the second, so the other way round it is the rest of the turn")
  (is (eq :angular-dimension (dom:entity-kind (an-angle-dimension)))))

(test an-angle-dimension-refuses-what-cannot-be-drawn
  (signals-invalid-entity :angular-dimension (an-angle-dimension :end-angle 0d0))
  (signals-invalid-entity :angular-dimension (an-angle-dimension :end-angle (* 2 pi)))
  (signals-invalid-entity :angular-dimension (an-angle-dimension :radius 0d0))
  (signals-invalid-entity :angular-dimension (an-angle-dimension :text-height 0d0))
  (signals-invalid-entity :angular-dimension (an-angle-dimension :precision 7))
  (signals-invalid-entity :angular-dimension (an-angle-dimension :vertex-y (* 2 base:+maximum-coordinate-magnitude+)))
  (signals-invalid-entity :angular-dimension (an-angle-dimension :start-angle 1d9)))

(test the-inspector-lists-and-edits-what-defines-each-dimension
  (let ((radial (a-radial-dimension :angle 0.5d0)))
    (is (equal '(:center-x :center-y :radius :angle :diameter :text-height :unit :precision)
               (mapcar #'dom:property-key (dom:entity-properties radial))))
    (let ((bigger (dom:entity-with-property radial :radius 2000d0)))
      (is (= 2000d0 (dom:radial-dimension-radius bigger)))
      (is (string= "R2000" (dom:radial-dimension-text bigger))))
    (is (dom:radial-dimension-diameter (dom:entity-with-property radial :diameter t)))
    (is (close-p 1d0 (dom:radial-dimension-angle (dom:entity-with-property radial :angle 1d0))))
    (is (= 1000d0 (dom:derived-value-value (first (dom:entity-derived-values radial)))))
    (is (= 2000d0 (dom:derived-value-value (first (dom:entity-derived-values (a-radial-dimension :diameter t))))))
    (signals base:invalid-entity (dom:entity-with-property radial :radius 0d0)))
  (let ((angular (an-angle-dimension)))
    (is (equal '(:vertex-x :vertex-y :start-angle :end-angle :radius :text-height :precision)
               (mapcar #'dom:property-key (dom:entity-properties angular))))
    (is (close-p (/ pi 4) (dom:angular-dimension-sweep (dom:entity-with-property angular :end-angle (/ pi 4)))))
    (is (close-p (/ pi 2) (dom:derived-value-value (first (dom:entity-derived-values angular)))))
    (signals base:invalid-entity (dom:entity-with-property angular :end-angle 0d0))))

(test dimensions-move-change-layer-and-take-new-identities
  (let ((radial (a-radial-dimension))
        (angular (an-angle-dimension)))
    (let ((moved (dom:entity-translated radial 100d0 -50d0)))
      (is (and (= 100d0 (dom:radial-dimension-center-x moved)) (= -50d0 (dom:radial-dimension-center-y moved)))))
    (let ((moved (dom:entity-translated angular 100d0 -50d0)))
      (is (and (= 100d0 (dom:angular-dimension-vertex-x moved)) (= -50d0 (dom:angular-dimension-vertex-y moved)))))
    (is (= 3 (dom:entity-layer-id (dom:entity-with-layer radial 3))))
    (is (= 9 (dom:entity-id (dom:entity-with-identity angular 9 0))))))

(test the-points-to-snap-to-are-the-center-and-the-edge-and-the-vertex-and-the-ends-of-the-arc
  (let ((points (dom:entity-snap-points (a-radial-dimension :angle (/ pi 2)))))
    (is (= 2 (length points)))
    (is (equal '(0d0 0d0 :center) (list (first (first points)) (second (first points)) (third (first points)))))
    (is (and (close-p 0d0 (first (second points))) (close-p 1000d0 (second (second points))))))
  (let ((points (dom:entity-snap-points (an-angle-dimension))))
    (is (= 3 (length points)))
    (is (and (close-p 1000d0 (first (second points))) (close-p 0d0 (second (second points)))))
    (is (and (close-p 0d0 (first (third points))) (close-p 1000d0 (second (third points)))))))

(test a-dimension-is-picked-on-its-strokes-and-its-text
  (let ((radial (a-radial-dimension))
        (angular (an-angle-dimension)))
    (is (close-p 0d0 (dom:entity-pick-distance radial 500d0 0d0)))
    (is (plusp (dom:entity-pick-distance radial 500d0 400d0)))
    (is (close-p 0d0 (dom:entity-pick-distance angular 1000d0 0d0)) "on the arc")
    (is (< (dom:entity-pick-distance angular (* 1000d0 (cos 0.5d0)) (* 1000d0 (sin 0.5d0))) 1d0)
        "mid-arc, within the sagitta of a four-degree chord")
    (is (plusp (dom:entity-pick-distance angular 200d0 200d0)) "inside the angle, away from the arc")
    (is (dom:entity-touches-bounds-p radial (geo:make-bounds 400d0 -10d0 600d0 10d0)))
    (is (not (dom:entity-touches-bounds-p angular (geo:make-bounds 100d0 100d0 300d0 300d0))))))

(test the-grips-turn-the-leader-reshape-the-angle-and-move-the-whole
  (let* ((radial (a-radial-dimension))
         (grips (dom:entity-grips radial)))
    (is (equal '(:direction :move) (mapcar #'dom:grip-id grips)))
    (let ((turned (dom:entity-with-grip radial (first grips) 0d0 500d0)))
      (is (close-p (/ pi 2) (dom:radial-dimension-angle turned))))
    (let ((moved (dom:entity-with-grip radial (second grips) 300d0 400d0)))
      (is (and (= 300d0 (dom:radial-dimension-center-x moved)) (= 400d0 (dom:radial-dimension-center-y moved))))))
  (let* ((angular (an-angle-dimension))
         (grips (dom:entity-grips angular)))
    (is (equal '(:start :end :radius :move) (mapcar #'dom:grip-id grips)))
    (is (close-p (/ pi 4) (dom:angular-dimension-sweep (dom:entity-with-grip angular (first grips) 500d0 500d0)))
        "the first arm turned to 45 degrees leaves 45")
    (is (close-p (/ pi 4) (dom:angular-dimension-end-angle (dom:entity-with-grip angular (second grips) 500d0 500d0))))
    (is (close-p 1500d0 (dom:angular-dimension-radius (dom:entity-with-grip angular (third grips) 0d0 1500d0))))
    (let ((moved (dom:entity-with-grip angular (fourth grips) 10d0 20d0)))
      (is (and (= 10d0 (dom:angular-dimension-vertex-x moved)) (= 20d0 (dom:angular-dimension-vertex-y moved)))))))

;;; Rotating, mirroring and scaling

(test rotating-turns-the-leader-and-the-arms
  (let ((radial (dom:entity-rotated (a-radial-dimension :center-x 1000d0) 0d0 0d0 (/ pi 2))))
    (is (and (close-p 0d0 (dom:radial-dimension-center-x radial))
             (close-p 1000d0 (dom:radial-dimension-center-y radial))))
    (is (close-p (/ pi 2) (dom:radial-dimension-angle radial))))
  (let ((angular (dom:entity-rotated (an-angle-dimension) 0d0 0d0 (/ pi 2))))
    (is (close-p (/ pi 2) (dom:angular-dimension-start-angle angular)))
    (is (close-p pi (dom:angular-dimension-end-angle angular)))
    (is (close-p (/ pi 2) (dom:angular-dimension-sweep angular)) "the angle is the same")))

(test mirroring-reflects-the-leader-and-keeps-the-angle-counter-clockwise
  (let ((radial (dom:entity-mirrored (a-radial-dimension :angle 0.5d0) 0d0 0d0 1d0 0d0)))
    (is (close-p (- (* 2 pi) 0.5d0) (dom:radial-dimension-angle radial))))
  (let ((angular (dom:entity-mirrored (an-angle-dimension :start-angle 0.2d0 :end-angle 1.2d0) 0d0 0d0 1d0 0d0)))
    (is (close-p 1d0 (dom:angular-dimension-sweep angular)) "a reflected angle is as wide")
    (is (close-p (- (* 2 pi) 1.2d0) (dom:angular-dimension-start-angle angular)) "and the arms swap")))

(test scaling-multiplies-the-radius-the-arc-and-the-text
  (let ((radial (dom:entity-scaled (a-radial-dimension) 0d0 0d0 2d0)))
    (is (= 2000d0 (dom:radial-dimension-radius radial)))
    (is (= 200d0 (dom:radial-dimension-text-height radial)))
    (is (string= "R2000" (dom:radial-dimension-text radial))))
  (let ((angular (dom:entity-scaled (an-angle-dimension :vertex-x 10d0) 0d0 0d0 3d0)))
    (is (= 3000d0 (dom:angular-dimension-radius angular)))
    (is (= 30d0 (dom:angular-dimension-vertex-x angular)))
    (is (= 300d0 (dom:angular-dimension-text-height angular)))))

;;; The file

(defun dimension-drawing ()
  "Return a drawing with a radius, a diameter and an angle dimension."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (a-radial-dimension :id 1 :angle 0.5d0)
                              (a-radial-dimension :id 2 :center-x 5000d0 :diameter t :unit :meter :precision 2)
                              (an-angle-dimension :id 3 :vertex-x -3000d0 :start-angle 0.3d0 :end-angle 4d0))))
    drawing))

(test the-dimensions-are-written-and-read-back-exactly
  (let* ((drawing (dimension-drawing))
         (text (persist:drawing-to-string drawing))
         (back (persist:parse-drawing-text text)))
    (is (search (concatenate 'string
                             "(radial-dimension (id 1) (layer 0) (center-x 0.0) (center-y 0.0) (radius 1000.0) "
                             "(angle 0.5) (diameter nil)")
                text))
    (is (search "(diameter t) (text-height 100.0) (unit meter) (precision 2))" text))
    (is (search "(angular-dimension (id 3) (layer 0) (vertex-x -3000.0)" text))
    (is (equal (entity-values drawing) (entity-values back)))
    (is (string= text (persist:drawing-to-string back)))))

(test the-file-says-the-newest-version-and-an-older-one-cannot-hold-the-dimensions
  (let ((text (persist:drawing-to-string (dimension-drawing))))
    (is (search (format nil "(format-version ~d)" persist:+supported-format-version+) text))
    (dolist (version '(1 2))
      (let ((old (replace-once-text text
                                    (format nil "(format-version ~d)" persist:+supported-format-version+)
                                    (format nil "(format-version ~d)" version))))
        (handler-case (progn (persist:parse-drawing-text old)
                             (fail "a version ~d file with a dimension was accepted" version))
          (base:drawing-file-content-error (condition)
            (is (search "unknown entity type" (princ-to-string condition)))))))))

(test the-reader-refuses-a-dimension-that-is-not-valid
  (let ((text (persist:drawing-to-string (dimension-drawing))))
    (dolist (broken (list (replace-once-text text "(radius 1000.0)" "(radius 0.0)")
                          (replace-once-text text "(diameter nil)" "(diameter 3)")
                          (replace-once-text text "(precision 2)" "(precision 9)")
                          (replace-once-text text "(start-angle 0.3) (end-angle 4.0)"
                                             "(start-angle 1.0) (end-angle 1.0)")
                          (replace-once-text text "(vertex-x " "(vertex-q ")))
      (signals base:drawing-file-error (persist:parse-drawing-text broken)))))

(test a-version-2-file-is-read-and-saved-as-the-newest-version
  (let* ((fixture (fixture-text "v2-plan.bcad"))
         (drawing (persist:parse-drawing-text fixture))
         (written (persist:drawing-to-string drawing)))
    (is (search "(format-version 2)" fixture))
    (is (= 9 (dom:drawing-entity-count drawing)))
    (is (search (format nil "(format-version ~d)" persist:+supported-format-version+) written))
    (is (string= (replace-once-text fixture
                                    "(format-version 2)"
                                    (format nil "(format-version ~d)" persist:+supported-format-version+))
                 written)
        "everything else is as it was")
    (is (equal (entity-values drawing) (entity-values (persist:parse-drawing-text written))))))

;;; The scene

(test the-dimensions-are-drawn-with-their-text
  (let ((application (make-test-app :drawing (dimension-drawing))))
    (set-test-view application :scale 0.05d0 :center-x 0d0 :center-y 0d0)
    (app:app-render application)
    (let ((texts (all-text application)))
      (is (member "R1000" texts :test #'string=))
      (is (member (format nil "~a2.00" (string (code-char 216))) texts :test #'string=))
      (is (member (format nil "~,1f~a" (* 180 (/ (- 4d0 0.3d0) pi)) (string (code-char 176))) texts :test #'string=)))
    (is (= 3 (scene:scene-result-drawn-entities (app:app-last-scene-result application))))))

;;; DXF

(defun dxf-types (text)
  "Return the entity types of the DXF TEXT, in order."
  (loop for (code value) in (groups-of text)
        when (= 0 code) collect value))

(test a-radius-dimension-is-written-as-a-line-an-arrowhead-and-text
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes (list (a-radial-dimension))))
    (multiple-value-bind (text report) (dxf:export-drawing-text drawing)
      (let ((types (dxf-types text)))
        (is (= 1 (count "LINE" types :test #'string=)))
        (is (= 1 (count "SOLID" types :test #'string=)))
        (is (= 1 (count "TEXT" types :test #'string=))))
      (is (equal '(1 "R1000") (assoc 1 (entity-groups text "TEXT"))))
      (is (find-if (lambda (note) (search "Dimensions" note)) (dxf:report-notes report))))))

(test a-diameter-and-an-angle-are-written-with-their-signs-escaped
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes (list (a-radial-dimension :id 1 :diameter t)
                                                                  (an-angle-dimension :id 2))))
    (let* ((text (dxf:export-drawing-text drawing))
           (types (dxf-types text)))
      (is (= (+ 1 2 23) (count "LINE" types :test #'string=)) "the diameter, and two arms and the arc of the angle")
      (is (= 4 (count "SOLID" types :test #'string=)) "two arrowheads each")
      (is (every (lambda (character) (< (char-code character) 128)) text) "plain ASCII")
      (is (search "\\U+00D82000" text))
      (is (search "90.0\\U+00B0" text)))))

;;; The console

(test the-console-draws-the-three-dimensions
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (is (equal '("1") (ran-ok context "(radius-dimension 0 0 1000 :angle 90)")))
    (is (equal '("2") (ran-ok context "(diameter-dimension (m 5) 0 500 :unit :meter :decimals 2)")))
    (is (equal '("3") (ran-ok context "(angle-dimension 0 0 0 90 1000 :decimals 0)")))
    (let ((radius (stored context 1))
          (diameter (stored context 2))
          (angle (stored context 3)))
      (is (close-p (/ pi 2) (dom:radial-dimension-angle radius)))
      (is (not (dom:radial-dimension-diameter radius)))
      (is (dom:radial-dimension-diameter diameter))
      (is (string= (format nil "~a1.00" (string (code-char 216))) (dom:radial-dimension-text diameter)))
      (is (string= (format nil "90~a" (string (code-char 176))) (dom:angular-dimension-text angle))))))

(test the-console-says-why-a-dimension-cannot-be-drawn
  (let ((context (script-env)))
    (is (search "different directions" (failure-of context "(angle-dimension 0 0 10 10 1000)")))
    (is (search "radius" (failure-of context "(radius-dimension 0 0 0)")))
    (is (= 0 (dom:drawing-entity-count (script:script-context-drawing context))) "nothing was left behind")))

(test the-three-commands-are-in-the-help-and-the-script-of-a-drawing-makes-them-again
  (let ((overview (script:script-result-output (run-script (script-env) "(help)"))))
    (is (search "radius-dimension diameter-dimension angle-dimension" overview)))
  (let* ((drawing (dimension-drawing))
         (again (dom:make-drawing))
         (result (run-script (script-env again) (script:drawing-script drawing))))
    (is (eq :ok (script:script-result-status result)) "~a" (script:script-result-message result))
    (is (equal (entity-shapes drawing) (entity-shapes again)))))

;;; The tools

(defun dimensioning-app ()
  "Return an application, with the grid snap and the object snap off, holding a circle of radius 1000 at the origin
and an arc of the same circle."
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (app:commit-changes application
                        (dom:create-entities-changes
                         (list (snap-circle 1 0 0 1000)
                               (dom:make-arc :id 2 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 600d0
                                             :start-angle 0d0 :end-angle (/ pi 2))
                               (snap-line 3 -2000 -1500 2000 -1500)))
                        "entity.create")
    application))

(test the-draw-menu-lists-the-three-dimension-tools
  (let ((application (dimensioning-app)))
    (click-widget application :draw)
    (let ((kinds (mapcar (lambda (row) (second (ui:widget-id row))) (menu-rows application))))
      (is (equal '(:dimension :radius-dimension :diameter-dimension :angular-dimension) (subseq kinds 8 12))))
    (click-widget application '(:menu-item :diameter-dimension))
    (is (eq :diameter-dimension (app:tool-kind (app:app-tool application))))
    (is (ui:widget-active (find-widget application :draw)) "the Draw button wears the icon of the tool")))

(test a-radius-dimension-takes-a-click-on-the-circle-and-a-click-for-the-direction
  (let ((application (dimensioning-app)))
    (choose-tool application :radius-dimension)
    (click application 1000 0)
    (is (app:tool-in-progress-p (app:app-tool application)))
    (is (search "where the leader" (app:tool-prompt (app:app-tool application) application)))
    (move-pointer-to application 0 1500)
    (is (equal '(:radial-dimension) (mapcar #'dom:entity-kind (app:tool-preview-entities (app:app-tool application)
                                                                                         application))))
    (click application 0 1500)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (let ((dimension (last-entity application)))
      (is (eq :radial-dimension (dom:entity-kind dimension)))
      (is (close-p (/ pi 2) (dom:radial-dimension-angle dimension)))
      (is (= 1000d0 (dom:radial-dimension-radius dimension)))
      (is (not (dom:radial-dimension-diameter dimension)))
      (is (string= "R1000" (dom:radial-dimension-text dimension))))
    (is (not (app:tool-in-progress-p (app:app-tool application))) "ready for the next circle")
    (app:operation-undo application)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))) "one step of undo")))

(test a-diameter-dimension-says-the-diameter
  (let ((application (dimensioning-app)))
    (choose-tool application :diameter-dimension)
    (click application 0 1000)
    (click application 1500 1500)
    (let ((dimension (last-entity application)))
      (is (dom:radial-dimension-diameter dimension))
      (is (string= (format nil "~a2000" (string (code-char 216))) (dom:radial-dimension-text dimension)))
      (is (close-p (/ pi 4) (dom:radial-dimension-angle dimension))))))

(test an-arc-can-be-dimensioned-too
  (let ((application (dimensioning-app)))
    (choose-tool application :radius-dimension)
    (click application 424 424)
    (click application 1000 1000)
    (is (= 600d0 (dom:radial-dimension-radius (last-entity application))))))

(test only-circles-and-arcs-can-be-dimensioned-this-way
  (let ((application (dimensioning-app)))
    (choose-tool application :radius-dimension)
    (click application 0 -1500)
    (is (eq :error (cdr (app:app-message application))) "a line is not round")
    (is (search "circle or an arc" (message-text application)))
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (click application 1500 1500)
    (is (eq :error (cdr (app:app-message application))) "nothing there either")
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))))

(test with-the-pointer-on-the-center-the-leader-takes-a-default-direction
  (let ((application (dimensioning-app)))
    (choose-tool application :radius-dimension)
    (click application 1000 0)
    (click application 0 0)
    (is (close-p (/ pi 4) (dom:radial-dimension-angle (last-entity application))))))

(test escape-cancels-a-dimension-and-then-leaves-the-tool
  (let ((application (dimensioning-app)))
    (choose-tool application :radius-dimension)
    (click application 1000 0)
    (press application :escape)
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (is (eq :radius-dimension (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))))

(test an-angle-dimension-takes-a-vertex-a-point-on-each-arm-and-a-click-for-the-arc
  (let ((application (dimensioning-app)))
    (choose-tool application :angular-dimension)
    (click application 0 0)
    (move-pointer-to application 800 0)
    (is (equal '(:line) (mapcar #'dom:entity-kind (app:tool-preview-entities (app:app-tool application)
                                                                             application))))
    (click application 1000 0)
    (move-pointer-to application 0 800)
    (is (= 2 (length (app:tool-preview-entities (app:app-tool application) application))) "both arms")
    (click application 0 1000)
    (move-pointer-to application 500 500)
    (is (equal '(:angular-dimension) (mapcar #'dom:entity-kind (app:tool-preview-entities (app:app-tool application)
                                                                                         application))))
    (click application 500 500)
    (let ((dimension (last-entity application)))
      (is (eq :angular-dimension (dom:entity-kind dimension)))
      (is (close-p 0d0 (dom:angular-dimension-start-angle dimension)))
      (is (close-p (/ pi 2) (dom:angular-dimension-end-angle dimension)))
      (is (close-p (* 500 (sqrt 2)) (dom:angular-dimension-radius dimension) 1d-3) "the arc goes through the click")
      (is (search "90.0" (dom:angular-dimension-text dimension))))
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (app:operation-undo application)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-last-click-says-which-of-the-two-angles-the-arms-make
  (let ((application (dimensioning-app)))
    (choose-tool application :angular-dimension)
    (click application 0 0)
    (click application 1000 0)
    (click application 0 1000)
    (click application -500 -500)
    (let ((dimension (last-entity application)))
      (is (close-p (* 3 (/ pi 2)) (dom:angular-dimension-sweep dimension)) "the side of the click, 270 degrees")
      (is (search "270.0" (dom:angular-dimension-text dimension)))
      (is (close-p (/ pi 2) (dom:angular-dimension-start-angle dimension)) "from the second arm to the first"))))

(test arms-that-are-not-arms-are-refused
  (let ((application (dimensioning-app)))
    (choose-tool application :angular-dimension)
    (click application 0 0)
    (click application 0 0)
    (is (eq :error (cdr (app:app-message application))) "on the vertex")
    (is (search "apart" (message-text application)))
    (click application 1000 0)
    (click application 1500 0)
    (is (eq :error (cdr (app:app-message application))) "the same direction")
    (is (search "different directions" (message-text application)))
    (is (search "second arm" (app:tool-prompt (app:app-tool application) application)) "it waits for another")
    (click application -1000 0)
    (is (search "where the arc" (app:tool-prompt (app:app-tool application) application)) "opposite arms are fine")))

(test the-new-tools-say-their-prompts-in-every-language
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (dimensioning-app)))
        (click-widget application :draw)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the menu in ~a" language)
        (dolist (kind '(:radius-dimension :diameter-dimension :angular-dimension))
          (app:operation-set-tool application kind)
          (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language))
              "~a in ~a" kind language))
        (app:operation-set-tool application :radius-dimension)
        (click application 1500 1500)
        (is (null (untranslated-texts (list (message-text application)) language)) "the error in ~a" language)))))

(test the-inspector-speaks-every-language-for-the-new-dimensions
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (dolist (entity (list (a-radial-dimension) (an-angle-dimension)))
        (let ((labels (append (mapcar #'dom:property-label (dom:entity-properties entity))
                              (mapcar #'dom:derived-value-label (dom:entity-derived-values entity)))))
          (is (null (untranslated-texts labels language)) "~a in ~a" (dom:entity-kind entity) language))))))
