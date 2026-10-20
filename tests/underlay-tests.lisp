(in-package #:borba-cad/tests)

(def-suite underlays
  :description "The pictures under a drawing: what an underlay is, where its picture stands, calibrating it so that
two points stand a known distance apart, the underlays of a drawing and how they are saved."
  :in all)

(in-suite underlays)

(defun a-picture-underlay (&rest keys)
  "Return an underlay of a made-up picture of 200 by 100 pixels, with the settings in KEYS changed."
  (apply #'dom:make-underlay
         (append keys
                 (list :name "plan.png"
                       :kind :png
                       :data (octets-of 137 80 78 71 1 2 3 4 5)
                       :pixel-width 200
                       :pixel-height 100))))

;;; What an underlay is

(test an-underlay-keeps-its-picture-and-where-it-stands
  (let ((underlay (a-picture-underlay :x 100 :y -50 :rotation 30 :scale 2.5 :opacity 0.4d0)))
    (is (string= "plan.png" (dom:underlay-name underlay)))
    (is (eq :png (dom:underlay-kind underlay)))
    (is (= 200 (dom:underlay-pixel-width underlay)))
    (is (= 100d0 (dom:underlay-x underlay)))
    (is (= -50d0 (dom:underlay-y underlay)))
    (is (= 30d0 (dom:underlay-rotation underlay)))
    (is (= 2.5d0 (dom:underlay-scale underlay)))
    (is (= 0.4d0 (dom:underlay-opacity underlay)))
    (is (dom:underlay-visible underlay))
    (is (= 500d0 (dom:underlay-width underlay)))
    (is (= 250d0 (dom:underlay-height underlay)))
    (is (null (dom:underlay-dpi underlay)))))

(test an-underlay-that-makes-no-sense-is-refused-with-a-reason
  (flet ((refused (&rest keys)
           (refusal-text (lambda () (apply #'a-picture-underlay keys)))))
    (is (search "name" (refused :name "  ")))
    (is (search "at most" (refused :name (make-string 80 :initial-element #\a))))
    (is (search "PNG" (refused :kind :gif)))
    (is (search "scale" (refused :scale 0)))
    (is (search "scale" (refused :scale 1d9)))
    (is (search "scale" (refused :scale 2d6)))
    (is (search "opacity" (refused :opacity 1.5)))
    (is (search "opacity" (refused :opacity -0.1)))
    (is (search "numbers" (refused :x 1d300)))
    (is (search "page" (refused :page 0)))
    (is (search "resolution" (refused :dpi -5)))
    (is (search "picture" (refused :data (octets-of))))
    (is (search "size" (refused :pixel-width 0)))
    (is (search "size" (refused :pixel-height 70000)))))

(test the-turn-of-an-underlay-is-kept-between-minus-and-plus-half-a-turn
  (is (= -170d0 (dom:underlay-rotation (a-picture-underlay :rotation 190))))
  (is (= 90d0 (dom:underlay-rotation (a-picture-underlay :rotation 450))))
  (is (= 180d0 (dom:underlay-rotation (a-picture-underlay :rotation -180)))))

(test changing-an-underlay-makes-another-and-leaves-the-picture
  (let* ((underlay (a-picture-underlay))
         (moved (dom:underlay-with underlay :x 5 :opacity 0.9d0 :visible nil)))
    (is (= 5d0 (dom:underlay-x moved)))
    (is (= 0.9d0 (dom:underlay-opacity moved)))
    (is (not (dom:underlay-visible moved)))
    (is (= 0d0 (dom:underlay-x underlay)) "the old one is as it was")
    (is (eq (dom:underlay-data underlay) (dom:underlay-data moved)))
    (is (refusal-text (lambda () (dom:underlay-with underlay :scale 0))))))

;;; Where the picture stands

(test a-point-of-the-picture-stands-where-the-position-turn-and-scale-put-it
  (let ((plain (a-picture-underlay :x 10 :y 20 :scale 2)))
    (multiple-value-bind (x y)
        (dom:underlay-world-point plain 0 100)
      (is (= 10d0 x))
      (is (= 20d0 y) "the bottom left of the picture is its lower left corner"))
    (multiple-value-bind (x y)
        (dom:underlay-world-point plain 200 0)
      (is (= 410d0 x))
      (is (= 220d0 y) "the top right corner")))
  (let ((turned (a-picture-underlay :x 0 :y 0 :scale 1 :rotation 90)))
    (multiple-value-bind (x y)
        (dom:underlay-world-point turned 200 100)
      (is (< (abs (- x 0)) 1d-9))
      (is (< (abs (- y 200)) 1d-9) "turned a quarter, the lower right corner goes up"))))

(test the-point-of-the-picture-under-a-point-of-the-drawing-is-the-inverse
  (let ((underlay (a-picture-underlay :x 33 :y -12 :scale 0.7d0 :rotation -27)))
    (dolist (point '((0 0) (200 100) (37.5 81.25) (150 3)))
      (multiple-value-bind (x y)
          (dom:underlay-world-point underlay (first point) (second point))
        (multiple-value-bind (column row)
            (dom:underlay-picture-point underlay x y)
          (is (< (abs (- column (first point))) 1d-9))
          (is (< (abs (- row (second point))) 1d-9)))))))

(test the-corners-and-the-bounds-of-a-turned-picture
  (let* ((underlay (a-picture-underlay :x 0 :y 0 :scale 1 :rotation 90))
         (corners (dom:underlay-corners underlay))
         (bounds (dom:underlay-bounds underlay)))
    (is (= 4 (length corners)))
    (is (< (abs (- (car (first corners)) 0)) 1d-9) "lower left")
    (is (< (abs (- (cdr (second corners)) 200)) 1d-9) "lower right goes up")
    (is (< (abs (- (geo:bounds-min-x bounds) -100)) 1d-9))
    (is (< (abs (- (geo:bounds-max-y bounds) 200)) 1d-9))))

;;; Calibrating

(defun distance-between-picture-points (underlay
                                        first
                                        second)
  "Return how far apart, in the drawing, two points of the picture, each (column row), stand."
  (multiple-value-bind (x1 y1)
      (dom:underlay-world-point underlay (first first) (second first))
    (multiple-value-bind (x2 y2)
        (dom:underlay-world-point underlay (first second) (second second))
      (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))))

(defun direction-between-picture-points (underlay
                                         first
                                         second)
  "Return the direction, in degrees, from one point of the picture to another, as they stand in the drawing."
  (multiple-value-bind (x1 y1)
      (dom:underlay-world-point underlay (first first) (second first))
    (multiple-value-bind (x2 y2)
        (dom:underlay-world-point underlay (first second) (second second))
      (* (atan (- y2 y1) (- x2 x1)) (/ 180 pi)))))

(test calibrating-makes-two-points-of-the-picture-stand-the-known-distance-apart
  (let* ((underlay (a-picture-underlay :x 400 :y 300 :scale 1 :rotation 12))
         (a '(20 80))
         (b '(180 40)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay (first a) (second a))
      (multiple-value-bind (bx by)
          (dom:underlay-world-point underlay (first b) (second b))
        (let ((calibrated (dom:calibrated-underlay underlay ax ay bx by 5000)))
          (is (< (abs (- 5000 (distance-between-picture-points calibrated a b))) 1d-6))
          (is (< (abs (- (direction-between-picture-points underlay a b)
                         (direction-between-picture-points calibrated a b)))
                 1d-9)
              "keeping the turn leaves the direction")
          (multiple-value-bind (x y)
              (dom:underlay-world-point calibrated (first a) (second a))
            (is (< (abs (- x ax)) 1d-6) "the first point stays where it was")
            (is (< (abs (- y ay)) 1d-6)))
          (is (= (dom:underlay-rotation underlay) (dom:underlay-rotation calibrated))))))))

(test calibrating-can-also-level-straighten-or-aim-the-line-between-the-points
  (let* ((underlay (a-picture-underlay :x 0 :y 0 :scale 1 :rotation 20))
         (a '(10 10))
         (b '(150 70)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay (first a) (second a))
      (multiple-value-bind (bx by)
          (dom:underlay-world-point underlay (first b) (second b))
        (flet ((calibrate (&rest keys)
                 (apply #'dom:calibrated-underlay underlay ax ay bx by 1000 keys)))
          (let ((level (calibrate :alignment :horizontal)))
            (is (< (abs (direction-between-picture-points level a b)) 1d-9))
            (is (< (abs (- 1000 (distance-between-picture-points level a b))) 1d-6)))
          (let ((upright (calibrate :alignment :vertical)))
            (is (< (abs (- -90 (direction-between-picture-points upright a b))) 1d-9)
                "the line leaned down, so it is made to point down"))
          (let ((aimed (calibrate :alignment :angle :angle 33)))
            (is (< (abs (- 33 (direction-between-picture-points aimed a b))) 1d-9)))
          (let ((leftwards (dom:calibrated-underlay underlay bx by ax ay 1000 :alignment :horizontal)))
            (is (< (abs (- 180 (abs (direction-between-picture-points leftwards b a)))) 1d-9)
                "the line from the first point picked to the second is made level, pointing the way it leaned")
            (is (< (abs (direction-between-picture-points leftwards a b)) 1d-9))))))))

(test calibrating-with-nothing-to-measure-is-refused
  (let ((underlay (a-picture-underlay)))
    (is (search "different" (refusal-text (lambda () (dom:calibrated-underlay underlay 5 5 5 5 100)))))
    (is (search "above zero" (refusal-text (lambda () (dom:calibrated-underlay underlay 0 0 10 0 0)))))
    (is (search "above zero" (refusal-text (lambda () (dom:calibrated-underlay underlay 0 0 10 0 -3)))))
    (is (search "keeps the turn"
                (refusal-text (lambda () (dom:calibrated-underlay underlay 0 0 10 0 3 :alignment :tilt)))))
    (is (search "scale" (refusal-text (lambda () (dom:calibrated-underlay underlay 0 0 1d-3 0 1d6)))))))

;;; The underlays of a drawing

(test a-drawing-numbers-the-underlays-it-is-given-and-keeps-them-in-order
  (let* ((drawing (dom:make-drawing))
         (first (dom:add-underlay drawing (a-picture-underlay :name "one.png")))
         (second (dom:add-underlay drawing (a-picture-underlay :name "two.png"))))
    (is (= 1 (dom:underlay-id first)))
    (is (= 2 (dom:underlay-id second)))
    (is (= 2 (dom:underlay-count drawing)))
    (is (equal '("one.png" "two.png") (mapcar #'dom:underlay-name (dom:drawing-underlays drawing))))
    (is (eq second (dom:find-underlay drawing 2)))
    (is (null (dom:find-underlay drawing 7)))
    (dom:remove-underlay drawing 1)
    (is (= 3 (dom:underlay-id (dom:add-underlay drawing (a-picture-underlay))))
        "a number is not given again")))

(test replacing-and-removing-an-underlay-needs-one-that-is-there
  (let* ((drawing (dom:make-drawing))
         (underlay (dom:add-underlay drawing (a-picture-underlay))))
    (dom:replace-underlay drawing (dom:underlay-with underlay :opacity 0.2d0))
    (is (= 0.2d0 (dom:underlay-opacity (dom:find-underlay drawing 1))))
    (is (search "no underlay"
                (refusal-text (lambda () (dom:replace-underlay drawing (dom:underlay-with underlay :id 9))))))
    (is (search "no underlay" (refusal-text (lambda () (dom:remove-underlay drawing 9)))))))

(test a-drawing-takes-only-so-many-underlays-and-so-many-bytes-of-pictures
  (let ((drawing (dom:make-drawing)))
    (dotimes (index dom:+maximum-underlays+)
      (dom:add-underlay drawing (a-picture-underlay)))
    (is (search "at most" (refusal-text (lambda () (dom:add-underlay drawing (a-picture-underlay))))))
    (is (= dom:+maximum-underlays+ (dom:underlay-count drawing)) "the refused one was not added"))
  (let ((drawing (dom:make-drawing))
        (large (dom:make-underlay :name "big.png"
                                  :kind :png
                                  :data (make-array (* 25 1024 1024) :element-type '(unsigned-byte 8)
                                                                     :initial-element 7)
                                  :pixel-width 10
                                  :pixel-height 10)))
    (dom:add-underlay drawing large)
    (is (search "MiB" (refusal-text (lambda () (dom:add-underlay drawing large)))))
    (is (= 1 (dom:underlay-count drawing)))))

(test changing-the-underlays-counts-as-a-change-of-the-drawing
  (let* ((drawing (dom:make-drawing))
         (before (dom:drawing-revision drawing)))
    (dom:add-underlay drawing (a-picture-underlay))
    (is (> (dom:drawing-revision drawing) before))))

(test the-bounds-of-the-underlays-hold-those-that-show
  (let ((drawing (dom:make-drawing)))
    (is (null (dom:underlays-bounds drawing)))
    (dom:add-underlay drawing (a-picture-underlay :x 0 :y 0 :scale 1))
    (dom:add-underlay drawing (a-picture-underlay :x 1000 :y 0 :scale 1 :visible nil))
    (let ((bounds (dom:underlays-bounds drawing)))
      (is (= 0d0 (geo:bounds-min-x bounds)))
      (is (= 200d0 (geo:bounds-max-x bounds)) "the one that is hidden is left out"))
    (dom:add-underlay drawing (a-picture-underlay :x 500 :y 0 :scale 1))
    (is (= 700d0 (geo:bounds-max-x (dom:underlays-bounds drawing))))))

;;; Saved with the drawing

(defun drawing-with-underlays ()
  "Return a drawing that has two underlays, one a PDF page of a chosen resolution."
  (let ((drawing (dom:make-drawing)))
    (dom:add-underlay drawing (a-picture-underlay :name "ground \"floor\".png"
                                                  :x 120.5
                                                  :y -30.25
                                                  :rotation 12.5
                                                  :scale 3.75
                                                  :opacity 0.35d0))
    (dom:add-underlay drawing (dom:make-underlay :name "scan.pdf"
                                                 :kind :pdf
                                                 :page 3
                                                 :dpi 200
                                                 :data (octets-of 37 80 68 70 45 49 10)
                                                 :pixel-width 1654
                                                 :pixel-height 2339
                                                 :visible nil))
    drawing))

(test the-underlays-are-written-in-the-drawing-file-as-text
  (let ((text (persist:drawing-to-string (drawing-with-underlays))))
    (is (search (format nil " (underlays~%  (underlay (id 1) (name \"ground \\\"floor\\\".png\") (kind png) (page 1) ~
                              (dpi auto)~%   (size 200 100) (position 120.5 -30.25) (rotation 12.5) (scale 3.75) ~
                              (opacity 0.35) (visible t)~%   (data~%    \"iVBORwECAwQF\"))~%")
                text))
    (is (search "(kind pdf) (page 3) (dpi 200.0)" text))
    (is (search "(visible nil)" text))
    (is (< (search " (underlays" text) (search " (entities" text)))))

(test the-underlays-come-back-as-they-were-saved
  (let* ((drawing (drawing-with-underlays))
         (again (persist:parse-drawing-text (persist:drawing-to-string drawing))))
    (is (= 2 (dom:underlay-count again)))
    (loop for old in (dom:drawing-underlays drawing)
          for new in (dom:drawing-underlays again)
          do (is (= (dom:underlay-id old) (dom:underlay-id new)))
             (is (string= (dom:underlay-name old) (dom:underlay-name new)))
             (is (eq (dom:underlay-kind old) (dom:underlay-kind new)))
             (is (= (dom:underlay-page old) (dom:underlay-page new)))
             (is (eql (dom:underlay-dpi old) (dom:underlay-dpi new)))
             (is (= (dom:underlay-pixel-width old) (dom:underlay-pixel-width new)))
             (is (= (dom:underlay-pixel-height old) (dom:underlay-pixel-height new)))
             (is (= (dom:underlay-x old) (dom:underlay-x new)))
             (is (= (dom:underlay-y old) (dom:underlay-y new)))
             (is (= (dom:underlay-rotation old) (dom:underlay-rotation new)))
             (is (= (dom:underlay-scale old) (dom:underlay-scale new)))
             (is (= (dom:underlay-opacity old) (dom:underlay-opacity new)))
             (is (eq (dom:underlay-visible old) (dom:underlay-visible new)))
             (is (equalp (dom:underlay-data old) (dom:underlay-data new))))
    (is (= 3 (dom:underlay-id (dom:add-underlay again (a-picture-underlay))))
        "a drawing that is read goes on numbering after its last underlay")))

(test a-picture-of-megabytes-is-written-in-lines-an-editor-can-open-and-read-back
  (let* ((drawing (dom:make-drawing))
         (bytes (make-array (* 3 1024 1024) :element-type '(unsigned-byte 8)))
         (state (sb-ext:seed-random-state 5)))
    (dotimes (index (length bytes))
      (setf (aref bytes index) (random 256 state)))
    (dom:add-underlay drawing (dom:make-underlay :name "big.png"
                                                 :kind :png
                                                 :data bytes
                                                 :pixel-width 5000
                                                 :pixel-height 3000))
    (let* ((text (persist:drawing-to-string drawing))
           (longest (loop with start = 0
                          for end = (position #\Newline text :start start)
                          while end
                          maximize (- end start)
                          do (setf start (1+ end)))))
      (is (<= longest 1100) "the longest line has ~d characters" longest)
      (is (equalp bytes (dom:underlay-data (first (dom:drawing-underlays (persist:parse-drawing-text text)))))))))

(defun underlay-text-problem (old
                              new)
  "Return the text of the content error that reading the drawing with OLD turned into NEW in its text signals."
  (handler-case (progn (persist:parse-drawing-text (replace-once (persist:drawing-to-string (drawing-with-underlays))
                                                                 old
                                                                 new))
                       nil)
    (base:drawing-file-error (condition)
      (princ-to-string condition))))

(test an-underlay-that-is-not-sound-in-the-file-is-refused-with-a-reason
  (is (search "unknown field" (underlay-text-problem "(page 3)" "(sheet 3)")))
  (is (search "appears twice" (underlay-text-problem "(page 3) " "(page 3) (page 4) ")))
  (is (search "is missing" (underlay-text-problem "(visible nil)" "")))
  (is (search "unknown kind" (underlay-text-problem "(kind pdf)" "(kind gif)")))
  (is (search "two whole numbers" (underlay-text-problem "(size 1654 2339)" "(size 1654)")))
  (is (search "two numbers" (underlay-text-problem "(position 0.0 0.0)" "(position 0.0)")))
  (is (search "Base64" (underlay-text-problem "\"JVBERi0xCg==\"" "\"!!!\"")))
  (is (search "expected a quoted string" (underlay-text-problem "\"JVBERi0xCg==\"" "12")))
  (is (search "scale" (underlay-text-problem "(scale 1.0)" "(scale 0.0)")))
  (is (search "opacity" (underlay-text-problem "(opacity 0.6)" "(opacity 7.0)")))
  (is (search "expected t or nil" (underlay-text-problem "(visible nil)" "(visible maybe)")))
  (is (search "expected an underlay" (underlay-text-problem "(underlay (id 2)" "(sheet (id 2)")))
  (is (search "same number" (underlay-text-problem "(underlay (id 2)" "(underlay (id 1)"))))
