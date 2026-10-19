(in-package #:borba-cad/tests)

(def-suite blocks
  :description "Blocks: defining and placing them, the file, the console, the clipboard and the tools."
  :in all)

(in-suite blocks)

;;; Helpers

(defun block-line (id x1 y1 x2 y2)
  "Return a line entity on layer 0."
  (dom:make-line :id id :layer-id 0 :x1 (float x1 1d0) :y1 (float y1 1d0) :x2 (float x2 1d0) :y2 (float y2 1d0)))

(defun a-block (&optional (name "door"))
  "Return a block of a line and a circle, with its base point at the origin."
  (dom:make-block-definition :name name
                             :entities (list (block-line 1 0 0 100 0)
                                             (dom:make-circle :id 2 :layer-id 0 :center-x 50d0 :center-y 0d0
                                                              :radius 50d0))))

(defun a-placement (&key (id 1) (layer-id 0) (block (a-block)) (x 0d0) (y 0d0) (scale 1d0) (rotation 0d0) flipped)
  "Return a placement of BLOCK."
  (dom:make-insert :id id :layer-id layer-id :block block :x x :y y :scale scale :rotation rotation :flipped flipped))

(defun placed-line (insert index)
  "Return the line that is the INDEX-th entity of INSERT, as a list of its four coordinates."
  (let ((line (nth index (dom:insert-entities insert))))
    (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line))))

(defun line-numbers (line)
  "Return the four coordinates of a line, as a list."
  (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line)))

(defun block-near-p (expected actual &optional (tolerance 1d-6))
  "Return true when two lists of numbers agree to TOLERANCE."
  (and (= (length expected) (length actual))
       (every (lambda (a b) (< (abs (- a b)) tolerance)) expected actual)))

(defun block-drawing (&rest entities)
  "Return a drawing holding the block \"door\" and ENTITIES."
  (let ((drawing (dom:make-drawing)))
    (dom:define-block drawing (a-block))
    (dom:apply-changes drawing (dom:create-entities-changes entities))
    drawing))

;;; Definitions

(test a-block-keeps-its-entities-as-if-its-base-point-were-the-origin
  (let ((definition (dom:make-block-definition :name " Door "
                                               :entities (list (block-line 7 1000 500 1100 500)
                                                               (dom:make-circle :id 9 :layer-id 3 :center-x 1050d0
                                                                                :center-y 500d0 :radius 50d0))
                                               :base-x 1000d0
                                               :base-y 500d0)))
    (is (string= "Door" (dom:block-definition-name definition)) "trimmed, with its case")
    (is (equal '(1 2) (mapcar #'dom:entity-id (dom:block-definition-entities definition))) "numbered from 1")
    (is (every (lambda (entity) (zerop (dom:entity-layer-id entity))) (dom:block-definition-entities definition)))
    (let ((line (first (dom:block-definition-entities definition))))
      (is (block-near-p '(0 0 100 0) (line-numbers line))))
    (is (= 50d0 (dom:circle-center-x (second (dom:block-definition-entities definition)))))
    (is (equalp (geo:make-bounds 0d0 -50d0 100d0 50d0) (dom:block-definition-bounds definition)))))

(test a-block-needs-a-name-and-entities-and-cannot-hold-a-block
  (flet ((refused (text &rest arguments)
           (handler-case (progn (apply #'dom:make-block-definition arguments)
                                (fail "a block with ~a was accepted" text))
             (base:edit-refused (condition)
               (is (plusp (length (princ-to-string condition))))))))
    (refused "no name" :name "   " :entities (list (block-line 1 0 0 1 1)))
    (refused "a long name" :name (make-string 65 :initial-element #\a) :entities (list (block-line 1 0 0 1 1)))
    (refused "a control character in its name" :name (format nil "a~cb" #\Tab) :entities (list (block-line 1 0 0 1 1)))
    (refused "no entities" :name "x" :entities nil)
    (refused "a block in it" :name "x" :entities (list (a-placement)))
    (refused "too many entities"
             :name "x"
             :entities (loop for index from 1 to (1+ dom:+maximum-block-entities+) collect (block-line index 0 0 1 1)))
    (is (dom:make-block-definition :name (make-string 64 :initial-element #\a)
                                   :entities (list (block-line 1 0 0 1 1))))))

;;; Placements

(test a-placement-flips-then-scales-turns-and-moves-the-block
  (let ((definition (dom:make-block-definition :name "one" :entities (list (block-line 1 0 0 10 5)))))
    (is (block-near-p '(1000 500 1010 505)
                      (placed-line (a-placement :block definition :x 1000d0 :y 500d0) 0)))
    (is (block-near-p '(1000 500 1020 510)
                      (placed-line (a-placement :block definition :x 1000d0 :y 500d0 :scale 2d0) 0)))
    (is (block-near-p '(0 0 -5 10)
                      (placed-line (a-placement :block definition :rotation (/ pi 2)) 0))
        "turned a quarter, (10, 5) goes to (-5, 10)")
    (is (block-near-p '(0 0 -10 5)
                      (placed-line (a-placement :block definition :flipped t) 0))
        "flipped, (10, 5) goes to (-10, 5)")
    (is (block-near-p '(990 480 990 480) (list 990 480 990 480)) "the order below: flip, scale, turn, move")
    (is (block-near-p '(1000 500 990 480)
                      (placed-line (a-placement :block definition :x 1000d0 :y 500d0 :scale 2d0
                                                :rotation (/ pi 2) :flipped t)
                                   0))
        "(10, 5) flipped is (-10, 5), twice that is (-20, 10), turned a quarter (-10, -20), moved (990, 480)")))

(test a-placement-puts-its-entities-on-its-own-layer
  (let ((insert (a-placement :layer-id 4)))
    (is (every (lambda (entity) (= 4 (dom:entity-layer-id entity))) (dom:insert-entities insert)))
    (is (= 4 (dom:entity-layer-id insert)))
    (let ((moved (dom:entity-with-layer insert 6)))
      (is (every (lambda (entity) (= 6 (dom:entity-layer-id entity))) (dom:insert-entities moved))))))

(test a-placement-has-the-bounds-of-what-it-places
  (let ((insert (a-placement :x 1000d0 :y 0d0 :scale 2d0)))
    (is (equalp (geo:make-bounds 1000d0 -100d0 1200d0 100d0) (dom:entity-bounds insert)))
    (is (eq :insert (dom:entity-kind insert)))))

(test a-placement-refuses-what-cannot-be-placed
  (signals-invalid-entity :insert (a-placement :scale 0d0))
  (signals-invalid-entity :insert (a-placement :scale -2d0))
  (signals-invalid-entity :insert (a-placement :scale (* 2 dom:+maximum-insert-scale+)))
  (signals-invalid-entity :insert (a-placement :scale 5d5) "so large that its entities leave the drawing")
  (signals-invalid-entity :insert (a-placement :block nil))
  (signals-invalid-entity :insert (a-placement :x (* 2 base:+maximum-coordinate-magnitude+)))
  (signals-invalid-entity :insert (a-placement :rotation 1d9))
  (signals-invalid-entity :insert (a-placement :id 0)))

(test the-rotation-is-kept-reduced-and-the-flag-a-boolean
  (is (< (abs (- (- (* 2 pi) 0.5d0) (dom:insert-rotation (a-placement :rotation -0.5d0)))) 1d-9))
  (is (eq t (dom:insert-flipped (a-placement :flipped :yes))))
  (is (eq nil (dom:insert-flipped (a-placement)))))

(test the-inspector-shows-the-block-and-edits-the-rest
  (let ((insert (a-placement :x 10d0 :scale 2d0 :rotation 0.5d0)))
    (is (equal '(:block :x :y :scale :rotation :flipped) (mapcar #'dom:property-key (dom:entity-properties insert))))
    (let ((block-property (first (dom:entity-properties insert))))
      (is (string= "door" (dom:property-value block-property)))
      (is (not (dom:property-editable block-property)) "it is the name of the block, which a placement cannot change"))
    (is (= 3d0 (dom:insert-scale (dom:entity-with-property insert :scale 3d0))))
    (is (< (abs (- 1d0 (dom:insert-rotation (dom:entity-with-property insert :rotation 1d0)))) 1d-9))
    (is (dom:insert-flipped (dom:entity-with-property insert :flipped t)))
    (is (= 50d0 (dom:insert-x (dom:entity-with-property insert :x 50d0))))
    (signals base:invalid-entity (dom:entity-with-property insert :scale 0d0))
    (is (= 2 (dom:derived-value-value (first (dom:entity-derived-values insert)))))))

(test a-placement-is-picked-on-what-it-places-and-offers-its-points
  (let ((insert (a-placement :x 1000d0 :y 0d0)))
    (is (= 0d0 (dom:entity-pick-distance insert 1050d0 0d0)) "on the line")
    (is (= 0d0 (dom:entity-pick-distance insert 1050d0 50d0)) "on the circle")
    (is (< 100d0 (dom:entity-pick-distance insert 1050d0 300d0)))
    (is (dom:entity-touches-bounds-p insert (geo:make-bounds 1090d0 -10d0 1110d0 10d0)))
    (is (not (dom:entity-touches-bounds-p insert (geo:make-bounds 0d0 0d0 100d0 100d0))))
    (let ((points (dom:entity-snap-points insert)))
      (is (equal '(1000d0 0d0 :endpoint) (first points)) "the base point first")
      (is (< 3 (length points)) "and the points of what it places"))
    (is (= 1000d0 (dom:insert-x (dom:entity-translated insert 0d0 5d0))))
    (is (= 5d0 (dom:insert-y (dom:entity-translated insert 0d0 5d0))))
    (let ((grips (dom:entity-grips insert)))
      (is (equal '(:move) (mapcar #'dom:grip-id grips)))
      (is (= 1200d0 (dom:insert-x (dom:entity-with-grip insert (first grips) 1200d0 0d0)))))))

(test the-lines-and-circles-of-a-placement-are-boundaries-for-trimming-and-snapping
  (let ((drawing (block-drawing (a-placement :id 1 :x 1000d0 :y 0d0))))
    (is (= 2 (length (dom::entity-primitives (dom:store-find (dom:drawing-store drawing) 1)))))))

;;; Rotating, mirroring and scaling a placement

(defun placed-ends (insert)
  "Return every end of every line that a placement places, as a flat list of numbers."
  (loop for entity in (dom:insert-entities insert)
        when (eq :line (dom:entity-kind entity))
          append (list (dom:line-x1 entity) (dom:line-y1 entity) (dom:line-x2 entity) (dom:line-y2 entity))))

(test turning-a-placement-turns-what-it-places-about-the-same-center
  (dolist (flipped '(nil t))
    (let* ((insert (a-placement :x 300d0 :y 200d0 :scale 1.5d0 :rotation 0.7d0 :flipped flipped))
           (turned (dom:entity-rotated insert 40d0 -30d0 1.1d0))
           (expected (let ((ends '()))
                       (loop for (x y) on (placed-ends insert) by #'cddr
                             do (multiple-value-bind (rx ry) (dom:rotate-xy x y 40d0 -30d0 1.1d0)
                                  (push rx ends)
                                  (push ry ends)))
                       (nreverse ends))))
      (is (block-near-p expected (placed-ends turned) 1d-6) "flipped ~a" flipped))))

(test mirroring-a-placement-reflects-what-it-places
  (dolist (flipped '(nil t))
    (let* ((insert (a-placement :x 300d0 :y 200d0 :scale 1.5d0 :rotation 0.7d0 :flipped flipped))
           (mirrored (dom:entity-mirrored insert 50d0 20d0 (+ 50d0 (* 100 (cos 0.4d0))) (+ 20d0 (* 100 (sin 0.4d0)))))
           (expected (let ((ends '()))
                       (loop for (x y) on (placed-ends insert) by #'cddr
                             do (multiple-value-bind (rx ry)
                                    (dom:mirror-xy x y 50d0 20d0 (+ 50d0 (* 100 (cos 0.4d0)))
                                                   (+ 20d0 (* 100 (sin 0.4d0))))
                                  (push rx ends)
                                  (push ry ends)))
                       (nreverse ends))))
      (is (block-near-p expected (placed-ends mirrored) 1d-6) "flipped ~a" flipped)
      (is (eq (not flipped) (dom:insert-flipped mirrored)) "a reflection flips"))))

(test scaling-a-placement-scales-what-it-places-about-the-same-center
  (let* ((insert (a-placement :x 300d0 :y 200d0 :scale 1.5d0 :rotation 0.7d0))
         (scaled (dom:entity-scaled insert 40d0 -30d0 2d0))
         (expected (let ((ends '()))
                     (loop for (x y) on (placed-ends insert) by #'cddr
                           do (multiple-value-bind (sx sy) (dom:scale-xy x y 40d0 -30d0 2d0)
                                (push sx ends)
                                (push sy ends)))
                     (nreverse ends))))
    (is (block-near-p expected (placed-ends scaled) 1d-6))
    (is (= 3d0 (dom:insert-scale scaled)))))

;;; The drawing's blocks

(test a-drawing-keeps-its-blocks-by-name-without-regard-to-case
  (let ((drawing (dom:make-drawing)))
    (is (null (dom:block-names drawing)))
    (dom:define-block drawing (a-block "Window"))
    (dom:define-block drawing (a-block "door"))
    (is (equal '("door" "Window") (dom:block-names drawing)) "sorted")
    (is (eq (dom:find-block drawing "WINDOW") (dom:find-block drawing "window")))
    (is (null (dom:find-block drawing "gate")))
    (let ((revision (dom:drawing-revision drawing)))
      (dom:define-block drawing (a-block "gate"))
      (is (> (dom:drawing-revision drawing) revision) "a drawing with a new block has changed"))))

(test a-block-that-is-in-use-cannot-be-redefined-or-removed
  (let ((drawing (block-drawing (a-placement :id 1))))
    (is (dom:block-in-use-p drawing "DOOR"))
    (handler-case (progn (dom:define-block drawing (a-block "door")) (fail "redefined a block in use"))
      (base:edit-refused (condition)
        (is (search "in use" (princ-to-string condition)))))
    (handler-case (progn (dom:remove-block drawing "door") (fail "removed a block in use"))
      (base:edit-refused (condition)
        (is (search "in use" (princ-to-string condition)))))
    (dom:define-block drawing (a-block "spare"))
    (dom:remove-block drawing "Spare")
    (is (null (dom:find-block drawing "spare")))
    (let ((other (dom:make-block-definition :name "gate" :entities (list (block-line 1 0 0 5 5)))))
      (dom:define-block drawing (a-block "gate"))
      (dom:define-block drawing other)
      (is (eq other (dom:find-block drawing "gate")) "an unused block is replaced"))))

;;; Change sets

(test making-a-block-replaces-the-entities-with-a-placement-of-it
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (block-line 1 1000 500 1100 500)
                                      (dom:make-circle :id 2 :layer-id 0 :center-x 1050d0 :center-y 500d0 :radius 50d0)
                                      (block-line 3 9000 9000 9100 9000))))
    (multiple-value-bind (definition changes skipped) (dom:make-block-changes drawing '(1 2) "door" 1000d0 500d0)
      (is (= 0 skipped))
      (is (= 2 (length (dom:block-definition-entities definition))))
      (is (= 3 (length changes)) "two deletions and a creation")
      (is (null (dom:change-after (first changes))))
      (let ((insert (dom:change-after (third changes))))
        (is (eq :insert (dom:entity-kind insert)))
        (is (= 4 (dom:entity-id insert)))
        (is (= 1000d0 (dom:insert-x insert)))
        (is (eq definition (dom:insert-block insert))))
      (dom:define-block drawing definition)
      (dom:apply-changes drawing changes)
      (is (equal '(3 4) (mapcar #'dom:entity-id (dom:drawing-entities drawing)))))))

(test making-a-block-says-what-is-wrong
  (let ((drawing (block-drawing (block-line 1 0 0 10 10) (a-placement :id 2))))
    (flet ((refused (&rest arguments)
             (handler-case (progn (apply #'dom:make-block-changes drawing arguments)
                                  (fail "a block was made"))
               (base:edit-refused (condition)
                 (princ-to-string condition)))))
      (is (search "needs a name" (refused '(1) "" 0d0 0d0)))
      (is (search "explode" (refused '(2) "new" 0d0 0d0)) "a block cannot hold a block")
      (is (search "needs at least one" (refused '(99) "new" 0d0 0d0)) "there is no such entity")
      (is (search "in use" (refused '(1) "door" 0d0 0d0)) "door is placed"))))

(test a-block-that-is-not-in-use-can-be-made-again-and-an-entity-on-a-locked-layer-is-skipped
  (let ((drawing (block-drawing (block-line 1 0 0 10 10) (block-line 2 20 20 30 30))))
    (let ((locked (dom:add-layer drawing "locked" (dom:make-rgb 1 2 3))))
      (let ((line (dom:store-find (dom:drawing-store drawing) 2)))
        (dom:apply-changes drawing (list (dom:make-change 2
                                                          line
                                                          (dom:entity-with-layer line (dom:layer-id locked))))))
      (dom:set-layer-locked drawing (dom:layer-id locked) t))
    (multiple-value-bind (definition changes skipped) (dom:make-block-changes drawing '(1 2) "door" 0d0 0d0)
      (is (= 1 skipped))
      (is (= 1 (length (dom:block-definition-entities definition))))
      (is (= 2 (length changes))))))

(test placing-a-block-adds-a-placement-on-the-current-layer
  (let* ((drawing (block-drawing))
         (layer (dom:add-layer drawing "doors" (dom:make-rgb 1 2 3))))
    (dom:set-current-layer drawing (dom:layer-id layer))
    (let* ((changes (dom:insert-changes drawing "DOOR" 100d0 200d0 :scale 2d0 :rotation 0.5d0 :flipped t))
           (insert (dom:change-after (first changes))))
      (is (= 1 (length changes)))
      (is (= (dom:layer-id layer) (dom:entity-layer-id insert)))
      (is (and (= 100d0 (dom:insert-x insert)) (= 2d0 (dom:insert-scale insert)) (dom:insert-flipped insert))))
    (handler-case (progn (dom:insert-changes drawing "gate" 0d0 0d0) (fail "placed a block that is not there"))
      (base:edit-refused (condition)
        (is (search "no block called gate" (princ-to-string condition)))))))

(test exploding-a-placement-puts-its-entities-in-the-drawing
  (let* ((drawing (block-drawing (a-placement :id 1 :layer-id 0 :x 1000d0 :scale 2d0) (block-line 2 0 0 5 5)))
         (before (placed-ends (dom:store-find (dom:drawing-store drawing) 1))))
    (multiple-value-bind (changes exploded skipped) (dom:explode-changes drawing '(1 2 99))
      (is (= 1 exploded))
      (is (= 2 skipped) "the line is not a placement and 99 is not there")
      (is (= 3 (length changes)) "the placement goes, two entities come")
      (dom:apply-changes drawing changes)
      (is (equal '(2 3 4) (mapcar #'dom:entity-id (dom:drawing-entities drawing))))
      (is (block-near-p before (line-numbers (dom:store-find (dom:drawing-store drawing) 3)))))))

;;; The file

(defun block-file-drawing ()
  "Return a drawing with two blocks and three placements, one of them turned and flipped, on another layer."
  (let ((drawing (dom:make-drawing)))
    (dom:define-block drawing (a-block "door"))
    (dom:define-block drawing (dom:make-block-definition
                               :name "Marker"
                               :entities (list (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :height 20d0
                                                              :string "A \"B\"" :angle 0.3d0)
                                               (dom:make-hatch :id 2 :layer-id 0
                                                               :coordinates (doubles 0d0 0d0 40d0 0d0 40d0 40d0)
                                                               :pattern :lines :angle 0.7d0 :spacing 10d0))))
    (let ((layer (dom:add-layer drawing "doors" (dom:make-rgb 9 8 7))))
      (dom:apply-changes drawing
                         (dom:create-entities-changes
                          (list (a-placement :id 1 :block (dom:find-block drawing "door") :x 100d0)
                                (a-placement :id 2 :layer-id (dom:layer-id layer) :block (dom:find-block drawing "door")
                                             :x 300d0 :y 50d0 :scale 2d0 :rotation 1.2d0 :flipped t)
                                (a-placement :id 3 :block (dom:find-block drawing "marker") :x 0d0 :y 500d0)))))
    drawing))

(test blocks-and-placements-are-written-and-read-back-exactly
  (let* ((drawing (block-file-drawing))
         (text (persist:drawing-to-string drawing))
         (back (persist:parse-drawing-text text)))
    (is (search (format nil " (blocks~%  (block (name \"door\")") text) "sorted by name, case aside: door, Marker")
    (is (search "(insert (id 2) (layer 1) (block \"door\") (x 300.0) (y 50.0) (scale 2.0) (rotation 1.2) (flipped t))"
                text))
    (is (equal '("door" "Marker") (dom:block-names back)))
    (is (equal (entity-values drawing) (entity-values back)))
    (is (string= text (persist:drawing-to-string back)))
    (is (eq (dom:insert-block (dom:store-find (dom:drawing-store back) 1))
            (dom:insert-block (dom:store-find (dom:drawing-store back) 2)))
        "two placements of one block share its definition")
    (is (block-near-p (placed-ends (dom:store-find (dom:drawing-store drawing) 2))
                      (placed-ends (dom:store-find (dom:drawing-store back) 2))))))

(test the-blocks-come-before-the-entities-that-place-them
  (let ((text (persist:drawing-to-string (block-file-drawing))))
    (is (< (search "(blocks" text) (search "(entities" text)))
    (is (< (search "(layers" text) (search "(blocks" text)))))

(test a-file-with-a-placement-of-an-unknown-block-is-refused-with-its-position
  (let ((text (replace-once-text (persist:drawing-to-string (block-file-drawing)) "(block \"door\") (x 100.0)"
                                 "(block \"gate\") (x 100.0)")))
    (handler-case (progn (persist:parse-drawing-text text) (fail "an unknown block was accepted"))
      (base:drawing-file-content-error (condition)
        (is (search "unknown block \"gate\"" (princ-to-string condition)))
        (is (integerp (base:drawing-file-error-line condition)))))))

(test the-reader-refuses-a-block-that-is-not-valid
  (let ((text (persist:drawing-to-string (block-file-drawing))))
    (dolist (broken (list (replace-once-text text "(block (name \"Marker\")" "(block (name \"door\")")
                          (replace-once-text text "(block (name \"Marker\")" "(block (name \"\")")
                          (replace-once-text text "(block (name \"Marker\")" "(block (title \"Marker\")")
                          (replace-once-text text " (blocks" " (blokcs")
                          (replace-once-text text "(scale 2.0)" "(scale 0.0)")
                          (replace-once-text text "(flipped t)" "(flipped 5)")))
      (signals base:drawing-file-error (persist:parse-drawing-text broken)))))

(test a-block-cannot-hold-a-placement-in-a-file-either
  (let ((text (replace-once-text (persist:drawing-to-string (block-file-drawing))
                                 (format nil "(block (name \"Marker\")~%   (entities~%")
                                 (format nil "(block (name \"Marker\")~%   (entities~%~a~%"
                                         (concatenate 'string
                                                      "   (insert (id 9) (layer 0) (block \"door\") (x 0.0) (y 0.0) "
                                                      "(scale 1.0) (rotation 0.0) (flipped nil))")))))
    (signals base:drawing-file-error (persist:parse-drawing-text text))))

(test an-older-file-has-no-blocks-and-cannot-have-a-placement
  (let ((text (persist:drawing-to-string (block-file-drawing))))
    (dolist (version '(1 2 3 4))
      (handler-case (progn (persist:parse-drawing-text
                            (replace-once-text text
                                               (format nil "(format-version ~d)" persist:+supported-format-version+)
                                               (format nil "(format-version ~d)" version)))
                           (fail "a version ~d file with a block was accepted" version))
        (base:drawing-file-error ())))
    (let ((plain (persist:drawing-to-string (dom:make-drawing))))
      (is (null (dom:block-names (persist:parse-drawing-text (as-older-version plain 4))))
          "a version 4 file without the section reads as a drawing without blocks"))))

(test a-version-4-file-is-read-and-saved-as-the-newest-version
  (let* ((fixture (fixture-text "v4-plan.bcad"))
         (drawing (persist:parse-drawing-text fixture))
         (written (persist:drawing-to-string drawing)))
    (is (search "(format-version 4)" fixture))
    (is (= 15 (dom:drawing-entity-count drawing)))
    (is (null (dom:block-names drawing)))
    (is (string= (as-newest-version fixture 4) written))))

(test a-version-5-file-is-read-and-saved-as-the-newest-version
  (let* ((fixture (fixture-text "v5-plan.bcad"))
         (drawing (persist:parse-drawing-text fixture))
         (written (persist:drawing-to-string drawing)))
    (is (search "(format-version 5)" fixture))
    (is (= 9 (dom:drawing-entity-count drawing)))
    (is (equal '("door") (dom:block-names drawing)) "the blocks of version 5 are still there")
    (is (string= (as-newest-version fixture 5) written))
    (is (every (lambda (hatch) (null (dom:hatch-holes hatch)))
               (remove :hatch (dom:drawing-entities drawing) :key #'dom:entity-kind :test-not #'eq))
        "a hatch of version 5 has no holes")))

;;; Drawing, plotting and exporting

(test a-placement-is-drawn-with-the-entities-it-places
  (let ((application (make-test-app :drawing (block-drawing (a-placement :id 1 :x 0d0 :y 0d0 :scale 5d0)))))
    (set-test-view application :scale 0.5d0 :center-x 250d0 :center-y 0d0)
    (app:app-render application)
    (is (= 1 (scene:scene-result-drawn-entities (app:app-last-scene-result application))))
    (dom:set-layer-visible (app:app-drawing application) 0 nil)
    (app:app-request-redraw application)
    (app:app-render application)
    (is (= 0 (scene:scene-result-drawn-entities (app:app-last-scene-result application))) "a hidden layer hides it")))

(test a-placement-is-plotted-and-exported-to-dxf-as-the-entities-it-places
  (let ((drawing (block-drawing (a-placement :id 1 :x 1000d0 :y 0d0 :rotation 0.4d0))))
    (let ((plan (plot:plan-plot drawing (plot:make-plot-settings))))
      (is (= 1 (plot:plot-plan-entity-count plan)))
      (is (= 2 (length (plot:plot-plan-items plan))) "the line and the circle"))
    (multiple-value-bind (text report) (dxf:export-drawing-text drawing)
      (let ((types (dxf-types text)))
        (is (= 1 (count "LINE" types :test #'string=)))
        (is (= 1 (count "CIRCLE" types :test #'string=)) "a circle is a circle"))
      (is (find-if (lambda (note) (search "Blocks were written" note)) (dxf:report-notes report))))))

;;; The clipboard

(test copying-a-placement-takes-its-block-and-pasting-gives-it-to-a-drawing-that-lacks-it
  (let* ((source (block-drawing (a-placement :id 1 :x 100d0)))
         (snippet (dom:extract-entities source (list (dom:store-find (dom:drawing-store source) 1))))
         (target (dom:make-drawing)))
    (is (equal '("door") (dom:block-names snippet)))
    (let ((text (persist:drawing-to-string snippet)))
      (is (equal '("door") (dom:block-names (persist:parse-drawing-text text))) "it survives the clipboard text"))
    (let ((pasted (dom:pasted-entities target snippet 10d0 20d0)))
      (is (equal '("door") (dom:block-names target)) "the block came along")
      (is (= 110d0 (dom:insert-x (first pasted))))
      (is (eq (dom:find-block target "door") (dom:insert-block (first pasted)))))))

(test pasting-a-placement-into-a-drawing-that-has-a-block-of-that-name-uses-that-block
  (let* ((source (block-drawing (a-placement :id 1)))
         (snippet (dom:extract-entities source (list (dom:store-find (dom:drawing-store source) 1))))
         (target (dom:make-drawing))
         (own (dom:make-block-definition :name "Door" :entities (list (block-line 1 0 0 9 9)))))
    (dom:define-block target own)
    (let ((pasted (dom:pasted-entities target snippet 0d0 0d0)))
      (is (eq own (dom:insert-block (first pasted))) "the drawing's own block, whatever the copy said")
      (is (= 1 (length (dom:block-names target)))))))

;;; The console

(test the-console-makes-places-and-explodes-blocks
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(line 1000 500 1100 500) (circle 1050 500 50)")
    (is (equal '("3") (ran-ok context "(make-block \"door\" '(1 2) :base-x 1000 :base-y 500)")))
    (is (equal '("(\"door\")") (ran-ok context "(blocks)")))
    (is (eq :insert (dom:entity-kind (stored context 3))))
    (is (equal '("4") (ran-ok context "(insert \"DOOR\" 5000 0 :scale 2 :rotation 90 :flipped t)")))
    (let ((insert (stored context 4)))
      (is (and (= 2d0 (dom:insert-scale insert)) (dom:insert-flipped insert)))
      (is (< (abs (- (/ pi 2) (dom:insert-rotation insert))) 1d-9)))
    (is (equal '("1") (ran-ok context "(explode 4)")))
    (is (= 3 (dom:drawing-entity-count drawing)) "the first placement and the two entities of the second")
    (is (equal '("0") (ran-ok context "(explode 5)")) "ids that are not placements are skipped")))

(test with-block-draws-into-a-block-instead-of-the-drawing
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (is (equal '("\"chair\"") (ran-ok context "(with-block \"chair\" (rect-size 0 0 400 400) (line 0 0 400 400))")))
    (is (= 0 (dom:drawing-entity-count drawing)) "nothing was drawn")
    (is (= 2 (length (dom:block-definition-entities (dom:find-block drawing "chair")))))
    (ran-ok context "(insert \"chair\" 100 100)")
    (is (= 1 (dom:drawing-entity-count drawing)))))

(test the-console-says-why-a-block-cannot-be-made-or-placed
  (let ((context (script-env)))
    (is (search "no block called gate" (failure-of context "(insert \"gate\" 0 0)")))
    (ran-ok context "(line 0 0 10 10)")
    (is (search "needs a name" (failure-of context "(make-block \"\" 1)")))
    (is (search "cannot hold a block" (failure-of context "(with-block \"x\" (insert \"y\" 0 0))")))
    (is (search "scale" (failure-of (progn (ran-ok context "(with-block \"y\" (line 0 0 1 1))") context)
                                    "(insert \"y\" 0 0 :scale 0)")))))

(test the-help-lists-the-block-commands-and-a-script-of-a-drawing-makes-its-blocks-again
  (let ((overview (script:script-result-output (run-script (script-env) "(help)"))))
    (is (search "with-block make-block insert explode spread blocks" overview)))
  (let* ((drawing (block-file-drawing))
         (again (dom:make-drawing))
         (result (run-script (script-env again) (script:drawing-script drawing))))
    (is (eq :ok (script:script-result-status result)) "~a" (script:script-result-message result))
    (is (equal (dom:block-names drawing) (dom:block-names again)))
    (is (equal (entity-shapes drawing) (entity-shapes again)))
    (dolist (name (dom:block-names drawing))
      (is (equal (mapcar #'dom:entity-kind (dom:block-definition-entities (dom:find-block drawing name)))
                 (mapcar #'dom:entity-kind (dom:block-definition-entities (dom:find-block again name))))))))

;;; The tools

(defun blocks-app (&rest entities)
  "Return an application, with the grid snap and the object snap off, holding ENTITIES, or a line and a circle."
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (app:commit-changes application
                        (dom:create-entities-changes (or entities
                                                         (list (block-line 1 -500 0 500 0)
                                                               (dom:make-circle :id 2 :layer-id 0 :center-x 0d0
                                                                                :center-y 0d0 :radius 300d0))))
                        "entity.create")
    application))

(defun make-block-through-the-app (application name base-x base-y)
  "Select everything, choose Make block, type NAME and click the base point."
  (press application :a :control)
  (press application :b :shift)
  (type-text application name)
  (press application :enter)
  (click application base-x base-y))

(test making-a-block-from-the-selection-asks-the-name-then-the-base-point
  (let ((application (blocks-app)))
    (press application :a :control)
    (click-widget application :modify)
    (is (member :make-block (mapcar (lambda (row) (second (ui:widget-id row))) (menu-rows application))))
    (click-widget application '(:menu-item :make-block))
    (is (string= "Make block" (prompt-title-of application)))
    (is (eq :text (app:prompt-kind (app:app-prompt application))))
    (type-text application "door")
    (press application :enter)
    (is (eq :make-block (app:tool-kind (app:app-tool application))))
    (is (search "door" (app:tool-prompt (app:app-tool application) application)))
    (is (ui:widget-active (find-widget application :modify)))
    (click application 500 0)
    (is (equal '(3) (mapcar #'dom:entity-id (entities-of application))) "two entities became one placement")
    (let ((insert (last-entity application)))
      (is (eq :insert (dom:entity-kind insert)))
      (is (= 500d0 (dom:insert-x insert)))
      (is (= 0d0 (dom:insert-y insert))))
    (is (equal '("door") (dom:block-names (app:app-drawing application))))
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (equal '(3) (app:selection-ids application)) "the placement is what is selected")
    (is (search "Made the block door of 2 entities" (message-text application)))
    (is (equal "door" (app:app-last-block-name application)))
    (app:operation-undo application)
    (is (equal '(1 2) (mapcar #'dom:entity-id (entities-of application))) "one step of undo")))

(test making-a-block-needs-a-selection-and-a-good-name
  (let ((application (blocks-app)))
    (press application :b :shift)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "Select what the block is made of" (message-text application)))
    (is (null (app:app-prompt application)))
    (press application :a :control)
    (press application :b :shift)
    (type-text application "   ")
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))) "no name")
    (is (search "needs a name" (message-text application)))
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (press application :b :shift)
    (press application :escape)
    (is (null (app:app-prompt application)) "Esc leaves the question")))

(test the-name-of-a-block-in-use-is-refused-at-the-click
  (let ((application (blocks-app)))
    (make-block-through-the-app application "door" 0 0)
    (app:operation-set-tool application :line)
    (click application -500 -500)
    (click application 500 -500)
    (press application :escape)
    (press application :escape)
    (app:select-ids application (list (dom:entity-id (last-entity application))))
    (press application :b :shift)
    (type-text application "door")
    (press application :enter)
    (click application 0 0)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "already in use" (message-text application)))
    (is (= 2 (length (entities-of application))) "nothing changed")))

(test placing-a-block-asks-which-then-takes-a-click-and-a-click-for-the-turn
  (let ((application (blocks-app)))
    (make-block-through-the-app application "door" 0 0)
    (choose-tool application :insert)
    (is (string= "Insert block" (prompt-title-of application)))
    (is (search "Blocks: door" (app:prompt-message (app:app-prompt application))))
    (is (string= "door" (ui:edit-state-text (app:prompt-edit (app:app-prompt application)))) "the one made last")
    (press application :enter)
    (is (eq :insert (app:tool-kind (app:app-tool application))))
    (move-pointer-to application 1500 800)
    (is (= 1 (length (app:tool-preview-entities (app:app-tool application) application)))
        "the placement follows the pointer")
    (click application 1500 800)
    (is (app:tool-in-progress-p (app:app-tool application)))
    (is (search "to turn it" (app:tool-prompt (app:app-tool application) application)))
    (move-pointer-to application 1500 1800)
    (click application 1500 1800)
    (let ((insert (last-entity application)))
      (is (= 1500d0 (dom:insert-x insert)))
      (is (< (abs (- (/ pi 2) (dom:insert-rotation insert))) 1d-6) "turned toward the second click"))
    (is (not (app:tool-in-progress-p (app:app-tool application))) "ready for another")
    (app:operation-undo application)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))))

(test enter-leaves-the-block-straight-and-typed-numbers-are-the-scale-then-the-turn
  (let ((application (blocks-app)))
    (make-block-through-the-app application "door" 0 0)
    (app:operation-insert-block application)
    (press application :enter)
    (type-text application "2.5")
    (press application :enter)
    (is (search "2.5" (app:tool-prompt (app:app-tool application) application)) "the scale is in the prompt")
    (click application 1000 500)
    (press application :enter)
    (let ((insert (last-entity application)))
      (is (= 2.5d0 (dom:insert-scale insert)))
      (is (= 0d0 (dom:insert-rotation insert))))
    (click application -1000 -500)
    (type-text application "90")
    (press application :enter)
    (let ((insert (last-entity application)))
      (is (< (abs (- (/ pi 2) (dom:insert-rotation insert))) 1d-9) "typed after the click it is the turn in degrees")
      (is (= 2.5d0 (dom:insert-scale insert)) "and the scale is kept"))
    (type-text application "0")
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))) "a scale of zero is refused")))

(test a-name-that-is-not-a-block-is-asked-again
  (let ((application (blocks-app)))
    (make-block-through-the-app application "door" 0 0)
    (app:operation-insert-block application)
    (type-text application "gate")
    (press application :enter)
    (is (eq :text (app:prompt-kind (app:app-prompt application))) "asked again")
    (is (search "no block called gate" (app:prompt-message (app:app-prompt application))))
    (type-text application "DOOR")
    (press application :enter)
    (is (eq :insert (app:tool-kind (app:app-tool application))))
    (is (search "door" (app:tool-prompt (app:app-tool application) application)) "with the name as it was made")))

(test with-no-block-the-insert-tool-says-so
  (let ((application (blocks-app)))
    (press application :i)
    (is (null (app:app-prompt application)))
    (is (eq :error (cdr (app:app-message application))))
    (is (search "no blocks yet" (message-text application)))))

(test escape-cancels-the-placement-and-then-leaves-the-tool
  (let ((application (blocks-app)))
    (make-block-through-the-app application "door" 0 0)
    (app:operation-insert-block application)
    (press application :enter)
    (click application 1000 500)
    (press application :escape)
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (is (eq :insert (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))))

(test exploding-takes-a-placement-apart-and-selects-what-came-out
  (let ((application (blocks-app)))
    (make-block-through-the-app application "door" 0 0)
    (press application :x :shift)
    (is (equal '(4 5) (mapcar #'dom:entity-id (entities-of application))))
    (is (equal '(4 5) (app:selection-ids application)))
    (is (search "Exploded 1 entity" (message-text application)))
    (press application :x :shift)
    (is (eq :error (cdr (app:app-message application))) "no block is selected now")
    (is (search "That cannot be exploded" (message-text application)))
    (app:operation-undo application)
    (is (equal '(3) (mapcar #'dom:entity-id (entities-of application))))))

(test the-inspector-shows-the-block-and-the-scale-of-a-selected-placement
  (let ((application (blocks-app)))
    (make-block-through-the-app application "door" 0 0)
    (let ((block-row (find-widget application '(:property :block)))
          (scale-row (find-widget application '(:property :scale))))
      (is (not (null block-row)))
      (is (string= "door" (ui:widget-label block-row)))
      (is (eq :label (ui:widget-kind block-row)) "it is not a field that can be edited")
      (is (not (null scale-row)))
      (is (string= "1" (ui:widget-label scale-row))))
    (app:operation-edit-property application 3 :scale 2d0)
    (is (= 2d0 (dom:insert-scale (last-entity application))))
    (is (string= "2" (ui:widget-label (find-widget application '(:property :scale)))))))

(test the-block-tools-and-their-messages-speak-every-language
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (blocks-app)))
        (press application :a :control)
        (click-widget application :modify)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the modify menu in ~a" language)
        (press application :escape)
        (press application :b :shift)
        (is (null (untranslated-texts (list (prompt-title-of application)
                                            (app:prompt-message (app:app-prompt application)))
                                      language))
            "the name question in ~a" language)
        (type-text application "door")
        (press application :enter)
        (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language)))
        (click application 0 0)
        (is (null (untranslated-texts (list (message-text application)) language)) "the notice in ~a" language)
        (app:operation-insert-block application)
        (is (null (untranslated-texts (list (prompt-title-of application)
                                            (app:prompt-message (app:app-prompt application)))
                                      language))
            "the block question in ~a" language)
        (press application :enter)
        (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language)))
        (click application 500 500)
        (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language)))
        (app:select-ids application (list (dom:entity-id (last-entity application))))
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the inspector in ~a" language)))))
