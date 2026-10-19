(in-package #:borba-cad/tests)

(def-suite arrays
  :description "Arrays of copies in a grid, round a point and along a path, and the forms that ask for them."
  :in all)

(in-suite arrays)

;;; Helpers

(defun path-coordinates (&rest values)
  "Return the vertices of a path as a flat array.

VALUES: x and y, vertex after vertex."
  (make-array (length values) :element-type 'double-float
                              :initial-contents (mapcar (lambda (value) (float value 1d0)) values)))

(defun array-drawing (&rest entities)
  "Return a drawing that holds ENTITIES.

ENTITIES: the entities."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes entities))
    drawing))

(defun made-by (changes)
  "Return the entities that a change set adds, in order.

CHANGES: a change set."
  (mapcar #'dom:change-after changes))

(defun array-line-ends (line)
  "Return the four coordinates of a line as a list.

LINE: a line entity."
  (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line)))

(defun all-close-p (expected
                    actual)
  "Return true when two lists of numbers agree one by one.

EXPECTED: the numbers it should be.
ACTUAL: the numbers it is."
  (and (= (length expected) (length actual))
       (every #'approx= expected actual)))

;;; Walking along a path

(test a-path-has-the-length-of-its-segments
  (let ((l-shape (path-coordinates 0 0 3 0 3 4)))
    (is (approx= 7d0 (geo:path-length l-shape nil)))
    (is (approx= 12d0 (geo:path-length l-shape t)) "a closed path adds the way back, 5")
    (is (equalp #(3d0 4d0) (geo:path-segment-lengths l-shape nil)))))

(test points-along-a-path-know-where-it-is-heading
  (let ((points (geo:path-points-at (path-coordinates 0 0 3 0 3 4)
                                    nil
                                    '(0 1.5 3 5 7 99))))
    (is (all-close-p (list 0 0 0) (first points)))
    (is (all-close-p (list 1.5 0 0) (second points)))
    (is (all-close-p (list 3 0 (/ pi 2)) (third points)) "a vertex has the direction of the segment that leaves it")
    (is (all-close-p (list 3 2 (/ pi 2)) (fourth points)))
    (is (all-close-p (list 3 4 (/ pi 2)) (fifth points)) "the end has the direction of the last segment")
    (is (all-close-p (list 3 4 (/ pi 2)) (sixth points)) "past the end is the end")))

(test a-closed-path-goes-back-to-its-start
  (let ((points (geo:path-points-at (path-coordinates 0 0 4 0 4 3)
                                    t
                                    '(7 12))))
    (is (all-close-p (list 4 3 (atan -3d0 -4d0)) (first points)) "7 is at the end of the second side")
    (is (all-close-p (list 0 0 (atan -3d0 -4d0)) (second points)) "12 is back at the start")))

(test equal-steps-end-at-the-end-of-an-open-path-and-a-step-short-of-a-closed-one
  (is (equal '(0d0 2d0 4d0 6d0 8d0) (geo:path-even-distances 8d0 nil 5)))
  (is (equal '(0d0 2d0 4d0 6d0) (geo:path-even-distances 8d0 t 4)))
  (is (equal '(0d0) (geo:path-even-distances 8d0 nil 1))))

(test a-spacing-gives-as-many-items-as-fit
  (is (equal '(0d0 3d0 6d0 9d0) (geo:path-spaced-distances 10d0 nil 3d0)))
  (is (equal '(0d0 3d0 6d0 9d0) (geo:path-spaced-distances 9d0 nil 3d0)) "the end counts when the spacing divides")
  (is (equal '(0d0 3d0 6d0) (geo:path-spaced-distances 9d0 t 3d0)) "the end of a closed path is its start")
  (is (equal '(0d0 3d0 6d0 9d0) (geo:path-spaced-distances 10d0 t 3d0)))
  (is (equal '(0d0) (geo:path-spaced-distances 2d0 nil 3d0)) "one item when it is longer than the path"))

;;; The entities that are paths

(test the-shapes-that-make-a-path
  (multiple-value-bind (coordinates closed) (dom:entity-path (cut-line 1 0 0 30 40))
    (is (equalp #(0d0 0d0 30d0 40d0) coordinates))
    (is (not closed)))
  (multiple-value-bind (coordinates closed) (dom:entity-path (spread-rectangle 1 0 0 30 40))
    (is (= 8 (length coordinates)))
    (is (not (null closed))))
  (multiple-value-bind (coordinates closed) (dom:entity-path (cut-circle 1 0 0 100))
    (is (not (null closed)))
    (is (approx= (* 2 pi 100) (geo:path-length coordinates closed) 1d-4) "the polygon has nearly the circle's length"))
  (multiple-value-bind (coordinates closed) (dom:entity-path (cut-arc 1 0 0 100 0 (/ pi 2)))
    (is (not closed))
    (is (approx= (* 50 pi) (geo:path-length coordinates closed) 1d-4))
    (is (all-close-p (list 100 0) (list (aref coordinates 0) (aref coordinates 1))))
    (is (all-close-p (list 0 100) (list (aref coordinates (- (length coordinates) 2))
                                        (aref coordinates (- (length coordinates) 1))))))
  (is (null (dom:entity-path (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :string "a" :height 10d0)))))

;;; A grid

(test a-grid-adds-a-copy-for-every-cell-but-the-first
  (let* ((drawing (array-drawing (spread-rectangle 1 0 0 100 100)))
         (changes (dom:rectangular-array-changes drawing '(1) 2 3 200d0 150d0))
         (copies (made-by changes)))
    (is (= 5 (length changes)))
    (is (equal '(2 3 4 5 6) (mapcar #'dom:change-entity-id changes)) "new ids")
    (is (every #'null (mapcar #'dom:change-before changes)) "all are additions")
    (is (equal '((150d0 0d0) (300d0 0d0) (0d0 200d0) (150d0 200d0) (300d0 200d0))
               (mapcar (lambda (rectangle) (list (dom:rectangle-min-x rectangle) (dom:rectangle-min-y rectangle)))
                       copies))
        "row by row, columns to the right and rows going up")
    (is (every (lambda (rectangle) (and (= 100d0 (dom:rectangle-width rectangle))
                                        (= 0 (dom:entity-layer-id rectangle))))
               copies))))

(test a-negative-spacing-goes-the-other-way-and-an-angle-turns-the-grid
  (let ((drawing (array-drawing (cut-line 1 0 0 10 0))))
    (is (all-close-p '(-50 0 -40 0)
                     (array-line-ends (first (made-by (dom:rectangular-array-changes drawing '(1) 1 2 0d0 -50d0))))))
    (is (all-close-p '(0 -30 10 -30)
                     (array-line-ends (first (made-by (dom:rectangular-array-changes drawing '(1) 2 1 -30d0 0d0))))))
    (let ((turned (made-by (dom:rectangular-array-changes drawing
                                                          '(1)
                                                          2
                                                          2
                                                          10d0
                                                          20d0
                                                          :angle (/ pi 2)))))
      (is (all-close-p '(0 20 10 20) (array-line-ends (first turned))) "a column step along the turned x, which is up")
      (is (all-close-p '(-10 0 0 0) (array-line-ends (second turned))) "a row step 90 degrees on from it, to the left")
      (is (all-close-p '(-10 20 0 20) (array-line-ends (third turned)))))))

(test every-entity-of-the-selection-is-copied-in-each-cell
  (let* ((drawing (array-drawing (cut-line 1 0 0 10 0) (cut-circle 2 5 5 3)))
         (copies (made-by (dom:rectangular-array-changes drawing '(1 2) 1 3 0d0 100d0))))
    (is (equal '(:line :circle :line :circle) (mapcar #'dom:entity-kind copies)))
    (is (equal '(3 4 5 6) (mapcar #'dom:entity-id copies)))))

(test a-grid-is-refused-when-it-would-be-nothing-or-a-pile
  (let ((drawing (array-drawing (cut-line 1 0 0 10 0))))
    (is (search "at least two items"
                (handler-case (dom:rectangular-array-changes drawing '(1) 1 1 10d0 10d0)
                  (base:edit-refused (condition) (princ-to-string condition)))))
    (is (search "spacing of the columns"
                (handler-case (dom:rectangular-array-changes drawing '(1) 1 3 10d0 0d0)
                  (base:edit-refused (condition) (princ-to-string condition)))))
    (is (search "spacing of the rows"
                (handler-case (dom:rectangular-array-changes drawing '(1) 3 1 0d0 10d0)
                  (base:edit-refused (condition) (princ-to-string condition)))))
    (is (search "at most"
                (handler-case (dom:rectangular-array-changes drawing '(1) 200 200 10d0 10d0)
                  (base:edit-refused (condition) (princ-to-string condition))))
        "40,000 entities are too many")
    (is (search "Select what to repeat"
                (handler-case (dom:rectangular-array-changes drawing '(99) 2 2 10d0 10d0)
                  (base:edit-refused (condition) (princ-to-string condition))))
        "a missing entity is nothing to repeat")))

(test a-grid-skips-what-is-on-a-locked-layer
  (let* ((drawing (array-drawing (cut-line 1 0 0 10 0) (cut-line 2 0 5 10 5)))
         (layer (dom:add-layer drawing "fixed" (dom:make-rgb 1 2 3))))
    (dom:apply-changes drawing
                       (list (dom:make-change 2
                                              (dom:store-find (dom:drawing-store drawing) 2)
                                              (dom:entity-with-layer (dom:store-find (dom:drawing-store drawing) 2)
                                                                     (dom:layer-id layer)))))
    (dom:set-layer-locked drawing (dom:layer-id layer) t)
    (multiple-value-bind (changes skipped) (dom:rectangular-array-changes drawing '(1 2) 1 2 0d0 50d0)
      (is (= 1 (length changes)))
      (is (= 1 skipped)))))

(test an-array-is-one-step-of-undo
  (let ((drawing (array-drawing (cut-line 1 0 0 10 0)))
        (history (dom:make-history)))
    (dom:history-commit history drawing (dom:rectangular-array-changes drawing '(1) 2 2 10d0 10d0))
    (is (= 4 (dom:drawing-entity-count drawing)))
    (dom:history-undo history drawing)
    (is (= 1 (dom:drawing-entity-count drawing)))
    (dom:history-redo history drawing)
    (is (= 4 (dom:drawing-entity-count drawing)))))

;;; Round a point

(test a-polar-array-round-a-full-turn-has-equal-steps
  (let* ((drawing (array-drawing (cut-line 1 10 0 20 0)))
         (copies (made-by (dom:polar-array-changes drawing '(1) 0d0 0d0 4 (* 2 pi)))))
    (is (= 3 (length copies)))
    (is (all-close-p '(0 10 0 20) (array-line-ends (first copies))) "a quarter turn")
    (is (all-close-p '(-10 0 -20 0) (array-line-ends (second copies))))
    (is (all-close-p '(0 -10 0 -20) (array-line-ends (third copies))))))

(test a-polar-array-over-part-of-a-circle-ends-at-the-end-of-the-angle
  (let* ((drawing (array-drawing (cut-line 1 10 0 20 0)))
         (copies (made-by (dom:polar-array-changes drawing '(1) 0d0 0d0 3 pi))))
    (is (all-close-p '(0 10 0 20) (array-line-ends (first copies))) "halfway")
    (is (all-close-p '(-10 0 -20 0) (array-line-ends (second copies))) "and at the end")
    (let ((clockwise (made-by (dom:polar-array-changes drawing '(1) 0d0 0d0 3 (- pi)))))
      (is (all-close-p '(0 -10 0 -20) (array-line-ends (first clockwise))) "a negative angle goes clockwise"))))

(test a-polar-array-can-keep-the-copies-as-they-were-drawn
  (let* ((drawing (array-drawing (cut-line 1 10 0 20 0)))
         (copies (made-by (dom:polar-array-changes drawing '(1) 0d0 0d0 4 (* 2 pi) :rotate-items nil))))
    (is (all-close-p '(-5 15 5 15) (array-line-ends (first copies)))
        "the center of the line, (15, 0), goes a quarter turn round, and the line stays horizontal")
    (is (all-close-p '(-20 0 -10 0) (array-line-ends (second copies))))))

(test a-polar-array-needs-items-and-an-angle
  (let ((drawing (array-drawing (cut-line 1 10 0 20 0))))
    (is (search "at least two"
                (handler-case (dom:polar-array-changes drawing '(1) 0d0 0d0 1 pi)
                  (base:edit-refused (condition) (princ-to-string condition)))))
    (is (search "must not be zero"
                (handler-case (dom:polar-array-changes drawing '(1) 0d0 0d0 4 0d0)
                  (base:edit-refused (condition) (princ-to-string condition)))))))

;;; Along a path

(test copies-along-a-line-are-at-equal-steps-from-its-start-to-its-end
  (let* ((drawing (array-drawing (cut-circle 1 0 0 5) (cut-line 2 0 0 100 0)))
         (copies (made-by (dom:path-array-changes drawing '(1) 2 :count 5))))
    (is (equal '(25d0 50d0 75d0 100d0) (mapcar #'dom:circle-center-x copies)))
    (is (every (lambda (circle) (= 0d0 (dom:circle-center-y circle))) copies))))

(test copies-along-a-line-by-a-spacing-are-as-many-as-fit
  (let* ((drawing (array-drawing (cut-circle 1 0 0 5) (cut-line 2 0 0 100 0)))
         (copies (made-by (dom:path-array-changes drawing '(1) 2 :spacing 30d0))))
    (is (equal '(30d0 60d0 90d0) (mapcar #'dom:circle-center-x copies)))))

(test the-path-is-never-copied-even-when-it-is-selected
  (let* ((drawing (array-drawing (cut-circle 1 0 0 5) (cut-line 2 0 0 100 0)))
         (copies (made-by (dom:path-array-changes drawing '(1 2) 2 :count 3))))
    (is (equal '(:circle :circle) (mapcar #'dom:entity-kind copies)))))

(test copies-can-turn-to-follow-the-path
  (let* ((corner (dom:make-polyline :id 2
                                    :layer-id 0
                                    :coordinates (path-coordinates 0 0 100 0 100 100)
                                    :closed nil))
         (drawing (array-drawing (cut-line 1 0 0 10 0) corner))
         (copies (made-by (dom:path-array-changes drawing '(1) 2 :count 3 :align t)))
         (straight (made-by (dom:path-array-changes drawing '(1) 2 :count 3))))
    (is (all-close-p '(105 -5 105 5) (array-line-ends (first copies)))
        "the corner: the line turned a quarter turn about its center, which went to (105, 0)")
    (is (all-close-p '(105 95 105 105) (array-line-ends (second copies))))
    (is (all-close-p '(100 0 110 0) (array-line-ends (first straight))) "turning is a choice")))

(test copies-round-a-closed-path-leave-the-end-to-the-start
  (let* ((drawing (array-drawing (cut-circle 1 100 0 3) (cut-circle 2 0 0 100)))
         (copies (made-by (dom:path-array-changes drawing '(1) 2 :count 4))))
    (is (= 3 (length copies)) "four items round, the fourth item is not on the first")
    (is (all-close-p '(0 100) (list (dom:circle-center-x (first copies)) (dom:circle-center-y (first copies)))))
    (is (all-close-p '(-100 0) (list (dom:circle-center-x (second copies)) (dom:circle-center-y (second copies)))))
    (is (all-close-p '(0 -100) (list (dom:circle-center-x (third copies)) (dom:circle-center-y (third copies)))))))

(test an-array-along-a-path-follows-the-shape-from-where-the-originals-are
  (let* ((drawing (array-drawing (cut-circle 1 7 7 1) (cut-line 2 0 0 100 0)))
         (copies (made-by (dom:path-array-changes drawing '(1) 2 :count 3))))
    (is (equal '(57d0 107d0) (mapcar #'dom:circle-center-x copies)) "moved by what the path moved")
    (is (equal '(7d0 7d0) (mapcar #'dom:circle-center-y copies)))))

(test an-array-along-a-path-says-what-is-wrong
  (let ((drawing (array-drawing (cut-circle 1 0 0 5)
                                (cut-line 2 0 0 100 0)
                                (dom:make-text :id 3 :layer-id 0 :x 0d0 :y 0d0 :string "a" :height 10d0))))
    (flet ((refusal (path &rest options)
             (handler-case (apply #'dom:path-array-changes drawing '(1) path options)
               (base:edit-refused (condition) (princ-to-string condition)))))
      (is (search "not in the drawing" (refusal 99 :count 3)))
      (is (search "A path is a line" (refusal 3 :count 3)))
      (is (search "number of items or a spacing" (refusal 2)))
      (is (search "at least two" (refusal 2 :count 1)))
      (is (search "at least two" (refusal 2 :spacing 500d0)) "only the start fits"))))

;;; The form

(defun typed-in-form (application
                      &rest texts)
  "Fill the lines of the open form with TEXTS, a line each, going down with Tab and typing over what was there.

APPLICATION: the application.
TEXTS: what to type in each line, from the first."
  (loop for text in texts
        for index from 0
        do (when (plusp index)
             (press application :tab))
           (when text
             (type-text application text))))

(defun array-app (&rest entities)
  "Return an application that holds ENTITIES, all of them selected, and the Array question open.

ENTITIES: the entities."
  (let ((application (apply #'reshaping-app entities)))
    (app:operation-select-all application)
    (choose-modify-tool-by-menu application :array)
    application))

(defun form-of (application)
  "Return the open prompt of APPLICATION, which must be a form.

APPLICATION: the application."
  (let ((prompt (app:app-prompt application)))
    (is (eq :form (app:prompt-kind prompt)))
    prompt))

(test the-array-question-offers-three-kinds-and-needs-a-selection
  (let ((application (reshaping-app (cut-line 1 0 0 100 0))))
    (choose-modify-tool-by-menu application :array)
    (is (null (app:app-prompt application)) "nothing is selected")
    (is (search "Select what to repeat" (message-text application)))
    (app:operation-select-all application)
    (press application :y :shift)
    (is (eq :choice (app:prompt-kind (app:app-prompt application))))
    (is (equal '("Grid" "Round a point" "Along a path" "Cancel")
               (mapcar #'car (app:prompt-choices (app:app-prompt application)))))
    (click-widget application '(:prompt-button 3))
    (is (null (app:app-prompt application)))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-grid-form-asks-the-numbers-and-says-how-many-entities-it-adds
  (let ((application (array-app (spread-rectangle 1 0 0 100 100))))
    (click-widget application '(:prompt-button 0))
    (let ((prompt (form-of application)))
      (is (equal '(:rows :columns :row-spacing :column-spacing :grid-angle)
                 (mapcar #'app:form-field-key (app:prompt-fields prompt))))
      (is (equal "3" (cdr (assoc :rows (app:form-values prompt)))))
      (is (search "Adds 8 entities" (ui:widget-label (find-widget application :prompt-info)))))
    (typed-in-form application "2" "4")
    (is (search "Adds 7 entities" (ui:widget-label (find-widget application :prompt-info))))))

(test the-grid-form-makes-the-array-as-one-step-and-keeps-what-was-typed
  (let ((application (array-app (spread-rectangle 1 0 0 100 100))))
    (click-widget application '(:prompt-button 0))
    (typed-in-form application "2" "3" "250" "300")
    (press application :enter)
    (is (null (app:app-prompt application)) "the form closed")
    (is (= 6 (dom:drawing-entity-count (app:app-drawing application))))
    (is (= 6 (hash-table-count (app:app-selection application))) "the originals and the copies are selected")
    (is (search "Added 5 entities" (message-text application)))
    (is (equal '((0d0 0d0) (300d0 0d0) (600d0 0d0) (0d0 250d0) (300d0 250d0) (600d0 250d0))
               (mapcar (lambda (rectangle) (list (dom:rectangle-min-x rectangle) (dom:rectangle-min-y rectangle)))
                       (entities-of application))))
    (app:operation-undo application)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (app:operation-select-all application)
    (press application :y :shift)
    (click-widget application '(:prompt-button 0))
    (is (equal "2" (cdr (assoc :rows (app:form-values (form-of application))))) "what was typed is the start")))

(test a-form-that-cannot-be-done-stays-open-and-says-why
  (let ((application (array-app (spread-rectangle 1 0 0 100 100))))
    (click-widget application '(:prompt-button 0))
    (typed-in-form application "abc")
    (is (search "Invalid input" (ui:widget-label (find-widget application :prompt-info))) "said before OK")
    (press application :enter)
    (is (eq :form (app:prompt-kind (app:app-prompt application))) "OK did not close it")
    (is (search "Invalid input" (app:prompt-problem (app:app-prompt application))))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (press application :a :control)
    (type-text application "2")
    (is (null (app:prompt-problem (app:app-prompt application))) "typing again clears the sentence")
    (press application :tab)
    (type-text application "1")
    (press application :tab)
    (type-text application "0")
    (press application :enter)
    (is (eq :form (app:prompt-kind (app:app-prompt application))))
    (is (search "spacing of the rows" (app:prompt-problem (app:app-prompt application)))
        "a spacing of zero is refused")
    (press application :escape)
    (is (null (app:app-prompt application)))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-form-moves-between-lines-with-tab-the-arrows-and-the-pointer
  (let ((application (array-app (spread-rectangle 1 0 0 100 100))))
    (click-widget application '(:prompt-button 0))
    (let ((prompt (form-of application)))
      (is (= 0 (app:prompt-active prompt)))
      (press application :tab)
      (is (= 1 (app:prompt-active prompt)))
      (press application :tab :shift)
      (press application :tab :shift)
      (is (= 4 (app:prompt-active prompt)) "going back from the first goes round to the last")
      (press application :down)
      (is (= 0 (app:prompt-active prompt)))
      (press application :up)
      (is (= 4 (app:prompt-active prompt)))
      (click-widget application '(:prompt-field 2))
      (is (= 2 (app:prompt-active prompt))))))

(test the-text-of-a-line-is-selected-when-it-gets-the-keyboard-so-typing-replaces-it
  (let ((application (array-app (spread-rectangle 1 0 0 100 100))))
    (click-widget application '(:prompt-button 0))
    (press application :tab)
    (type-text application "7")
    (is (equal "7" (cdr (assoc :columns (app:form-values (form-of application))))))
    (type-text application "5")
    (is (equal "75" (cdr (assoc :columns (app:form-values (form-of application))))) "then it adds")))

(test the-polar-array-asks-for-the-center-and-then-the-numbers
  (let ((application (array-app (cut-line 1 1000 0 2000 0))))
    (click-widget application '(:prompt-button 1))
    (is (eq :array-center (app:tool-kind (app:app-tool application))))
    (is (search "click the center" (app:tool-prompt (app:app-tool application) application)))
    (click application 0 0)
    (let ((prompt (form-of application)))
      (is (equal '(:items :sweep :turn) (mapcar #'app:form-field-key (app:prompt-fields prompt))))
      (is (equal '((:items . "6") (:sweep . "360") (:turn . :turn)) (app:form-values prompt))))
    (typed-in-form application "4")
    (press application :enter)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (all-close-p '(0 1000 0 2000) (array-line-ends (second (entities-of application)))))
    (is (all-close-p '(-1000 0 -2000 0) (array-line-ends (third (entities-of application)))))))

(test a-line-of-options-changes-with-space-the-arrows-and-the-pointer
  (let ((application (array-app (cut-line 1 1000 0 2000 0))))
    (click-widget application '(:prompt-button 1))
    (click application 0 0)
    (let ((prompt (form-of application)))
      (press application :tab)
      (press application :tab)
      (is (eq :turn (cdr (assoc :turn (app:form-values prompt)))))
      (press application :space)
      (is (eq :keep (cdr (assoc :turn (app:form-values prompt)))))
      (press application :right)
      (is (eq :turn (cdr (assoc :turn (app:form-values prompt)))) "it goes round")
      (press application :left)
      (is (eq :keep (cdr (assoc :turn (app:form-values prompt)))))
      (click-widget application '(:prompt-field 2))
      (is (eq :turn (cdr (assoc :turn (app:form-values prompt)))) "a click goes to the next"))))

(test the-polar-array-can-keep-the-copies-as-drawn
  (let ((application (array-app (cut-line 1 1000 0 2000 0))))
    (click-widget application '(:prompt-button 1))
    (click application 0 0)
    (typed-in-form application "4" nil)
    (press application :tab)
    (press application :space)
    (press application :enter)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (all-close-p '(-500 1500 500 1500) (array-line-ends (second (entities-of application)))))))

(test the-path-array-asks-for-the-path-by-a-click-and-then-the-numbers
  (let ((application (reshaping-app (cut-circle 1 0 0 50) (cut-line 2 0 0 1000 0))))
    (app:select-ids application '(1))
    (press application :y :shift)
    (click-widget application '(:prompt-button 2))
    (is (eq :array-path (app:tool-kind (app:app-tool application))))
    (click application 0 500)
    (is (search "Click a line" (message-text application)) "nothing there")
    (is (null (app:app-prompt application)))
    (click application 600 0)
    (let ((prompt (form-of application)))
      (is (equal '(:measure :path-items :path-spacing :align)
                 (mapcar #'app:form-field-key (app:prompt-fields prompt)))))
    (press application :tab)
    (type-text application "5")
    (press application :enter)
    (is (= 6 (dom:drawing-entity-count (app:app-drawing application))) "the circle, the line and four copies")
    (is (equal '(0d0 250d0 500d0 750d0 1000d0)
               (loop for entity in (entities-of application)
                     when (eq :circle (dom:entity-kind entity))
                       collect (dom:circle-center-x entity))))))

(test the-path-array-can-measure-a-spacing-instead
  (let ((application (reshaping-app (cut-circle 1 0 0 50) (cut-line 2 0 0 1000 0))))
    (app:select-ids application '(1))
    (press application :y :shift)
    (click-widget application '(:prompt-button 2))
    (click application 600 0)
    (is (search "The path is 1000" (ui:widget-label (find-widget application :prompt-message)))
        "the length of the path is said")
    (press application :space)
    (typed-in-form application nil nil "300")
    (press application :enter)
    (is (= 5 (dom:drawing-entity-count (app:app-drawing application))) "copies at 300, 600 and 900")))

(test escape-leaves-the-array-tools-without-changing-anything
  (let ((application (array-app (cut-line 1 1000 0 2000 0))))
    (click-widget application '(:prompt-button 1))
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-array-shortcut-and-icon-are-in-the-modify-menu
  (let ((application (reshaping-app (cut-line 1 0 0 100 0))))
    (click-widget application :modify)
    (let ((item (find-widget application '(:menu-item :array))))
      (is (not (null item)))
      (is (equal "Array..." (ui:widget-label item)))
      (is (eq :array (ui:widget-icon item))))))

;;; The console

(test the-console-lays-copies-in-a-grid-round-a-point-and-along-a-path
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(rect-size 0 0 10 10)")
    (is (equal '("(2 3 4 5 6)") (ran-ok context "(array-grid 1 2 3 30 40)")) "the ids of the copies")
    (is (= 6 (dom:drawing-entity-count drawing)))
    (is (search "at least two" (failure-of context "(array-grid 1 1 1 30 40)")))
    (ran-ok context "(line 0 0 100 0) (circle 5 5 2)")
    (is (equal '("(9 10 11)") (ran-ok context "(array-polar 8 0 0 4 :angle 270)")))
    (is (equal '("(12 13)") (ran-ok context "(array-path 8 7 :count 3 :turn nil)")))
    (is (= 13 (dom:drawing-entity-count drawing)))
    (is (search "not in the drawing" (failure-of context "(array-path 1 999 :count 3)")))))

(test the-console-help-lists-the-array-commands
  (let ((overview (script:script-result-output (run-script (script-env) "(help)"))))
    (is (search "Repeating" overview))
    (is (search "array-grid" overview))))
