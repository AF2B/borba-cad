(in-package #:borba-cad/tests)

(def-suite script
  :description "The language of the program: the commands, the evaluator, the transaction and the writer of scripts."
  :in all)

(in-suite script)

;;; Helpers

(defun script-env (&optional (drawing (dom:make-drawing)))
  "Return a script context on DRAWING whose selection is kept in the context itself."
  (let ((selection '()))
    (script:make-script-context drawing
                                :selection-reader (lambda () selection)
                                :selection-writer (lambda (ids) (setf selection ids)))))

(defun run-script (context
                   text
                   &rest options)
  "Run TEXT in CONTEXT and return the result.

CONTEXT: a script context.
TEXT: Lisp source.
OPTIONS: keyword arguments of EVALUATE-SOURCE."
  (apply #'script:evaluate-source context text options))

(defun ran-ok (context
               text)
  "Run TEXT in CONTEXT, check that it went well, and return the printed values.

CONTEXT: a script context.
TEXT: Lisp source."
  (let ((result (run-script context text)))
    (is (eq :ok (script:script-result-status result)) "~s: ~a" text (script:script-result-message result))
    (script:script-result-values result)))

(defun failure-of (context
                   text)
  "Run TEXT in CONTEXT, check that it failed as an error, and return the message.

CONTEXT: a script context.
TEXT: Lisp source."
  (let ((result (run-script context text)))
    (is (eq :error (script:script-result-status result)) "~s should fail" text)
    (script:script-result-message result)))

(defun entity-shapes (drawing)
  "Return, for every entity of DRAWING in stacking order, its kind, layer name and property values.

DRAWING: a drawing."
  (mapcar (lambda (entity)
            (list (dom:entity-kind entity)
                  (dom:layer-name (dom:find-layer drawing (dom:entity-layer-id entity)))
                  (mapcar #'dom:property-value (dom:entity-properties entity))))
          (dom:drawing-entities drawing)))

(defun stored (context
               id)
  "Return the entity with ID in the drawing of CONTEXT.

CONTEXT: a script context.
ID: an entity id."
  (dom:store-find (dom:drawing-store (script:script-context-drawing context)) id))

(defun degrees-close-p (expected-degrees
                        radians)
  "Return true when RADIANS is EXPECTED-DEGREES to within a part in a trillion.

EXPECTED-DEGREES: a number of degrees.
RADIANS: an angle in radians."
  (< (abs (- (base:degrees->radians expected-degrees) radians)) 1d-12))

(defun context-with-three-entities ()
  "Return a context on a drawing with two lines (ids 1 and 2) and a circle (id 3), with nothing yet recorded."
  (let ((context (script-env)))
    (ran-ok context "(line 0 0 100 0) (line 0 50 100 50) (circle 500 500 40)")
    (script:context-reset context)
    context))

;;; Units, angles, colors

(test lengths-are-millimetres-and-the-unit-functions-convert
  (is (= 3d0 (script:mm 3)))
  (is (= 200d0 (script:cm 20)))
  (is (= 5000d0 (script:m 5)))
  (is (= 25.4d0 (script:inch 1)))
  (is (= 609.6d0 (script:foot 2)))
  (is (= 2500d0 (script:m 5/2)))
  (signals error (script:m "5")))

(test an-angle-is-degrees-unless-it-is-written-as-radians
  (is (degrees-close-p 90 (script::angle-radians 90)))
  (is (= 1.5d0 (script::angle-radians (script:rad 1.5))))
  (is (= pi (script::angle-radians (script:rad pi))))
  (signals error (script::angle-radians "ninety")))

(test colors-are-made-from-channels-or-named
  (let ((color (script:rgb 10 20 30)))
    (is (equal '(10 20 30) (list (dom:rgb-red color) (dom:rgb-green color) (dom:rgb-blue color)))))
  (signals error (script:rgb 256 0 0))
  (signals error (script:rgb 1.5 0 0))
  (is (typep (script::color-of :red) 'dom:rgb))
  (signals error (script::color-of :chartreuse)))

;;; Making entities

(test every-command-that-draws-makes-the-entity-and-returns-its-id
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (is (equal '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")
               (ran-ok context "(line 0 0 1000 500)
                                (rect 0 0 2000 1000)
                                (rect-size 100 200 300 400)
                                (circle 500 500 250)
                                (arc 0 0 900 30 120)
                                (ellipse 0 0 800 300 15)
                                (polygon 0 0 600 6)
                                (pline '((0 0) (100 0) (100 100)) :closed t)
                                (text 10 20 \"Cozinha\" :height 200 :angle 45)
                                (dimension 0 0 3000 0 500)")))
    (is (equal '(:line :rectangle :rectangle :circle :arc :ellipse :polygon :polyline :text :dimension)
               (mapcar #'dom:entity-kind (dom:drawing-entities drawing))))
    (let ((line (stored context 1)))
      (is (equal '(0d0 0d0 1000d0 500d0)
                 (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line)))))
    (let ((rectangle (stored context 3)))
      (is (equal '(100d0 200d0 300d0 400d0)
                 (list (dom:rectangle-min-x rectangle)
                       (dom:rectangle-min-y rectangle)
                       (dom:rectangle-width rectangle)
                       (dom:rectangle-height rectangle)))))
    (let ((arc (stored context 5)))
      (is (degrees-close-p 30 (dom:arc-start-angle arc)))
      (is (degrees-close-p 120 (dom:arc-end-angle arc))))
    (let ((text (stored context 9)))
      (is (string= "Cozinha" (dom:text-string text)))
      (is (= 200d0 (dom:text-height text)))
      (is (degrees-close-p 45 (dom:text-angle text))))
    (let ((dimension (stored context 10)))
      (is (= 500d0 (dom:dimension-offset dimension)))
      (is (= (dom:drawing-default-text-height drawing) (dom:dimension-text-height dimension)))
      (is (eq (dom:drawing-display-unit drawing) (dom:dimension-unit dimension))))))

(test numbers-may-be-integers-rationals-or-floats-and-are-doubles-inside
  (let* ((context (script-env))
         (id (first (ran-ok context "(line 0 0 1/2 1.25)")))
         (line (stored context (parse-integer id))))
    (is (= 0.5d0 (dom:line-x2 line)))
    (is (= 1.25d0 (dom:line-y2 line)))))

(test an-invalid-entity-is-an-error-with-the-usual-reason-and-nothing-is-left-behind
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (is (search "line" (string-downcase (failure-of context "(line 0 0 100 100) (line 5 5 5 5) (circle 0 0 10)"))))
    (is (= 0 (dom:drawing-entity-count drawing)) "the first line was taken back too")
    (is (null (script:context-changes context)))))

(test commands-draw-on-the-current-layer-and-refuse-a-locked-one
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(layer \"walls\" :color :red) (line 0 0 10 10)")
    (is (equal '(:line "walls") (subseq (first (entity-shapes drawing)) 0 2)))
    (ran-ok context "(layer \"walls\" :locked t)")
    (is (search "locked" (failure-of context "(line 0 0 20 20)")))
    (is (= 1 (dom:drawing-entity-count drawing)))))

;;; Layers

(test layer-makes-a-layer-selects-it-and-keeps-what-it-was-given
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (is (equal '("\"walls\"") (ran-ok context "(layer \"walls\" :color (rgb 255 120 80))")))
    (let ((layer (dom:find-layer-by-name drawing "walls")))
      (is (equal '(255 120 80) (list (dom:rgb-red (dom:layer-color layer))
                                     (dom:rgb-green (dom:layer-color layer))
                                     (dom:rgb-blue (dom:layer-color layer)))))
      (is (= (dom:layer-id layer) (dom:drawing-current-layer-id drawing))))
    (ran-ok context "(layer \"0\") (layer \"walls\" :visible nil :locked t)")
    (let ((layer (dom:find-layer-by-name drawing "walls")))
      (is (not (dom:layer-visible layer)))
      (is (dom:layer-locked layer))
      (is (= 255 (dom:rgb-red (dom:layer-color layer))) "without a color given the one it has is kept"))
    (ran-ok context "(layer \"walls\" :color :blue)")
    (is (/= 255 (dom:rgb-red (dom:layer-color (dom:find-layer-by-name drawing "walls"))))
        "and one that is given changes it")
    (is (equal '("(\"0\" \"walls\")") (ran-ok context "(layers)")))))

(test with-layer-draws-on-a-layer-and-goes-back
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(with-layer (\"doors\") (line 0 0 10 10)) (line 5 5 20 20)")
    (is (equal '("doors" "0") (mapcar #'second (entity-shapes drawing))))
    (is (= dom:+default-layer-id+ (dom:drawing-current-layer-id drawing)))
    (failure-of context "(with-layer (\"doors\") (error \"stop\"))")
    (is (= dom:+default-layer-id+ (dom:drawing-current-layer-id drawing)) "even when the body fails")))

;;; Changing and finding entities

(test move-copy-and-erase-work-on-an-id-or-a-list-of-ids
  (let ((context (context-with-three-entities)))
    (is (equal '("2") (ran-ok context "(move '(1 2) 10 20)")))
    (is (equal '(10d0 20d0) (let ((line (stored context 1)))
                              (list (dom:line-x1 line) (dom:line-y1 line)))))
    (is (equal '("(4 5)") (ran-ok context "(copy '(1 2) 1000 0)")))
    (is (= 1010d0 (dom:line-x1 (stored context 4))))
    (is (equal '("1") (ran-ok context "(erase 3)")))
    (is (equal '("(1 2 4 5)") (ran-ok context "(entities)")))
    (is (equal '("4") (ran-ok context "(entity-count)")))))

(test move-and-erase-leave-entities-on-locked-layers-alone
  (let ((context (context-with-three-entities)))
    (ran-ok context "(layer \"fixed\") (move-to-layer 1 \"fixed\") (layer \"fixed\" :locked t) (layer \"0\")")
    (is (equal '("1") (ran-ok context "(move '(1 2) 5 5)")) "only the one that is not locked")
    (is (equal '("0") (ran-ok context "(erase 1)")))
    (is (search "locked" (failure-of context "(edit 1 :x1 99)")))))

(test edit-changes-properties-by-keyword-with-units-and-degrees
  (let ((context (script-env)))
    (ran-ok context "(circle 0 0 100) (arc 0 0 100 0 90) (text 0 0 \"a\")")
    (ran-ok context "(edit 1 :radius 250 :center-x 10)")
    (is (= 250d0 (dom:circle-radius (stored context 1))))
    (is (= 10d0 (dom:circle-center-x (stored context 1))))
    (ran-ok context "(edit 2 :end-angle 180)")
    (is (degrees-close-p 180 (dom:arc-end-angle (stored context 2))))
    (ran-ok context "(edit 3 :string \"b\" :height 300 :angle 90)")
    (is (string= "b" (dom:text-string (stored context 3))))
    (is (degrees-close-p 90 (dom:text-angle (stored context 3))))
    (is (search "no property" (failure-of context "(edit 1 :colour 3)")))
    (is (search "no entity" (string-downcase (failure-of context "(edit 99 :radius 1)"))))))

(test move-to-layer-and-the-queries-agree-with-the-drawing
  (let ((context (context-with-three-entities)))
    (ran-ok context "(layer \"other\") (layer \"0\")")
    (is (equal '("2") (ran-ok context "(move-to-layer '(1 3) \"other\")")))
    (is (equal '("(1 3)") (ran-ok context "(entities :layer \"other\")")))
    (is (equal '("(2)") (ran-ok context "(entities :layer \"0\")")))
    (is (equal '("(3)") (ran-ok context "(entities :kind :circle)")))
    (is (equal '("3") (ran-ok context "(entity-count)")))
    (is (equal '("(0.0 0.0 540.0 540.0)") (ran-ok context "(extents)")) "the circle reaches 540")
    (is (equal '("NIL") (ran-ok (script-env) "(extents)")) "an empty drawing has none")
    (is (search "no layer" (string-downcase (failure-of context "(entities :layer \"nowhere\")"))))))

(test info-says-what-an-entity-is-in-millimetres-and-degrees
  (let ((context (script-env)))
    (ran-ok context "(arc 0 0 1000 0 180)")
    (let ((output (script:script-result-output (run-script context "(info 1)"))))
      (is (search "arc 1 on layer \"0\"" output))
      (is (search "1000.0" output) "the radius")
      (is (search "180" output) "the end angle, in degrees as the commands take it")
      (is (search "3141.59" output) "the length"))))

;;; Selection and view

(test selection-commands-go-through-the-hooks-the-application-gives
  (let ((context (context-with-three-entities))
        (fitted nil))
    (setf (script::script-context-view-fitter context) (lambda () (setf fitted t)))
    (is (equal '("2") (ran-ok context "(select '(1 2))")))
    (is (equal '("(1 2)") (ran-ok context "(selected)")))
    (is (equal '("3") (ran-ok context "(select-all)")))
    (is (equal '("0") (ran-ok context "(deselect)")))
    (is (equal '("NIL") (ran-ok context "(selected)")))
    (is (equal '("1") (ran-ok context "(select '(1 99))")) "an id that is not there is not selected")
    (ran-ok context "(fit)")
    (is-true fitted)))

(test commands-without-hooks-still-run
  (let ((context (script:make-script-context (dom:make-drawing))))
    (is (equal '("NIL") (ran-ok context "(selected)")))
    (is (equal '("NIL") (ran-ok context "(fit)")))
    (is (equal '("0") (ran-ok context "(select '())")))))

(test the-settings-of-the-drawing-can-be-read-and-set
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (is (equal '(":METER") (ran-ok context "(display-unit :meter)")))
    (is (eq :meter (dom:drawing-display-unit drawing)))
    (is (equal '("250.0") (ran-ok context "(grid-spacing 250)")))
    (is (= 250d0 (dom:drawing-grid-spacing drawing)))
    (is (equal '(":METER") (ran-ok context "(display-unit)")))
    (failure-of context "(display-unit :parsec)")))

;;; The evaluator

(test the-evaluator-prints-what-each-form-returns-and-what-the-commands-say
  (let ((result (run-script (script-env) "(+ 1 2) (say \"hello\" 42) (values 1 2) (list 1 2 3) \"text\"")))
    (is (eq :ok (script:script-result-status result)))
    (is (= 5 (script:script-result-forms result)))
    (is (equal '("3" "NIL" "1 2" "(1 2 3)" "\"text\"") (script:script-result-values result)))
    (is (string= (format nil "hello 42~%") (script:script-result-output result)))))

(test numbers-are-read-as-double-floats-so-a-decimal-point-is-enough
  (is (equal '("DOUBLE-FLOAT" "0.1" "RATIO" "1000.0")
             (ran-ok (script-env) "(type-of 1.5) 0.1 (type-of 1/3) 1e3")))
  (is (equal '("12.0") (ran-ok (script-env) "12.0"))))

(test text-that-ends-inside-a-form-is-incomplete-and-text-that-is-not-lisp-is-an-error
  (dolist (text '("(line 0 0" "(+ 1 (* 2 3)" "\"unfinished" "(list '(1 2)"))
    (is (eq :incomplete (script:script-result-status (run-script (script-env) text))) "~s" text))
  (dolist (text '(")" "(+ 1 2))" "#<foo>"))
    (let ((result (run-script (script-env) text)))
      (is (eq :error (script:script-result-status result)) "~s" text)
      (is (plusp (length (script:script-result-message result)))))))

(test a-form-that-fails-takes-back-what-the-forms-before-it-did
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (is (search "UNDEFINED-THING"
                (string-upcase (failure-of context
                                           "(line 0 0 10 10) (circle 0 0 5) (undefined-thing) (line 1 1 2 2)"))))
    (is (= 0 (dom:drawing-entity-count drawing)))
    (is (null (script:context-changes context)))
    (is (equal '("3") (ran-ok context "(line 0 0 1 1)")) "ids are never used twice, so the next entity is not 1")))

(test an-error-is-reported-with-its-message-whatever-kind-it-is
  (dolist (text '("(car 5)" "(/ 1 0)" "(error \"my own\")" "(line \"a\" 0 0 0)" "(gethash 1)"))
    (let ((message (failure-of (script-env) text)))
      (is (plusp (length message)) "~s" text)
      (is (not (find #\Newline message)) "one line"))))

(test a-command-that-runs-too-long-is-stopped-and-undone
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing))
         (started (sbcl:monotonic-nanoseconds))
         (result (run-script context "(line 0 0 1 1) (loop)" :timeout 0.3d0)))
    (is (eq :timeout (script:script-result-status result)))
    (is (search "stopped" (script:script-result-message result)))
    (is (= 0 (dom:drawing-entity-count drawing)) "the line it drew was taken back")
    (is (< (/ (- (sbcl:monotonic-nanoseconds) started) 1d9) 5d0) "and it did not take long to stop")
    (is (eq :ok (script:script-result-status (run-script context "(line 0 0 1 1)"))) "the next command runs")))

(test a-timeout-never-leaves-a-change-half-made
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (dotimes (round 5)
      (let ((result (run-script context "(loop (line 0 0 1 1))" :timeout 0.1d0)))
        (is (member (script:script-result-status result) '(:timeout :error)) "round ~d" round))
      (is (= 0 (dom:drawing-entity-count drawing)) "round ~d" round)
      (is (null (script:context-changes context)) "round ~d" round)
      (is (zerop (script::script-context-change-count context)) "round ~d" round))))

(test a-command-may-not-make-more-than-the-limit-of-changes
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing))
         (message (let ((script:*maximum-changes* 50))
                    (failure-of context "(dotimes (i 1000) (line 0 0 (+ i 1) 1))"))))
    (is (search "at most 50 changes" message))
    (is (= 0 (dom:drawing-entity-count drawing)) "and nothing was kept")))

(test too-much-output-is-cut-while-it-is-written
  (let ((result (run-script (script-env) "(dotimes (i 100000) (say \"a line of output that repeats and repeats\"))")))
    (is (eq :ok (script:script-result-status result)))
    (is (<= (length (script:script-result-output result)) (+ script::+maximum-output-characters+ 4)))
    (is (string= (format nil "~%...") (subseq (script:script-result-output result)
                                              (- (length (script:script-result-output result)) 4))))))

(test a-printed-result-is-cut-and-survives-a-structure-that-contains-itself
  (let ((values (ran-ok (script-env) "(loop for i below 1000 collect i)")))
    (is (< (length (first values)) 1500))
    (is (search "..." (first values))))
  (is (stringp (first (ran-ok (script-env) "(let ((x (list 1 2))) (setf (cdr (last x)) x) x)")))))

(test reading-does-not-evaluate-anything
  (let ((message (failure-of (script-env) "#.(error \"read-time evaluation\")")))
    (is (search "read" (string-downcase message)))))

(test the-user-package-has-lisp-and-the-commands-and-nothing-else
  (is (equal '("\"BORBA-CAD/USER\"") (ran-ok (script-env) "(package-name *package*)")))
  (let ((user (find-package :borba-cad/user)))
    (is (eq (find-symbol "LINE" user) (find-symbol "LINE" :borba-cad/script)))
    (is (eq (find-symbol "CAR" user) (find-symbol "CAR" :cl)))
    (is (null (find-symbol "MAKE-LINE" user)) "the internals of the program are not in it")
    (is (null (find-symbol "MAKE-DRAWING" user)))))

(test standard-input-is-empty-so-a-command-cannot-wait-for-a-keyboard
  (let ((result (run-script (script-env) "(read)" :timeout 2d0)))
    (is (eq :error (script:script-result-status result)))))

;;; Comments and counting forms

(test comments-are-taken-out-but-not-from-strings-or-characters
  (is (string= (format nil "(a b) ~%(c)") (script:strip-comments (format nil "(a b) ; one~%(c)"))))
  (is (string= "\"a;b\"" (script:strip-comments "\"a;b\"")))
  (is (string= "\"a;b\" x " (script:strip-comments "\"a;b\" x ;gone")))
  (is (string= "(list #\\; 1) " (script:strip-comments "(list #\\; 1) ;gone")))
  (is (string= "\"a\\\"; still in\"" (script:strip-comments "\"a\\\"; still in\"")))
  (is (string= "" (script:strip-comments ";only a comment"))))

(test the-forms-of-a-text-are-counted-or-it-is-said-it-cannot-be-read
  (is (= 3 (script:count-source-forms "(a) (b) c")))
  (is (= 0 (script:count-source-forms "; nothing")))
  (is (null (script:count-source-forms "(unfinished")))
  (is (null (script:count-source-forms ")"))))

;;; The transaction

(test what-a-command-did-is-one-net-change-for-each-entity
  (let* ((context (script-env))
         (drawing (script:script-context-drawing context)))
    (ran-ok context "(line 0 0 1 1) (circle 0 0 5) (move 1 10 10) (move 1 5 5) (erase 2)")
    (let ((changes (script:context-changes context)))
      (is (= 1 (length changes)) "the line once, from nothing to where it ended; the circle never was")
      (is (null (dom:change-before (first changes))))
      (is (= 15d0 (dom:line-x1 (dom:change-after (first changes))))))
    (script:context-rollback context)
    (is (= 0 (dom:drawing-entity-count drawing)))
    (is (null (script:context-changes context)))
    (ran-ok context "(line 0 0 1 1)")
    (script:context-reset context)
    (ran-ok context "(move 3 3 3) (move 3 -3 -3)")
    (is (null (script:context-changes context)) "an entity moved and moved back did not change")))

(test the-net-changes-are-in-id-order-and-undoing-them-restores-the-drawing
  (let* ((context (script-env))
         (drawing (script:script-context-drawing context)))
    (ran-ok context "(dotimes (i 5) (circle (* i 100) 0 10))")
    (script:context-reset context)
    (let ((before (entity-shapes drawing)))
      (ran-ok context "(move '(5 1 3) 0 50) (erase 2) (line 0 0 1 1)")
      (let ((changes (script:context-changes context)))
        (is (equal '(1 2 3 5 6) (mapcar #'dom:change-entity-id changes)))
        (dom:apply-changes drawing (dom:invert-changes changes))
        (is (equal before (entity-shapes drawing)) "undone, the drawing is as it was")))))

(test history-record-puts-changes-that-are-already-applied-on-the-undo-stack
  (let* ((drawing (dom:make-drawing))
         (history (dom:make-history))
         (context (script-env drawing)))
    (ran-ok context "(line 0 0 1 1) (circle 5 5 5)")
    (is (dom:history-record history (script:context-changes context)))
    (is (= 2 (dom:drawing-entity-count drawing)) "recording does not apply them again")
    (is (= 1 (dom:history-undo-depth history)))
    (is (dom:history-undo history drawing))
    (is (= 0 (dom:drawing-entity-count drawing)) "one undo takes the whole command back")
    (is (dom:history-redo history drawing))
    (is (= 2 (dom:drawing-entity-count drawing)))
    (is (null (dom:history-record history '())) "nothing to record")
    (dom:history-undo history drawing)
    (script:context-reset context)
    (ran-ok context "(line 0 0 3 3)")
    (dom:history-record history (script:context-changes context))
    (is (= 0 (dom:history-redo-depth history)) "a new command discards what was undone")))

;;; Help

(test help-lists-the-commands-and-explains-one
  (let ((overview (script:script-result-output (run-script (script-env) "(help)")))
        (one (script:script-result-output (run-script (script-env) "(help 'line)")))
        (unknown (script:script-result-output (run-script (script-env) "(help 'frobnicate)"))))
    (is (search "Making entities: line rect" overview))
    (is (search "Layers: layer with-layer layers" overview))
    (is (search "line (x1 y1 x2 y2)" one))
    (is (search "Draw a line from" one))
    (is (search "There is no command" unknown))))

(test every-command-the-help-lists-exists-and-is-documented
  (dolist (topic script::*help-topics*)
    (dolist (name (rest topic))
      (is (or (fboundp name) (macro-function name)) "~s exists" name)
      (is (plusp (length (documentation name 'function))) "~s is documented" name))))

;;; A drawing as a script

(defun script-sample-drawing ()
  "Return a drawing with every kind of entity on three layers, one hidden and locked, in meters."
  (let ((drawing (export-sample-drawing)))
    (dom:set-display-unit drawing :meter)
    (dom:set-grid-spacing drawing 250d0)
    drawing))

(test a-drawing-becomes-a-script-that-makes-it-again
  (let* ((drawing (script-sample-drawing))
         (again (dom:make-drawing))
         (result (run-script (script-env again) (script:drawing-script drawing))))
    (is (eq :ok (script:script-result-status result)) "~a" (script:script-result-message result))
    (is (equal (entity-shapes drawing) (entity-shapes again))
        "the same shapes with the same numbers, bit for bit, in the same order, on layers of the same names")
    (is (equal (mapcar #'dom:layer-name (dom:layers-in-order drawing))
               (mapcar #'dom:layer-name (dom:layers-in-order again))))
    (is (eq :meter (dom:drawing-display-unit again)))
    (is (= 250d0 (dom:drawing-grid-spacing again)))))

(test the-script-of-a-drawing-keeps-the-state-of-each-layer
  (let* ((drawing (script-sample-drawing))
         (again (dom:make-drawing)))
    (ran-ok (script-env again) (script:drawing-script drawing))
    (dolist (layer (dom:layers-in-order drawing))
      (let ((other (dom:find-layer-by-name again (dom:layer-name layer))))
        (is-true other "~a" (dom:layer-name layer))
        (is (eq (dom:layer-visible layer) (dom:layer-visible other)))
        (is (eq (dom:layer-locked layer) (dom:layer-locked other)))
        (is (equalp (dom:layer-color layer) (dom:layer-color other)))))
    (is (string= (dom:layer-name (dom:find-layer drawing (dom:drawing-current-layer-id drawing)))
                 (dom:layer-name (dom:find-layer again (dom:drawing-current-layer-id again)))))))

(test random-drawings-survive-a-trip-through-a-script-exactly
  (dolist (seed '(1 2 3 4 5))
    (let ((drawing (rng:generate-drawing :seed seed
                                         :entity-count 60))
          (again (dom:make-drawing)))
      (ran-ok (script-env again) (script:drawing-script drawing))
      (is (equal (entity-shapes drawing) (entity-shapes again)) "seed ~d" seed))))

(test the-script-writes-numbers-so-they-read-back-and-angles-in-degrees-when-that-is-exact
  (is (string= "5000" (script::number-text 5000d0)))
  (is (string= "-3" (script::number-text -3d0)))
  (is (string= "0.1" (script::number-text 0.1d0)))
  (is (string= "90" (script::angle-text (/ pi 2))))
  (is (string= "45" (script::angle-text (/ pi 4))))
  (let ((radians 0.7071067811865476d0))
    (let ((form (first (script:read-source-forms (script::angle-text radians)))))
      (is (= radians (script::angle-radians (if (consp form)
                                                (script:rad (second form))
                                                form)))
          "whichever way it is written, it reads back as the same angle"))))

(test a-script-of-an-empty-drawing-is-a-valid-script
  (let ((context (script-env)))
    (is (eq :ok (script:script-result-status (run-script context (script:drawing-script (dom:make-drawing))))))
    (is (= 0 (dom:drawing-entity-count (script:script-context-drawing context))))))

(test a-script-is-the-same-in-portuguese
  (let ((drawing (script-sample-drawing))
        (again (dom:make-drawing)))
    (i18n:with-language (:pt-br)
      (is (eq :ok (script:script-result-status (run-script (script-env again) (script:drawing-script drawing))))))
    (is (equal (entity-shapes drawing) (entity-shapes again)))))
