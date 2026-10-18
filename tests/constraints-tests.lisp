(in-package #:borba-cad/tests)

(def-suite constraints
  :description "Ortho, polar, and points that are typed."
  :in all)

(in-suite constraints)

;;; Helpers

(defun point-close-p (expected-x
                      expected-y
                      x
                      y)
  "Return true when (X, Y) is the point (EXPECTED-X, EXPECTED-Y) to a millionth of a millimetre."
  (and (< (abs (- expected-x x)) 1d-6)
       (< (abs (- expected-y y)) 1d-6)))

(defun cursor-point (application)
  "Return the snapped position of the cursor of APPLICATION as an (x . y) cons."
  (let ((cursor (app:app-cursor application)))
    (cons (app:cursor-x cursor) (app:cursor-y cursor))))

(defun cursor-at-p (application
                    x
                    y)
  "Return true when the snapped cursor of APPLICATION is at (X, Y) to a millionth of a millimetre."
  (let ((point (cursor-point application)))
    (point-close-p (float x 1d0) (float y 1d0) (car point) (cdr point))))

(defun move-pointer-to (application
                        world-x
                        world-y)
  "Move the pointer to a world position."
  (send application (pointer application :move nil world-x world-y)))

(defun typed-point (application
                    text)
  "Type TEXT as a point and press Enter."
  (type-text application text)
  (press application :enter))

(defun line-tool-app ()
  "Return an application with the line tool active, the grid snap and the object snap off."
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (press application :l)
    application))

;;; The geometry

(test ortho-holds-the-point-to-the-nearer-axis-through-the-anchor
  (let ((anchor (cons 100d0 200d0)))
    (multiple-value-bind (x y) (app:constrain-point :ortho anchor 900d0 230d0)
      (is (point-close-p 900d0 200d0 x y) "wider than high: horizontal"))
    (multiple-value-bind (x y) (app:constrain-point :ortho anchor 130d0 900d0)
      (is (point-close-p 100d0 900d0 x y) "higher than wide: vertical"))
    (multiple-value-bind (x y) (app:constrain-point :ortho anchor -500d0 190d0)
      (is (point-close-p -500d0 200d0 x y) "to the left too"))
    (multiple-value-bind (x y) (app:constrain-point :ortho anchor 100d0 200d0)
      (is (point-close-p 100d0 200d0 x y) "the anchor itself stays"))))

(test polar-holds-the-point-to-a-multiple-of-fifteen-degrees
  (let ((anchor (cons 0d0 0d0)))
    (multiple-value-bind (x y) (app:constrain-point :polar anchor 100d0 5d0)
      (is (point-close-p (* 100d0 (cos (base:degrees->radians 0d0))) 0d0 (- x 0.0d0) (- y 0.0d0))
          "2.9 degrees goes to 0, and keeps its length along the ray")
      (is (< (abs (- 100d0 x)) 0.2d0)))
    (multiple-value-bind (x y) (app:constrain-point :polar anchor 100d0 40d0)
      (let ((angle (atan y x)))
        (is (< (abs (- (base:degrees->radians 15d0) angle)) 1d-9) "21.8 degrees goes to 15")
        (is (< (abs (- (+ (* 100d0 (cos (base:degrees->radians 15d0))) (* 40d0 (sin (base:degrees->radians 15d0))))
                       (sqrt (+ (* x x) (* y y)))))
               1d-9)
            "and its length is how far the point went along the ray")))
    (multiple-value-bind (x y) (app:constrain-point :polar anchor 3d0 100d0)
      (is (point-close-p 0d0 100d0 (if (< (abs x) 1d-9) 0d0 x) y) "near the vertical goes to 90")))
  (dolist (degrees '(0 15 30 45 90 135 180 -45 -90))
    (let* ((radians (base:degrees->radians (float degrees 1d0)))
           (anchor (cons 10d0 20d0)))
      (multiple-value-bind (x y) (app:constrain-point :polar
                                                      anchor
                                                      (+ 10d0 (* 500d0 (cos radians)))
                                                      (+ 20d0 (* 500d0 (sin radians))))
        (is (point-close-p (+ 10d0 (* 500d0 (cos radians))) (+ 20d0 (* 500d0 (sin radians))) x y)
            "a point already on a ray stays: ~d degrees" degrees)))))

;;; What is typed

(defparameter *typed-anchor* (cons 1000d0 1000d0)
  "The last point, for the tests of what is typed.")

(defun typed-values (text
                     &key (anchor *typed-anchor*)
                          (pointer (cons 5000d0 1000d0))
                          (unit :millimeter))
  "Return the point TEXT names as a list of x and y."
  (multiple-value-bind (x y) (app:parse-point-input text anchor pointer unit)
    (list x y)))

(test a-length-goes-from-the-anchor-toward-the-pointer
  (is (equal '(1500d0 1000d0) (typed-values "500")))
  (is (equal '(1000d0 1750d0) (typed-values "750" :pointer (cons 1000d0 9000d0))))
  (is (equal '(500d0 1000d0) (typed-values "-500")) "a negative length goes the other way")
  (let ((diagonal (typed-values "1414.2135623730951" :pointer (cons 2000d0 2000d0))))
    (is (point-close-p 2000d0 2000d0 (first diagonal) (second diagonal)))))

(test x-semicolon-y-is-absolute-and-at-x-semicolon-y-is-from-the-anchor
  (is (equal '(300d0 -400d0) (typed-values "300;-400")))
  (is (equal '(1300d0 600d0) (typed-values "@300;-400")))
  (is (equal '(300d0 -400d0) (typed-values " 300 ; -400 ")))
  (is (equal '(300d0 -400d0) (typed-values "300;-400" :anchor nil)))
  (is (equal '(300d0 -400d0) (typed-values "@300;-400" :anchor nil)) "with no anchor the origin takes its place"))

(test length-less-than-angle-is-polar-from-the-anchor
  (let ((east (typed-values "500<0"))
        (north (typed-values "500<90"))
        (diagonal (typed-values "@1000<45")))
    (is (point-close-p 1500d0 1000d0 (first east) (second east)))
    (is (point-close-p 1000d0 1500d0 (first north) (second north)))
    (is (point-close-p (+ 1000d0 (* 1000d0 (cos (/ pi 4)))) (+ 1000d0 (* 1000d0 (sin (/ pi 4))))
                       (first diagonal)
                       (second diagonal))
        "the @ may be left out or written"))
  (is (equal '(500d0 0d0) (mapcar (lambda (value) (if (< (abs value) 1d-9) 0d0 value))
                                  (typed-values "500<0" :anchor nil)))))

(test numbers-are-in-the-display-unit-unless-they-say-otherwise
  (is (equal '(4000d0 1000d0) (typed-values "3" :unit :meter)))
  (is (equal '(1000d0 3000d0) (typed-values "@0;2" :unit :meter)))
  (is (equal '(1500d0 1000d0) (typed-values "500mm" :unit :meter)))
  (is (equal '(1050d0 1000d0) (typed-values "5cm" :unit :meter)))
  (is (equal '(1500d0 1000d0) (typed-values "0,5m" :unit :millimeter)) "a decimal comma, as the fields accept"))

(test what-cannot-be-a-point-is-said
  (dolist (case '(("" "type a length")
                  ("@" "type a length")
                  ("abc" "")
                  ("12;" "")
                  (";12" "")
                  ("5<" "")
                  ("5;5;5" "")))
    (destructuring-bind (text reason) case
      (handler-case (progn (typed-values text) (fail "~s should not be a point" text))
        (base:invalid-input (condition)
          (is (search reason (base:invalid-input-reason condition)) "~s" text)))))
  (handler-case (typed-values "500" :anchor nil)
    (base:invalid-input (condition)
      (is (search "first point as x;y" (base:invalid-input-reason condition)))))
  (handler-case (typed-values "500" :pointer (cons 1000d0 1000d0))
    (base:invalid-input (condition)
      (is (search "which way" (base:invalid-input-reason condition)))))
  (handler-case (typed-values "500" :pointer nil)
    (base:invalid-input (condition)
      (is (search "which way" (base:invalid-input-reason condition))))))

;;; Ortho and polar in the application

(test f8-and-f10-turn-ortho-and-polar-on-one-at-a-time
  (let ((application (make-test-app)))
    (is (null (app:app-constraint application)))
    (press application :f8)
    (is (eq :ortho (app:app-constraint application)))
    (is (ui:widget-active (find-widget application :ortho)))
    (press application :f10)
    (is (eq :polar (app:app-constraint application)) "polar replaces ortho")
    (is (ui:widget-active (find-widget application :polar)))
    (is (not (ui:widget-active (find-widget application :ortho))))
    (press application :f10)
    (is (null (app:app-constraint application)))
    (click-widget application :ortho)
    (is (eq :ortho (app:app-constraint application)) "the chips do the same")
    (click-widget application :ortho)
    (is (null (app:app-constraint application)))))

(test ortho-holds-the-next-point-of-a-line-to-an-axis
  (let ((application (line-tool-app)))
    (press application :f8)
    (click application 0 0)
    (move-pointer-to application 2000 300)
    (is (cursor-at-p application 2000d0 0d0))
    (is (eq :ortho (app:cursor-snap-kind (app:app-cursor application))))
    (is (search "ortho" (nth-value 2 (app:app-status-texts application nil))) "the status bar says what holds it")
    (move-pointer-to application 200 -2000)
    (is (cursor-at-p application 0d0 -2000d0))
    (click application 2000 300)
    (let ((line (last-entity application)))
      (is (= 0d0 (dom:line-y1 line)))
      (is (= 0d0 (dom:line-y2 line)) "the line is horizontal"))))

(test the-first-point-is-not-held-and-neither-is-a-point-the-tool-cannot-measure-from
  (let ((application (line-tool-app)))
    (press application :f8)
    (move-pointer-to application 1234 567)
    (is (cursor-at-p application 1234d0 567d0) "no anchor yet"))
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (press application :f8)
    (move-pointer-to application 1234 567)
    (is (cursor-at-p application 1234d0 567d0) "the select tool has none")))

(test an-object-snap-wins-over-ortho
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) t)
    (app:commit-changes application
                        (dom:create-entities-changes
                         (list (dom:make-line :id 1 :layer-id 0 :x1 1500d0 :y1 700d0 :x2 1500d0 :y2 900d0)))
                        "entity.create")
    (press application :l)
    (press application :f8)
    (click application 0 0)
    (move-pointer-to application 1500 700)
    (is (cursor-at-p application 1500d0 700d0) "the endpoint, not the axis")
    (is (eq :endpoint (app:cursor-snap-kind (app:app-cursor application))))))

(test polar-holds-the-next-point-to-multiples-of-fifteen-degrees
  (let ((application (line-tool-app)))
    (press application :f10)
    (click application 0 0)
    (move-pointer-to application 1000 100)
    (let ((point (cursor-point application)))
      (is (< (abs (- (base:degrees->radians 0d0) (atan (cdr point) (car point)))) 1d-9) "5.7 degrees goes to 0"))
    (move-pointer-to application 1000 1000)
    (let ((point (cursor-point application)))
      (is (< (abs (- (base:degrees->radians 45d0) (atan (cdr point) (car point)))) 1d-9)))
    (is (eq :polar (app:cursor-snap-kind (app:app-cursor application))))))

(test ortho-follows-the-last-vertex-of-a-polyline-and-the-first-point-of-a-dimension
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (press application :p)
    (press application :f8)
    (click application 0 0)
    (click application 2000 100)
    (move-pointer-to application 2100 1500)
    (is (cursor-at-p application 2000d0 1500d0) "from the last vertex, not the first")
    (press application :escape)
    (press application :d)
    (click application 0 0)
    (move-pointer-to application 800 90)
    (is (cursor-at-p application 800d0 0d0))))

;;; Typing a point

(test the-keyboard-is-open-to-typed-points-only-while-a-drawing-tool-is-active
  (let ((application (make-test-app)))
    (is (not (app:app-wants-text-input-p application)) "select")
    (press application :l)
    (is (app:app-wants-text-input-p application) "a drawing tool")
    (press application :escape)
    (is (not (app:app-wants-text-input-p application)))
    (type-text application "500")
    (is (null (app:app-coordinate-edit application)) "typing with the select tool begins nothing")))

(test a-typed-first-point-and-a-typed-relative-point-make-a-line
  (let ((application (line-tool-app)))
    (typed-point application "1000;500")
    (is (null (app:app-coordinate-edit application)) "the box closes when the point is taken")
    (is (app:tool-in-progress-p (app:app-tool application)))
    (typed-point application "@1000;0")
    (is (= 1 (length (entities-of application))))
    (let ((line (last-entity application)))
      (is (equal '(1000d0 500d0 2000d0 500d0)
                 (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line)))))))

(test a-typed-length-goes-toward-the-pointer
  (let ((application (line-tool-app)))
    (click application 0 0)
    (move-pointer-to application 800 5)
    (typed-point application "1500")
    (let* ((line (last-entity application))
           (hypotenuse (sqrt (+ (* 800d0 800d0) 25d0))))
      (is (< (abs (- (/ (* 1500d0 800d0) hypotenuse) (dom:line-x2 line))) 1d-6))
      (is (< (abs (- (/ (* 1500d0 5d0) hypotenuse) (dom:line-y2 line))) 1d-6)
          "along the direction of the pointer, 5 in 800")
      (is (< (abs (- 1500d0 (geo:distance 0d0 0d0 (dom:line-x2 line) (dom:line-y2 line)))) 1d-6)
          "and the length that was typed"))))

(test a-typed-length-follows-ortho-and-a-typed-polar-point-ignores-the-pointer
  (let ((application (line-tool-app)))
    (press application :f8)
    (click application 0 0)
    (move-pointer-to application 900 400)
    (typed-point application "700")
    (let ((line (last-entity application)))
      (is (= 700d0 (dom:line-x2 line)))
      (is (= 0d0 (dom:line-y2 line)) "the pointer was held to the axis, so was the length")))
  (let ((application (line-tool-app)))
    (click application 0 0)
    (move-pointer-to application 5 5)
    (typed-point application "1000<90")
    (let ((line (last-entity application)))
      (is (point-close-p 0d0 1000d0 (dom:line-x2 line) (dom:line-y2 line))))))

(test the-box-shows-what-is-typed-and-how-to-finish-it
  (let ((application (line-tool-app)))
    (is (null (find-widget application :coordinate-input)))
    (type-text application "12")
    (is (string= "12" (ui:widget-label (find-widget application :coordinate-input))))
    (is (ui:widget-edit (find-widget application :coordinate-input)))
    (is (search "Enter accepts" (ui:widget-label (find-widget application :coordinate-hint))))
    (type-text application "3")
    (is (string= "123" (ui:widget-label (find-widget application :coordinate-input))))
    (press application :backspace)
    (is (string= "12" (ui:widget-label (find-widget application :coordinate-input))) "backspace edits the text")
    (press application :escape)
    (is (null (app:app-coordinate-edit application)) "the first Escape throws away what was typed")
    (is (null (find-widget application :coordinate-input)))
    (is (eq :line (app:tool-kind (app:app-tool application))) "and keeps the tool")))

(test a-text-that-is-not-a-point-is-said-and-kept-for-correcting
  (let ((application (line-tool-app)))
    (type-text application "5;")
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))))
    (is-true (app:app-coordinate-edit application) "it stays")
    (type-text application "5")
    (press application :enter)
    (is (null (app:app-coordinate-edit application)))
    (is (app:tool-in-progress-p (app:app-tool application)) "5;5 is a point")))

(test a-click-throws-away-what-was-being-typed
  (let ((application (line-tool-app)))
    (type-text application "12")
    (click application 300 300)
    (is (null (app:app-coordinate-edit application)))
    (is (app:tool-in-progress-p (app:app-tool application)) "and the click was the first point")))

(test letters-do-not-begin-a-typed-point-so-the-tool-keys-still-work
  (let ((application (line-tool-app)))
    (type-text application "r")
    (is (null (app:app-coordinate-edit application)))
    (press application :r)
    (is (eq :rectangle (app:tool-kind (app:app-tool application))))))

(test typed-points-work-for-every-tool-that-waits-for-one
  (let ((application (make-test-app)))
    (press application :r)
    (typed-point application "0;0")
    (typed-point application "@3000;2000")
    (is (equal '(:rectangle 3000d0 2000d0)
               (list (dom:entity-kind (last-entity application))
                     (dom:rectangle-width (last-entity application))
                     (dom:rectangle-height (last-entity application))))))
  (let ((application (make-test-app)))
    (press application :c)
    (typed-point application "1000;1000")
    (move-pointer-to application 2000 1000)
    (typed-point application "750")
    (is (= 750d0 (dom:circle-radius (last-entity application)))))
  (let ((application (make-test-app)))
    (press application :p)
    (typed-point application "0;0")
    (typed-point application "@1000;0")
    (typed-point application "@0;1000")
    (press application :enter)
    (is (eq :polyline (dom:entity-kind (last-entity application))))
    (is (= 3 (floor (length (dom:polyline-coordinates (last-entity application))) 2))))
  (let ((application (make-test-app)))
    (press application :d)
    (typed-point application "0;0")
    (typed-point application "@4000;0")
    (typed-point application "2000;600")
    (is (eq :dimension (dom:entity-kind (last-entity application))))
    (is (= 4000d0 (dom:dimension-measured-length (last-entity application))))))

(test the-numbers-typed-are-in-the-display-unit
  (let ((application (line-tool-app)))
    (dom:set-display-unit (app:app-drawing application) :meter)
    (typed-point application "0;0")
    (typed-point application "@2;0")
    (is (= 2000d0 (dom:line-x2 (last-entity application))) "two meters")))

(test the-typed-point-boxes-and-keys-speak-portuguese-and-russian
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (line-tool-app)))
        (type-text application "12")
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "~a" language)
        (is (string/= "Hold the next point to a horizontal or vertical line"
                      (ui:widget-hint (find-widget application :ortho)))
            "~a" language)))))
