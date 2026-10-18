(in-package #:borba-cad/tests)

(def-suite menus
  :description "The drop-down menus: the list of drawing tools that opens from the palette."
  :in all)

(in-suite menus)

(defparameter *draw-menu-order*
  '(:line :polyline :rectangle :polygon :circle :arc :ellipse :text :dimension :radius-dimension :diameter-dimension
    :angular-dimension :hatch :solid-fill)
  "The tools of the draw menu, top to bottom.")

(defun menu-rows (application)
  "Return the rows of the open menu, top to bottom."
  (sort (remove-if-not (lambda (widget)
                         (and (consp (ui:widget-id widget)) (eq :menu-item (first (ui:widget-id widget)))))
                       (app:build-widgets application))
        #'<
        :key #'ui:widget-y))

(defun highlighted-row (application)
  "Return the id of the tool of the row the keyboard is on, or NIL."
  (let ((row (find-if #'ui:widget-active (menu-rows application))))
    (and row (second (ui:widget-id row)))))

;;; Opening and closing

(test the-draw-button-opens-a-list-with-every-drawing-tool
  (let ((application (make-test-app)))
    (is (null (menu-rows application)) "nothing is open to begin with")
    (click-widget application :draw)
    (let ((rows (menu-rows application)))
      (is (equal *draw-menu-order* (mapcar (lambda (row) (second (ui:widget-id row))) rows)))
      (dolist (row rows)
        (is (ui:widget-icon row))
        (is (ui:widget-label row)))
      (is (equal '("L" "P" "R" "N" "C" "A" "E" "T" "D" nil nil nil nil nil) (mapcar #'ui:widget-shortcut rows))
          "the letters, and none for the round dimensions and the hatches"))
    (is-true (find-widget application :menu))))

(test the-list-hangs-from-the-draw-button-and-stays-inside-the-window
  (dolist (size '((1440d0 . 900d0) (1000d0 . 600d0)))
    (let ((application (make-test-app :width (car size)
                                      :height (cdr size))))
      (click-widget application :draw)
      (let ((card (find-widget application :menu))
            (button (find-widget application :draw)))
        (is (>= (ui:widget-x card) (+ (ui:widget-x button) (ui:widget-width button))) "to the right of the button")
        (is (>= (ui:widget-y card) 0))
        (is (<= (+ (ui:widget-x card) (ui:widget-width card)) (car size)))
        (is (<= (+ (ui:widget-y card) (ui:widget-height card)) (- (cdr size) ui:+status-height+)))
        (dolist (row (menu-rows application))
          (is (>= (ui:widget-x row) (ui:widget-x card)))
          (is (<= (+ (ui:widget-x row) (ui:widget-width row)) (+ (ui:widget-x card) (ui:widget-width card)))))))))

(test choosing-a-row-activates-that-tool-and-closes-the-list
  (let ((application (make-test-app)))
    (dolist (kind *draw-menu-order*)
      (choose-tool application kind)
      (is (eq kind (app:tool-kind (app:app-tool application))))
      (is (null (menu-rows application)) "the list closed"))))

(test the-button-closes-the-list-it-opened
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (is (menu-rows application))
    (click-widget application :draw)
    (is (null (menu-rows application)))
    (is (eq :select (app:tool-kind (app:app-tool application))) "nothing was chosen")))

(test a-click-anywhere-else-closes-the-list-and-does-nothing-more
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (click application 500 300)
    (is (null (menu-rows application)))
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (click-widget application :draw)
    (click-widget application :save-as)
    (is (null (menu-rows application)))
    (is (null (app:app-dialog application)) "the click that closed it did not reach the button under it")))

(test escape-closes-the-list-without-changing-the-tool
  (let ((application (make-test-app)))
    (press application :l)
    (click-widget application :draw)
    (press application :escape)
    (is (null (menu-rows application)))
    (is (eq :line (app:tool-kind (app:app-tool application))) "Escape closed the list, not the tool")))

(test opening-a-dialog-or-a-prompt-closes-the-list
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (press application :o :control)
    (is-true (app:app-dialog application))
    (is (null (app:app-menu application)))
    (app:close-file-dialog application)
    (is (null (menu-rows application)))))

;;; The keyboard

(test the-list-opens-with-the-keyboard-on-the-tool-in-use
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (is (eq :line (highlighted-row application)) "the first, while selecting")
    (press application :escape)
    (press application :c)
    (click-widget application :draw)
    (is (eq :circle (highlighted-row application)))))

(test arrows-move-the-highlight-and-wrap-around-and-enter-chooses
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (press application :down)
    (is (eq :polyline (highlighted-row application)))
    (press application :up)
    (press application :up)
    (is (eq :solid-fill (highlighted-row application)) "up from the first goes to the last")
    (press application :down)
    (is (eq :line (highlighted-row application)) "down from the last goes to the first")
    (press application :end)
    (is (eq :solid-fill (highlighted-row application)))
    (press application :home)
    (is (eq :line (highlighted-row application)))
    (press application :down)
    (press application :down)
    (press application :enter)
    (is (eq :rectangle (app:tool-kind (app:app-tool application))))
    (is (null (menu-rows application)))))

(test the-pointer-moves-the-highlight
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (let* ((row (find (list :menu-item :text) (menu-rows application) :key #'ui:widget-id :test #'equal))
           (x (float (+ (ui:widget-x row) 20) 1d0))
           (y (float (+ (ui:widget-y row) 10) 1d0)))
      (send application (app:make-pointer-event :move nil x y))
      (is (eq :text (highlighted-row application))))))

(test a-tool-letter-chosen-in-the-list-activates-that-tool
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (press application :r)
    (is (eq :rectangle (app:tool-kind (app:app-tool application))))
    (is (null (menu-rows application)))))

(test other-keys-do-nothing-while-the-list-is-open
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (press application :g)
    (is (menu-rows application) "G did not close the list")
    (is (dom:drawing-grid-visible (app:app-drawing application)) "and did not toggle the grid either")))

(test the-wheel-does-not-zoom-while-the-list-is-open
  (let* ((application (make-test-app))
         (scale (view:viewport-scale (app:app-viewport application))))
    (click-widget application :draw)
    (send application (app:make-wheel-event 3d0 700d0 400d0))
    (is (= scale (view:viewport-scale (app:app-viewport application))))
    (is (menu-rows application))))

;;; Drawing after choosing

(test a-tool-chosen-from-the-list-draws-like-one-chosen-by-its-letter
  (let ((application (make-test-app)))
    (choose-tool application :rectangle)
    (click application 0 0)
    (click application 2000 1000)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :rectangle (dom:entity-kind (last-entity application))))))
