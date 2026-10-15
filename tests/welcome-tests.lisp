(in-package #:borba-cad/tests)

(def-suite welcome
  :description "The start screen: recent drawings, a new drawing, a drawing to open, a script to run, and the way out."
  :in all)

(in-suite welcome)

;;; Helpers

(defun welcome-app (directory
                    &key (recents 0)
                         (width 1440d0)
                         (height 900d0))
  "Return an application whose recent drawings are RECENTS files made in DIRECTORY, with the start screen open.

DIRECTORY: a folder made for the test.
RECENTS: how many recent drawings to make.
WIDTH: window width.
HEIGHT: window height."
  (let* ((config (quiet-config))
         (state (merge-pathnames "state/" directory)))
    (setf (base:config-state-directory config) state)
    (ensure-directories-exist state)
    (dotimes (index recents)
      (let ((path (merge-pathnames (format nil "plans/drawing-~d.bcad" (1+ index)) directory)))
        (ensure-directories-exist path)
        (persist:save-drawing (rng:generate-drawing :seed index :entity-count 5) path)
        (persist:remember-recent-file state path)))
    (let ((application (app:make-app config :width width :height height)))
      (set-test-view application)
      (app:open-welcome application)
      application)))

(defun welcome-rows (application)
  "Return the rows of the start screen, top to bottom."
  (sort (remove-if-not (lambda (widget)
                         (and (consp (ui:widget-id widget)) (eq :welcome-item (first (ui:widget-id widget)))))
                       (app:build-widgets application))
        #'<
        :key #'ui:widget-y))

(defun welcome-row-ids (application)
  "Return the ids of the choices of the start screen, top to bottom."
  (mapcar (lambda (row) (second (ui:widget-id row))) (welcome-rows application)))

;;; What it shows

(test the-start-screen-offers-the-recent-drawings-and-the-four-other-choices
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 3)))
      (is-true (app:app-welcome application))
      (is (equal '((:recent 0) (:recent 1) (:recent 2) :new :open :script :quit) (welcome-row-ids application)))
      (let ((rows (welcome-rows application)))
        (is (equal '("drawing-3.bcad" "drawing-2.bcad" "drawing-1.bcad" "New drawing" "Open a drawing..."
                     "Run a script..." "Quit")
                   (mapcar #'ui:widget-label rows))
            "the latest first")
        (is (equal '("plans" "plans" "plans" "Ctrl+N" "Ctrl+O" nil "Ctrl+Q") (mapcar #'ui:widget-shortcut rows))
            "a recent drawing says its folder, the others their keys")
        (is (every #'ui:widget-icon rows))))))

(test with-nothing-recent-the-screen-says-so-and-still-has-its-four-choices
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory)))
      (is (equal '(:new :open :script :quit) (welcome-row-ids application)))
      (is-true (find-widget application :welcome-no-recent)))))

(test only-the-recent-drawings-that-still-exist-are-listed-and-at-most-five
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 8)))
      (is (= 5 (length (app:welcome-recents (app:app-welcome application)))))
      (app:close-welcome application)
      (delete-file (merge-pathnames "plans/drawing-8.bcad" directory))
      (app:open-welcome application)
      (is (equal "drawing-7.bcad" (file-namestring (first (app:welcome-recents (app:app-welcome application)))))
          "the one that was deleted is gone"))))

(test a-low-window-lists-fewer-recent-drawings-and-the-card-stays-inside
  (dolist (height '(900d0 640d0 520d0))
    (with-temporary-directory (directory)
      (let* ((application (welcome-app directory
                                       :recents 5
                                       :width 1000d0
                                       :height height))
             (card (find-widget application :welcome)))
        (is (<= (length (app:welcome-recents (app:app-welcome application))) 5))
        (is (>= (ui:widget-y card) 0) "~a" height)
        (is (<= (+ (ui:widget-y card) (ui:widget-height card)) height) "~a" height)
        (dolist (row (welcome-rows application))
          (is (<= (+ (ui:widget-y row) (ui:widget-height row)) (+ (ui:widget-y card) (ui:widget-height card)))))))))

(test the-card-is-in-the-middle-of-the-window
  (with-temporary-directory (directory)
    (let* ((application (welcome-app directory :recents 2))
           (card (find-widget application :welcome)))
      (is (< (abs (- (+ (ui:widget-x card) (/ (ui:widget-width card) 2)) 720)) 1))
      (is-true (find-widget application :welcome-logo))
      (is (string= "Borba CAD" (ui:widget-label (find-widget application :welcome-title)))))))

;;; Modal

(test while-the-start-screen-is-open-the-rest-of-the-window-ignores-the-pointer-and-the-keys
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory)))
      (click application -2500 1800)
      (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
      (is-true (app:app-welcome application) "a click outside the card does not close it")
      (press application :l)
      (is (eq :select (app:tool-kind (app:app-tool application))) "a tool letter is not a tool")
      (press application :f2)
      (is (not (app:console-open-p application)))
      (let ((scale (view:viewport-scale (app:app-viewport application))))
        (send application (app:make-wheel-event 3d0 700d0 400d0))
        (is (= scale (view:viewport-scale (app:app-viewport application)))))
      (click-widget application :new)
      (is-true (app:app-welcome application) "the button of the title bar under the veil does not answer")
      (press application :escape)
      (is (null (app:app-welcome application))))))

;;; Choosing

(test new-drawing-and-escape-both-go-on-with-the-empty-drawing
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 1)))
      (click-widget application (list :welcome-item :new))
      (is (null (app:app-welcome application)))
      (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
      (is (null (app:app-file-path application))))
    (let ((application (welcome-app directory :recents 1)))
      (press application :escape)
      (is (null (app:app-welcome application)))
      (is (not (app:app-dirty-p application))))))

(test a-recent-drawing-opens-and-the-screen-goes
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 2)))
      (click-widget application (list :welcome-item (list :recent 1)))
      (is (null (app:app-welcome application)))
      (is (equal "drawing-1.bcad" (file-namestring (app:app-file-path application))))
      (is (= 5 (dom:drawing-entity-count (app:app-drawing application)))))))

(test a-recent-drawing-that-cannot-be-opened-says-why-and-the-screen-stays
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 2)))
      (touch-text (merge-pathnames "plans/drawing-2.bcad" directory) "this is not a drawing")
      (click-widget application (list :welcome-item (list :recent 0)))
      (is-true (app:app-welcome application) "so that another choice can be made")
      (is (eq :error (cdr (app:app-message application)))))))

(test open-opens-the-file-dialog-and-run-a-script-the-one-for-scripts
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory)))
      (click-widget application (list :welcome-item :open))
      (is (null (app:app-welcome application)))
      (is-true (app:app-dialog application))
      (is (string= "Open a drawing" (dialog-title-text application))))
    (let ((application (welcome-app directory)))
      (click-widget application (list :welcome-item :script))
      (is (null (app:app-welcome application)))
      (is (string= "Run a script" (dialog-title-text application))))))

(test quit-ends-the-program
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory)))
      (is (not (app:app-quit-requested application)))
      (click-widget application (list :welcome-item :quit))
      (is-true (app:app-quit-requested application)))))

;;; The keyboard and the pointer

(test the-arrows-walk-through-the-choices-and-enter-chooses
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 2)))
      (flet ((highlighted ()
               (let ((row (find-if #'ui:widget-active (welcome-rows application))))
                 (and row (second (ui:widget-id row))))))
        (is (equal '(:recent 0) (highlighted)) "it starts on the latest drawing")
        (press application :down)
        (is (equal '(:recent 1) (highlighted)))
        (press application :up)
        (press application :up)
        (is (eq :quit (highlighted)) "up from the first wraps to the last")
        (press application :down)
        (is (equal '(:recent 0) (highlighted)))
        (press application :end)
        (is (eq :quit (highlighted)))
        (press application :home)
        (is (equal '(:recent 0) (highlighted)))
        (dotimes (index 2)
          (press application :down))
        (is (eq :new (highlighted)))
        (press application :down)
        (press application :enter)
        (is (null (app:app-welcome application)))
        (is-true (app:app-dialog application) "Enter on Open")))))

(test with-nothing-recent-the-keyboard-starts-on-new-drawing
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory)))
      (is (eq :new (second (ui:widget-id (find-if #'ui:widget-active (welcome-rows application)))))))))

(test the-pointer-moves-the-highlight
  (with-temporary-directory (directory)
    (let* ((application (welcome-app directory :recents 1))
           (row (find (list :welcome-item :open) (welcome-rows application) :key #'ui:widget-id :test #'equal)))
      (send application (app:make-pointer-event :move nil
                                                (float (+ (ui:widget-x row) 30) 1d0)
                                                (float (+ (ui:widget-y row) 10) 1d0)))
      (is (eq :open (second (ui:widget-id (find-if #'ui:widget-active (welcome-rows application)))))))))

;;; Other ways to the same place

(test opening-a-drawing-by-any-other-way-closes-the-screen
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 1)))
      (app:load-drawing-into-app application
                                 (uiop:native-namestring (merge-pathnames "plans/drawing-1.bcad" directory)))
      (is (null (app:app-welcome application))))
    (let ((application (welcome-app directory)))
      (app:operation-new application)
      (is (null (app:app-welcome application))))))

(test the-screen-is-not-open-unless-the-program-opens-it
  (let ((application (make-test-app)))
    (is (null (app:app-welcome application)) "tests, scripts and benchmarks start without it")))

;;; Portuguese

(test the-start-screen-speaks-portuguese
  (with-temporary-directory (directory)
    (let ((application (welcome-app directory :recents 1)))
      (i18n:with-language (:pt-br)
        (let ((labels (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))))
          (is (null (untranslated-texts labels)) "no English text reached the screen")
          (is (find "Desenhos recentes" labels :test #'equal))
          (is (find "Sair" labels :test #'equal))
          (is (find "O que voc"  labels :test (lambda (prefix label) (search prefix label)))))))))
