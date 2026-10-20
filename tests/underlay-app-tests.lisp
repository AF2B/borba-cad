(in-package #:borba-cad/tests)

(def-suite underlay-tools
  :description "Underlays in the application: adding a picture or a PDF, calibrating it by two points, its settings,
showing, hiding and removing it, and keeping it with the drawing."
  :in all)

(in-suite underlay-tools)

;;; Helpers

(defun picture-path (name)
  "Return the path of the picture fixture called NAME, as the text a user would type."
  (namestring (fixture-path (format nil "images/~a" name))))

(defun write-picture-file (directory
                           name
                           bytes)
  "Write BYTES to a file called NAME in DIRECTORY and return its path as text."
  (let ((path (merge-pathnames name directory)))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence bytes out))
    (namestring path)))

(defun two-page-pdf ()
  "Return a PDF of two pages of different sizes that draw a black square each."
  (pdf-with-objects
   (list 1 "/Type /Catalog /Pages 2 0 R")
   (list 2 "/Type /Pages /Kids [3 0 R 4 0 R] /Count 2")
   (list 3 "/Type /Page /Parent 2 0 R /MediaBox [0 0 200 100] /Contents 5 0 R")
   (list 4 "/Type /Page /Parent 2 0 R /MediaBox [0 0 100 300] /Contents 5 0 R")
   (list 5 "" (latin-1 "0 g 10 10 50 50 re f"))))

(defun retyped-in-form (application
                        &rest texts)
  "Fill the lines of the open form with TEXTS, a line each from the first, taking out what each line held; a text of
NIL leaves its line alone.

APPLICATION: the application.
TEXTS: what to type in each line."
  (let ((prompt (app:app-prompt application)))
    (loop for text in texts
          for index from 0
          do (loop until (= index (app:prompt-active prompt))
                   do (press application :tab))
             (when text
               (dotimes (each (length (app::form-field-value (nth index (app:prompt-fields prompt)))))
                 (press application :backspace))
               (type-text application text)))))

(defun an-application ()
  "Return an application with an empty drawing and the view of the tests."
  (make-test-app))

(defun the-underlay (application)
  "Return the first underlay of the drawing of APPLICATION."
  (first (dom:drawing-underlays (app:app-drawing application))))

(defun add-picture-underlay (application
                             &optional (name "png-rgb8.png"))
  "Add the picture fixture called NAME as an underlay and return the underlay."
  (is (app:add-underlay-from-app application (picture-path name)))
  (the-underlay application))

(defun measured-distance (underlay
                          first
                          second)
  "Return how far apart two points of the picture, each (column row), stand in the drawing."
  (multiple-value-bind (x1 y1)
      (dom:underlay-world-point underlay (first first) (second first))
    (multiple-value-bind (x2 y2)
        (dom:underlay-world-point underlay (first second) (second second))
      (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))))

;;; Adding one

(test adding-a-picture-puts-it-under-the-drawing-in-the-middle-of-the-view
  (let* ((application (an-application))
         (underlay (add-picture-underlay application))
         (bounds (dom:underlay-bounds underlay))
         (view (view:viewport-visible-bounds (app:app-viewport application))))
    (is (= 1 (dom:underlay-count (app:app-drawing application))))
    (is (string= "png-rgb8.png" (dom:underlay-name underlay)))
    (is (eq :png (dom:underlay-kind underlay)))
    (is (= 13 (dom:underlay-pixel-width underlay)))
    (is (= 11 (dom:underlay-pixel-height underlay)))
    (is (null (dom:underlay-dpi underlay)))
    (is (< (geo:bounds-width bounds) (geo:bounds-width view)) "it fits in the view")
    (is (< (geo:bounds-height bounds) (geo:bounds-height view)))
    (is (> (geo:bounds-width bounds) (* 0.3 (geo:bounds-width view))) "and fills much of it")
    (is (approx= (geo:bounds-center-x view) (geo:bounds-center-x bounds) 1d-6))
    (is (approx= (geo:bounds-center-y view) (geo:bounds-center-y bounds) 1d-6))
    (is (dom:underlay-visible underlay))
    (is (> 1 (dom:underlay-opacity underlay)) "it lets the drawing show through")))

(test adding-a-picture-waits-for-the-two-points-that-calibrate-it
  (let ((application (an-application)))
    (add-picture-underlay application)
    (is (eq :calibrate (app:tool-kind (app:app-tool application))))
    (is (search "Added png-rgb8.png" (message-text application)))
    (is (search "Calibrate" (app:tool-prompt (app:app-tool application) application)))))

(test the-picture-is-read-once-and-the-frame-draws-it
  (let* ((application (an-application))
         (underlay (add-picture-underlay application))
         (entry (app:underlay-picture application underlay)))
    (is (app:picture-entry-raster entry))
    (is (eq entry (app:underlay-picture application underlay)) "the same pixels are used again")
    (app:app-render application)
    (let ((runs (scene:draw-list-picture-runs (app:app-draw-list application))))
      (is (= 1 (length runs)))
      (is (= (app:picture-entry-key entry) (scene:picture-run-key (aref runs 0))))
      (is (eq (app:picture-entry-raster entry) (scene:picture-run-raster (aref runs 0)))))))

(test a-picture-that-cannot-be-read-is-refused-and-the-drawing-is-untouched
  (with-temporary-directory (directory)
    (let ((application (an-application)))
      (is (not (app:add-underlay-from-app application
                                          (write-picture-file directory "broken.png" (octets-of 137 80 78 71 1 2)))))
      (is (zerop (dom:underlay-count (app:app-drawing application))))
      (is (eq :error (cdr (app:app-message application))))
      (is (not (app:add-underlay-from-app application
                                          (write-picture-file directory "notes.png" (latin-1 "hello"))))
          "a file that is not a picture, whatever it is called")
      (is (search "PNG, JPEG or PDF" (message-text application)))
      (is (not (app:add-underlay-from-app application (picture-path "does-not-exist.png"))))
      (is (zerop (dom:underlay-count (app:app-drawing application)))))))

(test a-pdf-asks-for-its-page-and-resolution-and-keeps-its-size-on-paper
  (with-temporary-directory (directory)
    (let ((application (an-application))
          (path (write-picture-file directory "plan.pdf" (two-page-pdf))))
      (is (app:add-underlay-from-app application path))
      (let ((prompt (form-of application)))
        (is (equal '(:page :resolution) (mapcar #'app:form-field-key (app:prompt-fields prompt))))
        (is (search "Pages in plan.pdf: 2" (app:prompt-message prompt))))
      (retyped-in-form application "2" "100")
      (press application :enter)
      (is (null (app:app-prompt application)))
      (let ((underlay (the-underlay application)))
        (is (eq :pdf (dom:underlay-kind underlay)))
        (is (= 2 (dom:underlay-page underlay)))
        (is (= 100d0 (dom:underlay-dpi underlay)))
        (is (= 139 (dom:underlay-pixel-width underlay)) "100 points at 100 dots per inch")
        (is (= 417 (dom:underlay-pixel-height underlay)))
        (is (approx= (/ 25.4d0 100) (dom:underlay-scale underlay) 1d-9) "a pixel is a hundredth of an inch")
        (is (approx= (* 100 (/ 25.4d0 72)) (dom:underlay-width underlay) 0.2d0) "as wide as the page on paper")))))

(test the-form-of-a-pdf-refuses-a-page-or-a-resolution-that-does-not-exist
  (with-temporary-directory (directory)
    (let ((application (an-application)))
      (app:add-underlay-from-app application (write-picture-file directory "plan.pdf" (two-page-pdf)))
      (retyped-in-form application "3")
      (press application :enter)
      (is (app:app-prompt application) "the form stays")
      (is (search "from 1 to 2" (app:prompt-problem (app:app-prompt application))))
      (retyped-in-form application "1" "9000")
      (press application :enter)
      (is (search "dots per inch" (app:prompt-problem (app:app-prompt application))))
      (retyped-in-form application "1" "auto")
      (press application :enter)
      (is (null (app:app-prompt application)))
      (is (= 1 (dom:underlay-count (app:app-drawing application))))
      (is (= 150 (dom:underlay-dpi (the-underlay application))) "a page of lines is read at 150 dots per inch"))))

(test cancelling-the-question-about-a-pdf-adds-nothing
  (with-temporary-directory (directory)
    (let ((application (an-application)))
      (app:add-underlay-from-app application (write-picture-file directory "plan.pdf" (two-page-pdf)))
      (click-widget application '(:prompt-button 1))
      (is (null (app:app-prompt application)))
      (is (zerop (dom:underlay-count (app:app-drawing application)))))))

(test the-shortcut-and-the-menu-open-the-dialog-for-a-picture
  (let ((application (an-application)))
    (press application :u :control :shift)
    (is (eq :underlay (app:file-dialog-purpose (app:app-dialog application)))))
  (let ((application (an-application)))
    (click-widget application :more)
    (is (find-widget application '(:menu-item :add-underlay)))
    (is (find-widget application '(:menu-item :underlays)))
    (click-widget application '(:menu-item :add-underlay))
    (is (eq :underlay (app:file-dialog-purpose (app:app-dialog application))))))

;;; Calibrating

(test two-clicks-and-a-distance-give-the-picture-its-scale
  (let* ((application (an-application))
         (underlay (add-picture-underlay application))
         (a '(1 9))
         (b '(11 2)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay (first a) (second a))
      (multiple-value-bind (bx by)
          (dom:underlay-world-point underlay (first b) (second b))
        (click application ax ay)
        (is (search "second point" (app:tool-prompt (app:app-tool application) application)))
        (click application bx by)
        (let ((prompt (form-of application)))
          (is (equal '(:distance :alignment :angle) (mapcar #'app:form-field-key (app:prompt-fields prompt))))
          (is (search "apart now" (app:prompt-message prompt))))
        (retyped-in-form application "5000")
        (press application :enter)
        (is (null (app:app-prompt application)))
        (let ((calibrated (the-underlay application)))
          (is (approx= 5000d0 (measured-distance calibrated a b) 1d-3) "the two points are 5 m apart")
          (is (eq :select (app:tool-kind (app:app-tool application))))
          (is (search "Calibrated" (message-text application)))
          (multiple-value-bind (x y)
              (dom:underlay-world-point calibrated (first a) (second a))
            (is (approx= ax x 1d-6) "the first point did not move")
            (is (approx= ay y 1d-6))))))))

(test the-distance-may-be-typed-in-a-unit
  (let* ((application (an-application))
         (underlay (add-picture-underlay application)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay 1 9)
      (multiple-value-bind (bx by)
          (dom:underlay-world-point underlay 11 2)
        (click application ax ay)
        (click application bx by)
        (retyped-in-form application "12.5 m")
        (press application :enter)
        (is (approx= 12500d0 (measured-distance (the-underlay application) '(1 9) '(11 2)) 1d-3))))))

(test a-calibration-can-also-level-the-line-between-the-points
  (let* ((application (an-application))
         (underlay (add-picture-underlay application)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay 1 9)
      (multiple-value-bind (bx by)
          (dom:underlay-world-point underlay 11 2)
        (click application ax ay)
        (click application bx by)
        (let ((prompt (form-of application)))
          (retyped-in-form application "3000" nil)
          (press application :tab)
          (press application :tab)
          (is (equal '(:distance :alignment :angle) (mapcar #'app:form-field-key (app:prompt-fields prompt)))))
        (click-widget application '(:prompt-field 1))
        (is (eq :horizontal (cdr (assoc :alignment (app:form-values (form-of application))))))
        (press application :enter)
        (let ((calibrated (the-underlay application)))
          (multiple-value-bind (x1 y1)
              (dom:underlay-world-point calibrated 1 9)
            (multiple-value-bind (x2 y2)
                (dom:underlay-world-point calibrated 11 2)
              (is (approx= y1 y2 1d-6) "the line is level")
              (is (approx= 3000d0 (abs (- x2 x1)) 1d-3)))))))))

(test a-calibration-that-cannot-be-made-leaves-the-form-open-and-the-picture-as-it-was
  (let* ((application (an-application))
         (underlay (add-picture-underlay application)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay 1 9)
      (multiple-value-bind (bx by)
          (dom:underlay-world-point underlay 11 2)
        (click application ax ay)
        (click application bx by)
        (retyped-in-form application "")
        (press application :enter)
        (is (app:app-prompt application) "an empty distance")
        (retyped-in-form application "0")
        (press application :enter)
        (is (app:app-prompt application))
        (is (search "above zero" (app:prompt-problem (app:app-prompt application))))
        (retyped-in-form application "abc")
        (press application :enter)
        (is (app:app-prompt application))
        (is (eq underlay (the-underlay application)) "nothing changed")))))

(test the-calibration-tool-needs-two-different-points
  (let* ((application (an-application))
         (underlay (add-picture-underlay application)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay 5 5)
      (click application ax ay)
      (is (app:calibrate-first-point (app:app-tool application)))
      (click application ax ay)
      (is (search "must be apart" (message-text application)))
      (is (null (app:app-prompt application)))
      (press application :escape)
      (is (null (app:calibrate-first-point (app:app-tool application))) "the first Escape forgets the point")
      (press application :escape)
      (is (eq :select (app:tool-kind (app:app-tool application)))))))

(test calibrating-a-hidden-underlay-shows-it
  (let ((application (an-application)))
    (add-picture-underlay application)
    (app:operation-toggle-underlays application)
    (is (not (dom:underlay-visible (the-underlay application))))
    (app:start-calibration application (the-underlay application))
    (is (dom:underlay-visible (the-underlay application)))))

(test the-calibration-tool-shows-the-line-and-its-length-while-the-second-point-is-chosen
  (let* ((application (an-application))
         (underlay (add-picture-underlay application)))
    (multiple-value-bind (ax ay)
        (dom:underlay-world-point underlay 1 9)
      (click application ax ay)
      (multiple-value-bind (bx by)
          (dom:underlay-world-point underlay 11 2)
        (send application (pointer application :move nil bx by))
        (let ((entities (app:tool-preview-entities (app:app-tool application) application)))
          (is (= 2 (length entities)))
          (is (eq :line (dom:entity-kind (first entities))))
          (is (eq :text (dom:entity-kind (second entities)))))))))

(test with-several-underlays-the-click-picks-the-topmost-one-under-it
  (let* ((application (an-application))
         (first (add-picture-underlay application "png-rgb8.png"))
         (second (add-picture-underlay application "png-gray8.png")))
    (declare (ignorable first))
    (let ((underlays (dom:drawing-underlays (app:app-drawing application))))
      (is (= 2 (length underlays)))
      (setf second (second underlays)))
    (app:operation-calibrate-underlay application)
    (is (eq :choice (app:prompt-kind (app:app-prompt application))) "which one?")
    (click-widget application '(:prompt-button 1))
    (is (= (dom:underlay-id second) (app:calibrate-underlay-id (app:app-tool application))))))

;;; Settings

(test the-settings-of-an-underlay-are-changed-in-a-form
  (let* ((application (an-application))
         (underlay (add-picture-underlay application)))
    (app:ask-underlay-settings application underlay)
    (let ((prompt (form-of application)))
      (is (equal '(:name :visible :opacity :x :y :rotation :scale)
                 (mapcar #'app:form-field-key (app:prompt-fields prompt)))))
    (retyped-in-form application "traced.png" nil "35" "1 m" "-20 cm" "12.5" "2")
    (press application :enter)
    (is (null (app:app-prompt application)))
    (let ((changed (the-underlay application)))
      (is (string= "traced.png" (dom:underlay-name changed)))
      (is (approx= 0.35d0 (dom:underlay-opacity changed) 1d-9))
      (is (= 1000d0 (dom:underlay-x changed)))
      (is (= -200d0 (dom:underlay-y changed)))
      (is (= 12.5d0 (dom:underlay-rotation changed)))
      (is (= 2d0 (dom:underlay-scale changed)))
      (is (= (dom:underlay-id underlay) (dom:underlay-id changed)))
      (is (eq (dom:underlay-data underlay) (dom:underlay-data changed))))))

(test settings-that-make-no-sense-keep-the-form-open
  (let* ((application (an-application))
         (underlay (add-picture-underlay application)))
    (app:ask-underlay-settings application underlay)
    (retyped-in-form application "x" nil "250")
    (press application :enter)
    (is (search "opacity" (app:prompt-problem (app:app-prompt application))))
    (retyped-in-form application "x" nil "50" "0" "0" "0" "0")
    (press application :enter)
    (is (search "scale" (app:prompt-problem (app:app-prompt application))))
    (retyped-in-form application "x" nil "50" "0" "0" "0" "oops")
    (press application :enter)
    (is (app:app-prompt application))
    (is (eq underlay (the-underlay application)))))

(test the-page-and-resolution-of-a-pdf-can-be-changed-and-its-size-on-paper-stays
  (with-temporary-directory (directory)
    (let ((application (an-application)))
      (app:add-underlay-from-app application (write-picture-file directory "plan.pdf" (two-page-pdf)))
      (retyped-in-form application "1" "72")
      (press application :enter)
      (let ((before (the-underlay application)))
        (is (= 200 (dom:underlay-pixel-width before)))
        (is (approx= (* 200 (/ 25.4d0 72)) (dom:underlay-width before) 1d-6))
        (app:ask-underlay-settings application before)
        (is (equal '(:name :visible :opacity :x :y :rotation :scale :page :resolution)
                   (mapcar #'app:form-field-key (app:prompt-fields (form-of application)))))
        (retyped-in-form application nil nil nil nil nil nil nil "1" "144")
        (press application :enter)
        (let ((after (the-underlay application)))
          (is (= 144d0 (dom:underlay-dpi after)))
          (is (= 400 (dom:underlay-pixel-width after)) "twice the pixels")
          (is (approx= (dom:underlay-width before) (dom:underlay-width after) 1d-6) "the same size on paper")
          (is (approx= (dom:underlay-x before) (dom:underlay-x after) 1d-9) "where it was"))))))

;;; Choosing, showing, removing

(test with-no-underlays-the-commands-say-so
  (let ((application (an-application)))
    (app:operation-underlays application)
    (is (search "no underlays" (message-text application)))
    (app:operation-calibrate-underlay application)
    (is (search "no underlays" (message-text application)))
    (app:operation-toggle-underlays application)
    (is (search "no underlays" (message-text application)))
    (is (null (app:app-prompt application)))))

(test the-only-underlay-is-asked-about-directly
  (let ((application (an-application)))
    (add-picture-underlay application)
    (app:operation-underlays application)
    (let ((prompt (app:app-prompt application)))
      (is (eq :choice (app:prompt-kind prompt)))
      (is (equal '("Calibrate" "Settings..." "Hide" "Zoom to it" "Remove..." "Cancel")
                 (mapcar #'car (app:prompt-choices prompt)))))))

(test an-underlay-can-be-hidden-and-shown-and-a-hidden-one-is-not-drawn
  (let ((application (an-application)))
    (add-picture-underlay application)
    (is (= 1 (length (app:scene-underlays application))))
    (app:operation-underlays application)
    (click-widget application '(:prompt-button 2))
    (is (not (dom:underlay-visible (the-underlay application))))
    (is (null (app:scene-underlays application)))
    (app:app-render application)
    (is (zerop (length (scene:draw-list-picture-runs (app:app-draw-list application)))))
    (app:operation-underlays application)
    (is (equal "Show" (car (nth 2 (app:prompt-choices (app:app-prompt application))))))
    (click-widget application '(:prompt-button 2))
    (is (dom:underlay-visible (the-underlay application)))))

(test every-underlay-is-hidden-or-shown-at-once
  (let ((application (an-application)))
    (add-picture-underlay application "png-rgb8.png")
    (add-picture-underlay application "png-gray8.png")
    (app:operation-toggle-underlays application)
    (is (notany #'dom:underlay-visible (dom:drawing-underlays (app:app-drawing application))))
    (is (search "hidden" (message-text application)))
    (app:operation-toggle-underlays application)
    (is (every #'dom:underlay-visible (dom:drawing-underlays (app:app-drawing application))))))

(test removing-an-underlay-asks-first-and-is-not-a-step-of-undo
  (let ((application (an-application)))
    (add-picture-underlay application)
    (app:operation-underlays application)
    (click-widget application '(:prompt-button 4))
    (is (search "cannot be undone" (app:prompt-message (app:app-prompt application))))
    (click-widget application '(:prompt-button 1))
    (is (= 1 (dom:underlay-count (app:app-drawing application))) "cancelled")
    (app:operation-underlays application)
    (click-widget application '(:prompt-button 4))
    (click-widget application '(:prompt-button 0))
    (is (zerop (dom:underlay-count (app:app-drawing application))))
    (is (zerop (hash-table-count (app:app-pictures application))) "the pixels are let go")))

(test undo-leaves-the-underlays-alone
  (let ((application (an-application)))
    (add-picture-underlay application)
    (press application :escape)
    (click-widget application :draw)
    (app:operation-undo application)
    (is (= 1 (dom:underlay-count (app:app-drawing application))))))

(test zoom-to-fit-takes-in-the-underlays
  (let ((application (an-application)))
    (add-picture-underlay application)
    (let ((moved (dom:underlay-with (the-underlay application) :x 50000 :y 50000 :scale 10)))
      (dom:replace-underlay (app:app-drawing application) moved)
      (app:operation-zoom-extents application)
      (let ((view (view:viewport-visible-bounds (app:app-viewport application)))
            (bounds (dom:underlay-bounds moved)))
        (is (<= (geo:bounds-min-x view) (geo:bounds-min-x bounds)))
        (is (>= (geo:bounds-max-x view) (geo:bounds-max-x bounds)))
        (is (<= (geo:bounds-min-y view) (geo:bounds-min-y bounds)))
        (is (>= (geo:bounds-max-y view) (geo:bounds-max-y bounds)))))))

(test the-palette-offers-the-underlay-commands
  (let ((application (an-application)))
    (dolist (name '("Underlay: Add a picture or a PDF under the drawing..."
                    "Underlay: Calibrate an underlay"
                    "Underlay: Manage the underlays..."
                    "Underlay: Show or hide the underlays"))
      (is (member name (mapcar #'app:palette-entry-label (app:palette-entries application)) :test #'string=)
          "~a" name))))

;;; Kept with the drawing

(test an-underlay-is-saved-with-the-drawing-and-read-again-when-it-is-opened
  (with-temporary-directory (directory)
    (let* ((application (an-application))
           (underlay (add-picture-underlay application))
           (path (merge-pathnames "traced.bcad" directory)))
      (persist:save-drawing (app:app-drawing application) path)
      (let* ((again (persist:load-drawing path))
             (reopened (make-test-app :drawing again))
             (old (app:picture-entry-raster (app:underlay-picture application underlay)))
             (new (app:picture-entry-raster (app:underlay-picture reopened (first (dom:drawing-underlays again))))))
        (is (= 1 (dom:underlay-count again)))
        (is (equalp (image:raster-pixels old) (image:raster-pixels new)) "the same picture")
        (is (= (dom:underlay-x underlay) (dom:underlay-x (first (dom:drawing-underlays again)))))))))

(test an-underlay-whose-picture-is-damaged-is-left-out-and-the-drawing-still-works
  (let* ((drawing (dom:make-drawing))
         (application (make-test-app :drawing drawing)))
    (dom:add-underlay drawing (dom:make-underlay :name "damaged.png"
                                                 :kind :png
                                                 :data (octets-of 137 80 78 71 13 10 26 10 0 0 0 0)
                                                 :pixel-width 10
                                                 :pixel-height 10))
    (is (null (app:scene-underlays application)))
    (is (search "cannot be shown" (message-text application)))
    (is (eq :error (cdr (app:app-message application))))
    (let ((entry (app:underlay-picture application (the-underlay application))))
      (is (null (app:picture-entry-raster entry)))
      (is (app:picture-entry-problem entry)))
    (app:app-render application)
    (is (zerop (length (scene:draw-list-picture-runs (app:app-draw-list application)))))))

(test a-picture-of-another-size-than-the-file-said-is-corrected-when-it-is-read
  (let* ((drawing (dom:make-drawing))
         (application (make-test-app :drawing drawing)))
    (dom:add-underlay drawing (dom:make-underlay :name "rgb.png"
                                                 :kind :png
                                                 :data (with-open-file (in (fixture-path "images/png-rgb8.png")
                                                                           :element-type '(unsigned-byte 8))
                                                         (let ((bytes (make-array (file-length in)
                                                                                  :element-type '(unsigned-byte 8))))
                                                           (read-sequence bytes in)
                                                           bytes))
                                                 :pixel-width 99
                                                 :pixel-height 77
                                                 :scale 3))
    (let ((shown (first (app:scene-underlays application))))
      (is (= 13 (dom:underlay-pixel-width (first shown))))
      (is (= 11 (dom:underlay-pixel-height (first shown))))
      (is (= 3d0 (dom:underlay-scale (first shown))) "the size of a pixel stays")
      (is (= 13 (dom:underlay-pixel-width (the-underlay application))) "and the drawing is given the right size"))))

(test a-new-drawing-lets-the-old-pictures-go
  (let ((application (an-application)))
    (add-picture-underlay application)
    (is (plusp (hash-table-count (app:app-pictures application))))
    (app:operation-new application)
    (click-widget application '(:prompt-button 1))
    (is (zerop (hash-table-count (app:app-pictures application))))
    (is (zerop (dom:underlay-count (app:app-drawing application))))))
