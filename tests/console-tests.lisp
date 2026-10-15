(in-package #:borba-cad/tests)

(def-suite console
  :description "The console under the canvas, and the scripts run from files and written to them."
  :in all)

(in-suite console)

;;; Helpers

(defun open-console (application)
  "Open the console of APPLICATION with F2."
  (press application :f2))

(defun run-command (application text)
  "Type TEXT in the console of APPLICATION and press Enter."
  (type-text application text)
  (press application :enter))

(defun console-texts (application)
  "Return the texts of the output of the console of APPLICATION, oldest first."
  (reverse (mapcar #'cdr (app:console-lines (app:app-console application)))))

(defun console-kinds (application)
  "Return the kinds of the lines of the output of the console of APPLICATION, oldest first."
  (reverse (mapcar #'car (app:console-lines (app:app-console application)))))

(defun entity-count-of (application)
  "Return how many entities the drawing of APPLICATION has."
  (dom:drawing-entity-count (app:app-drawing application)))

(defun write-script (directory
                     name
                     text)
  "Write a script file called NAME in DIRECTORY and return its path.

DIRECTORY: a folder made for the test.
NAME: the file name.
TEXT: what the file holds."
  (let ((path (merge-pathnames name directory)))
    (touch-text path text)
    path))

;;; Opening and closing

(test f2-opens-the-console-under-the-canvas-and-gives-it-the-keyboard
  (let* ((application (make-test-app))
         (before (canvas-of application)))
    (is (not (app:console-open-p application)))
    (open-console application)
    (is (app:console-open-p application))
    (is (app:console-focused-p application))
    (is (app:app-wants-text-input-p application))
    (let ((canvas (canvas-of application))
          (console (ui:layout-console (app:app-layout application))))
      (is (= (ui:rect-width before) (ui:rect-width canvas)))
      (is (< (ui:rect-height canvas) (ui:rect-height before)) "the canvas gave room to the console")
      (is (= (+ (ui:rect-y canvas) (ui:rect-height canvas)) (ui:rect-y console)) "the console is under the canvas")
      (is (= (+ (ui:rect-height canvas) (ui:rect-height console)) (ui:rect-height before)))
      (is (= (ui:rect-height canvas) (view:viewport-height (app:app-viewport application)))
          "the viewport follows the canvas"))
    (press application :f2)
    (is (not (app:console-open-p application)))
    (is (not (app:app-focus application)))
    (is (= (ui:rect-height before) (view:viewport-height (app:app-viewport application))))))

(test the-console-keeps-the-zoom-when-it-opens-and-closes
  (let* ((application (make-test-app))
         (scale (view:viewport-scale (app:app-viewport application))))
    (open-console application)
    (is (= scale (view:viewport-scale (app:app-viewport application))))
    (press application :f2)
    (is (= scale (view:viewport-scale (app:app-viewport application))))))

(test the-more-menu-opens-the-console-and-the-close-button-closes-it
  (let ((application (make-test-app)))
    (click-widget application :more)
    (click-widget application (list :menu-item :console))
    (is (app:console-open-p application))
    (is-true (find-widget application :console-input))
    (click-widget application :console-close)
    (is (not (app:console-open-p application)))
    (is (null (find-widget application :console-input)) "a closed console has no widgets")))

(test the-console-says-how-to-begin-the-first-time-it-opens
  (let ((application (make-test-app)))
    (open-console application)
    (is (equal '(:note) (console-kinds application)))
    (is (search "(help)" (first (console-texts application))))
    (press application :f2)
    (open-console application)
    (is (= 1 (length (console-texts application))) "and only the first time")))

(test escape-gives-the-keyboard-back-to-the-canvas-and-the-tool-letters-work-again
  (let ((application (make-test-app)))
    (open-console application)
    (type-text application "l")
    (is (eq :select (app:tool-kind (app:app-tool application))) "typed in the console, not a shortcut")
    (press application :escape)
    (is (not (app:console-focused-p application)))
    (is (app:console-open-p application) "the console stays")
    (press application :l)
    (is (eq :line (app:tool-kind (app:app-tool application))))
    (click-widget application :console-input)
    (is (app:console-focused-p application) "a click on the field takes the keyboard back")))

(test a-click-on-the-canvas-takes-the-keyboard-from-the-console
  (let ((application (make-test-app)))
    (open-console application)
    (click application 0 0)
    (is (not (app:console-focused-p application)))
    (is (app:console-open-p application))))

(test a-click-on-the-empty-part-of-the-console-keeps-the-keyboard
  (let* ((application (make-test-app))
         (console (progn (open-console application)
                         (ui:layout-console (app:app-layout application)))))
    (send application (app:make-pointer-event :move nil
                                              (float (+ (ui:rect-x console) 200) 1d0)
                                              (float (+ (ui:rect-y console) 100) 1d0))
          (app:make-pointer-event :down :left
                                  (float (+ (ui:rect-x console) 200) 1d0)
                                  (float (+ (ui:rect-y console) 100) 1d0)))
    (is (app:console-focused-p application))))

;;; Commands

(test a-command-typed-and-entered-draws-and-the-console-shows-what-came-back
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(line 0 0 1000 0)")
    (is (= 1 (entity-count-of application)))
    (is (eq :line (dom:entity-kind (last-entity application))))
    (is (equal '(:note :input :value) (console-kinds application)))
    (is (string= "> (line 0 0 1000 0)" (second (console-texts application))))
    (is (string= "=> 1" (third (console-texts application))))
    (is (app:console-focused-p application) "the console is ready for the next command")
    (is (string= "" (ui:edit-state-text (app:app-edit application))) "and the field is empty")
    (is (app:app-dirty-p application))))

(test several-expressions-in-one-line-show-a-value-for-each
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(+ 1 2) (list 4 5)")
    (is (equal '("=> 3" "=> (4 5)") (rest (rest (console-texts application)))))))

(test what-a-command-prints-is-shown-before-what-it-returns
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(say \"two\" 2)")
    (is (equal '(:note :input :output :value) (console-kinds application)))
    (is (string= "two 2" (third (console-texts application))))))

(test a-command-is-one-step-of-undo-and-redo
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(dotimes (i 3) (circle (* i 500) 0 100))")
    (is (= 3 (entity-count-of application)))
    (press application :escape)
    (press application :z :control)
    (is (= 0 (entity-count-of application)) "one undo took back all three")
    (press application :y :control)
    (is (= 3 (entity-count-of application)))))

(test a-command-that-fails-leaves-no-trace-and-the-console-says-why
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(line 0 0 10 10) (car 5)")
    (is (= 0 (entity-count-of application)))
    (is (eq :error (first (last (console-kinds application)))))
    (press application :escape)
    (press application :z :control)
    (is (= 0 (entity-count-of application)) "there was nothing to undo")
    (is (null (dom:history-can-undo-p (app:app-history application))))))

(test an-invalid-entity-is-an-error-line-not-a-crash
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(circle 0 0 -5)")
    (is (= 0 (entity-count-of application)))
    (is (eq :error (first (last (console-kinds application)))))
    (is (app:console-focused-p application))
    (run-command application "(circle 0 0 5)")
    (is (= 1 (entity-count-of application)) "and the next command works")))

(test an-unfinished-expression-waits-for-its-other-lines
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(line 0 0")
    (is (= 0 (entity-count-of application)))
    (is (plusp (length (app:console-pending (app:app-console application)))))
    (is (string= "..." (ui:widget-label (find-widget application :console-prompt))))
    (run-command application "1000 0)")
    (is (= 1 (entity-count-of application)))
    (is (string= "" (app:console-pending (app:app-console application))))
    (is (string= ">" (ui:widget-label (find-widget application :console-prompt))))
    (is (equal "... 1000 0)" (find "... 1000 0)" (console-texts application) :test #'string=)))))

(test escape-drops-an-unfinished-command-before-it-gives-up-the-keyboard
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(line 0 0")
    (press application :escape)
    (is (string= "" (app:console-pending (app:app-console application))))
    (is (app:console-focused-p application) "the first Escape dropped the command only")
    (press application :escape)
    (is (not (app:console-focused-p application)))))

(test a-blank-line-does-nothing
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "   ")
    (is (null (app:console-history (app:app-console application))))
    (is (= 0 (entity-count-of application)))))

(test a-command-can-select-and-fit-through-the-window
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(line 0 0 1000 0) (circle 5000 0 200) (select (list 1 2))")
    (is (equal '(1 2) (app:selection-ids application)))
    (run-command application "(deselect)")
    (is (null (app:selection-ids application)))
    (let ((scale (view:viewport-scale (app:app-viewport application))))
      (run-command application "(fit)")
      (is (/= scale (view:viewport-scale (app:app-viewport application))) "the view moved to show the drawing"))))

(test the-selection-of-the-window-is-what-a-command-sees
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(line 0 0 1000 0) (line 0 500 1000 500)")
    (press application :escape)
    (click application 500 0)
    (is (equal '(1) (app:selection-ids application)))
    (click-widget application :console-input)
    (run-command application "(move (selected) 0 -100)")
    (is (= -100d0 (dom:line-y1 (first (entities-of application)))))))

(test erasing-what-is-selected-takes-it-out-of-the-selection
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(circle 0 0 100) (select 1) (erase 1)")
    (is (null (app:selection-ids application)))))

;;; History

(test the-arrows-walk-through-the-commands-already-run
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(+ 1 1)")
    (run-command application "(+ 2 2)")
    (type-text application "(draft")
    (press application :up)
    (is (string= "(+ 2 2)" (ui:edit-state-text (app:app-edit application))))
    (press application :up)
    (is (string= "(+ 1 1)" (ui:edit-state-text (app:app-edit application))))
    (press application :up)
    (is (string= "(+ 1 1)" (ui:edit-state-text (app:app-edit application))) "it stops at the oldest")
    (press application :down)
    (is (string= "(+ 2 2)" (ui:edit-state-text (app:app-edit application))))
    (press application :down)
    (is (string= "(draft" (ui:edit-state-text (app:app-edit application))) "and back to what was being typed")
    (press application :down)
    (is (string= "(draft" (ui:edit-state-text (app:app-edit application))))))

(test the-same-command-twice-in-a-row-is-remembered-once
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(+ 1 1)")
    (run-command application "(+ 1 1)")
    (is (equal '("(+ 1 1)") (app:console-history (app:app-console application))))))

(test an-unfinished-command-is-remembered-whole-once-it-runs
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(list 1 ; one")
    (run-command application "2)")
    (is (equal '("(list 1  2)") (app:console-history (app:app-console application)))
        "on one line, and without the comment that would have swallowed the rest")))

(test the-history-and-the-output-are-bounded
  (let ((application (make-test-app)))
    (open-console application)
    (dotimes (index 320)
      (run-command application (format nil "(+ ~d 0)" index)))
    (is (<= (length (app:console-history (app:app-console application))) 200))
    (is (<= (length (app:console-lines (app:app-console application))) 500))
    (is (string= "(+ 319 0)" (first (app:console-history (app:app-console application)))))))

;;; Pasting

(test pasting-puts-the-clipboard-in-the-field-on-one-line-without-comments
  (let ((application (make-test-app)))
    (share-clipboard application)
    (app:set-clipboard-text application (format nil "(line 0 0 ; start~%        1000 0)~%; the end"))
    (open-console application)
    (press application :v :control)
    (let ((text (ui:edit-state-text (app:app-edit application))))
      (is (search "1000 0)" text))
      (is (null (find #\Newline text)))
      (is (null (find #\; text)) "the comments are gone"))
    (press application :enter)
    (is (= 1 (entity-count-of application)))))

;;; Looking at the output

(test the-widgets-of-the-console-are-inside-its-strip
  (dolist (size '((1440d0 . 900d0) (1000d0 . 600d0)))
    (let ((application (make-test-app :width (car size)
                                      :height (cdr size))))
      (open-console application)
      (dotimes (index 40)
        (run-command application (format nil "(say \"line ~d\")" index)))
      (let ((console (ui:layout-console (app:app-layout application))))
        (dolist (widget (app:build-widgets application))
          (when (and (consp (ui:widget-id widget)) (eq :console-row (first (ui:widget-id widget))))
            (is (>= (ui:widget-y widget) (ui:rect-y console)))
            (is (<= (+ (ui:widget-y widget) (ui:widget-height widget))
                    (+ (ui:rect-y console) (ui:rect-height console))))
            (is (<= (+ (ui:widget-x widget) (ui:widget-width widget))
                    (+ (ui:rect-x console) (ui:rect-width console))))))
        (let ((input (find-widget application :console-input)))
          (is (<= (+ (ui:widget-y input) (ui:widget-height input)) (+ (ui:rect-y console) (ui:rect-height console))))
          (is (>= (ui:widget-y input) (ui:rect-y console))))))))

(test the-console-shows-the-last-lines-and-page-keys-and-the-wheel-scroll-the-rest
  (let ((application (make-test-app)))
    (open-console application)
    (dotimes (index 60)
      (run-command application (format nil "(say \"row ~d\")" index)))
    (flet ((last-row-text ()
             (let ((rows (remove-if-not (lambda (widget)
                                          (and (consp (ui:widget-id widget))
                                               (eq :console-row (first (ui:widget-id widget)))))
                                        (app:build-widgets application))))
               (ui:widget-label (first (last (sort rows #'< :key #'ui:widget-y)))))))
      (is (string= "=> NIL" (last-row-text)) "the end of the output")
      (press application :page-up)
      (is (plusp (app:console-scroll (app:app-console application))))
      (let ((scrolled (app:console-scroll (app:app-console application))))
        (press application :page-down)
        (is (< (app:console-scroll (app:app-console application)) scrolled)))
      (let ((console (ui:layout-console (app:app-layout application))))
        (send application (app:make-wheel-event 2d0
                                                (float (+ (ui:rect-x console) 100) 1d0)
                                                (float (+ (ui:rect-y console) 100) 1d0))))
      (is (plusp (app:console-scroll (app:app-console application))) "the wheel over the console scrolls it")
      (let ((scale (view:viewport-scale (app:app-viewport application))))
        (send application (app:make-wheel-event 2d0 700d0 300d0))
        (is (< scale (view:viewport-scale (app:app-viewport application))) "and over the canvas it still zooms"))
      (run-command application "(say \"new\")")
      (is (zerop (app:console-scroll (app:app-console application))) "a new line brings the end back"))))

(test scrolling-stops-at-the-first-line
  (let ((application (make-test-app)))
    (open-console application)
    (dotimes (index 10)
      (run-command application (format nil "(say ~d)" index)))
    (dotimes (index 40)
      (press application :page-up))
    (let ((limit (app:console-scroll (app:app-console application))))
      (press application :page-up)
      (is (= limit (app:console-scroll (app:app-console application)))))))

(test control-l-clears-the-output
  (let ((application (make-test-app)))
    (open-console application)
    (run-command application "(+ 1 1)")
    (press application :l :control)
    (is (null (console-texts application)))))

(test a-long-line-is-wrapped-to-the-width-of-the-console
  (let ((application (make-test-app :width 1000d0
                                    :height 700d0)))
    (open-console application)
    (run-command application (format nil "(say ~s)" (make-string 300 :initial-element #\a)))
    (let ((rows (remove-if-not (lambda (widget)
                                 (and (consp (ui:widget-id widget)) (eq :console-row (first (ui:widget-id widget)))))
                               (app:build-widgets application))))
      (is (> (length rows) 3) "300 characters take several rows")
      (let ((console (ui:layout-console (app:app-layout application))))
        (dolist (row rows)
          (is (<= (ui:text-width-pixels (ui:widget-label row) 12f0 :mono) (ui:rect-width console))))))))

;;; Portuguese

(test the-console-speaks-portuguese
  (let ((application (make-test-app)))
    (i18n:with-language (:pt-br)
      (open-console application)
      (let ((texts (mapcar #'ui:widget-label (app:build-widgets application))))
        (is (null (untranslated-texts (remove nil texts))) "no English text reached the screen")
        (is (find "Fechar o console" (mapcar #'ui:widget-hint (app:build-widgets application)) :test #'equal))
        (is (find-if (lambda (text) (and text (search (format nil "mil~ametros" (code-char 237)) text))) texts))))
    (is (find-if (lambda (text) (and text (search "millimeters" text)))
                 (mapcar #'ui:widget-label (app:build-widgets application)))
        "and in English again afterwards")))

;;; Scripts from files

(defun run-script-question (application
                            path)
  "Ask APPLICATION to run the script at PATH and return the prompt it opened."
  (app:run-script-file-in-app application (uiop:native-namestring path))
  (app:app-prompt application))

(test a-script-file-is-run-only-after-the-user-says-so
  (with-temporary-directory (directory)
    (let* ((application (make-test-app))
           (path (write-script directory
                               "plan.lisp"
                               (format nil ";; a plan~%(rect 0 0 4000 3000)~%(circle 2000 1500 500)~%")))
           (prompt (run-script-question application path)))
      (is-true prompt)
      (is (= 0 (entity-count-of application)) "asking ran nothing")
      (is (search "plan.lisp" (app:prompt-title prompt)))
      (is (search "2 expressions" (app:prompt-message prompt)))
      (is (search "program" (app:prompt-message prompt)) "it says a script is a program")
      (is (search "Run only scripts you trust" (app:prompt-message prompt)))
      (is (search "(rect 0 0 4000 3000)" (app:prompt-message prompt)) "it shows how the script begins")
      (is (null (search "a plan" (app:prompt-message prompt))) "without its comments")
      (is (equal '("Run script" "Cancel") (mapcar #'car (app:prompt-choices prompt))))
      (choose-in-prompt application 0)
      (is (= 2 (entity-count-of application)))
      (is (app:console-open-p application) "the console opened to show what came of it")
      (is (find-if (lambda (text) (search "plan.lisp" text)) (console-texts application)))
      (is (find-if (lambda (text) (search "2 expressions ran" text)) (console-texts application))))))

(test cancelling-the-question-runs-nothing
  (with-temporary-directory (directory)
    (let* ((application (make-test-app))
           (path (write-script directory "plan.lisp" "(circle 0 0 100)")))
      (run-script-question application path)
      (press application :escape)
      (is (null (app:app-prompt application)))
      (is (= 0 (entity-count-of application)))
      (is (not (app:console-open-p application))))))

(test a-script-is-one-step-of-undo
  (with-temporary-directory (directory)
    (let* ((application (make-test-app))
           (path (write-script directory "many.lisp" "(dotimes (i 20) (circle (* i 300) 0 100))")))
      (run-script-question application path)
      (choose-in-prompt application 0)
      (is (= 20 (entity-count-of application)))
      (app:operation-undo application)
      (is (= 0 (entity-count-of application))))))

(test a-script-that-fails-changes-nothing-and-says-so
  (with-temporary-directory (directory)
    (let* ((application (make-test-app))
           (path (write-script directory "bad.lisp" (format nil "(circle 0 0 100)~%(no-such-command 1)"))))
      (run-script-question application path)
      (choose-in-prompt application 0)
      (is (= 0 (entity-count-of application)))
      (is (member :error (console-kinds application)))
      (is (search "entities are as they were" (first (last (console-texts application))))))))

(test a-file-that-is-not-a-script-is-refused-before-any-question
  (with-temporary-directory (directory)
    (let ((application (make-test-app)))
      (dolist (case (list (list "open.lisp" "(circle 0 0 100" "parentheses")
                          (list "close.lisp" "(circle 0 0 100))" "parentheses")
                          (list "empty.lisp" (format nil ";; nothing~%") "empty")
                          (list "blank.lisp" "" "empty")))
        (destructuring-bind (name text reason) case
          (app:run-script-file-in-app application (uiop:native-namestring (write-script directory name text)))
          (is (null (app:app-prompt application)) "~a: no question" name)
          (is (search reason (message-text application)) "~a: ~a" name (message-text application))
          (is (eq :error (cdr (app:app-message application))))))
      (app:run-script-file-in-app application (uiop:native-namestring (merge-pathnames "missing.lisp" directory)))
      (is (eq :error (cdr (app:app-message application))))
      (is (= 0 (entity-count-of application))))))

(test reading-a-script-evaluates-nothing
  (with-temporary-directory (directory)
    (let* ((application (make-test-app))
           (path (write-script directory "trap.lisp" "#.(error \"read-time evaluation\")")))
      (app:run-script-file-in-app application (uiop:native-namestring path))
      (is (null (app:app-prompt application)) "a file the reader would evaluate is not a script")
      (is (eq :error (cdr (app:app-message application)))))))

(test the-name-of-a-script-may-leave-out-the-extension
  (with-temporary-directory (directory)
    (write-script directory "plan.lisp" "(circle 0 0 100)")
    (let ((application (make-test-app)))
      (app:run-script-file-in-app application (uiop:native-namestring (merge-pathnames "plan" directory)))
      (is (app:app-prompt application)))))

(test the-dialog-for-scripts-says-what-it-is-for
  (let ((application (make-test-app)))
    (app:operation-run-script application)
    (is (string= "Run a script" (dialog-title-text application)))
    (app:close-file-dialog application)
    (app:operation-export-script application)
    (is (string= "Export the drawing as a script" (dialog-title-text application)))))

;;; Scripts to files

(test the-drawing-is-written-as-a-script-and-the-script-draws-it-again
  (with-temporary-directory (directory)
    (let* ((application (make-test-app))
           (path (merge-pathnames "out.lisp" directory)))
      (open-console application)
      (run-command application
                   "(layer \"walls\" :color :red) (rect 0 0 4000 3000) (text 100 100 \"Sala\" :height 150 :angle 30)")
      (is (app:export-script-from-app application (uiop:native-namestring path)))
      (is (probe-file path))
      (is (search "exported" (string-downcase (message-text application))))
      (let ((again (make-test-app)))
        (run-script-question again path)
        (choose-in-prompt again 0)
        (is (equal (entity-shapes (app:app-drawing application)) (entity-shapes (app:app-drawing again))))))))

(test exporting-a-script-adds-the-extension-to-a-name-without-one
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (is (app:export-script-from-app application (uiop:native-namestring (merge-pathnames "named" directory))))
      (is (probe-file (merge-pathnames "named.lisp" directory))))))

(test a-script-that-cannot-be-written-is-reported
  (with-temporary-directory (directory)
    (let ((application (make-test-app)))
      (is (null (app:export-script-from-app application
                                            (uiop:native-namestring (merge-pathnames "no-such-folder/out.lisp"
                                                                                     directory)))))
      (is (eq :error (cdr (app:app-message application)))))))

;;; The examples that come with the program

(defun example-path (name)
  "Return the path of the file called NAME in the folder of examples of the project."
  (asdf:system-relative-pathname "borba-cad" (format nil "examples/~a" name)))

(test the-example-scripts-run-and-draw-what-they-say
  (dolist (case '(("house-plan.lisp" 16) ("rosette.lisp" 14)))
    (destructuring-bind (name expected) case
      (let ((application (make-test-app)))
        (run-script-question application (example-path name))
        (choose-in-prompt application 0)
        (is (= expected (entity-count-of application)) "~a" name)
        (is (null (member :error (console-kinds application))) "~a: ~a" name (console-texts application))
        (is (find-if (lambda (text) (search "drawn" text)) (console-texts application)))
        (app:operation-undo application)
        (is (= 0 (entity-count-of application)) "~a is one step of undo" name)))))
