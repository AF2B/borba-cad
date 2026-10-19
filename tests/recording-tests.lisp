(in-package #:borba-cad/tests)

(def-suite recording
  :description "A session as the commands that make it: the steps of undo, written as a script."
  :in all)

(in-suite recording)

;;; Helpers

(defun recorded-session (setup
                         &rest commands)
  "Run SETUP on a new drawing, then each of COMMANDS as one step, and return the script that records the steps and
the drawing as the steps leave it.

SETUP: source that makes the drawing the recording begins from, or NIL.
COMMANDS: sources, one for each step."
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (when setup
      (ran-ok context setup))
    (let ((steps (loop for command in commands
                       do (ran-ok context command)
                       collect (script:context-changes context))))
      (values (script:steps-script steps drawing)
              drawing))))

(defun replayed-shapes (setup
                        script)
  "Run SETUP and then SCRIPT on a new drawing and return the shapes it ends with.

SETUP: source that makes the drawing the recording began from, or NIL.
SCRIPT: the recorded script."
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (when setup
      (ran-ok context setup))
    (script:context-reset context)
    (ran-ok context script)
    (entity-shapes drawing)))

(defun script-lines (script)
  "Return the lines of SCRIPT that are not comments or blank.

SCRIPT: the text of a script."
  (remove-if (lambda (line)
               (or (zerop (length line))
                   (char= #\; (char line 0))))
             (uiop:split-string script :separator '(#\Newline))))

(defun records-the-session-p (setup
                              &rest commands)
  "Return true when the script recorded from COMMANDS, run on the drawing SETUP makes, ends with the same shapes as
the commands did.

SETUP: source that makes the drawing the recording begins from, or NIL.
COMMANDS: sources, one for each step."
  (multiple-value-bind (script drawing) (apply #'recorded-session setup commands)
    (equal (entity-shapes drawing)
           (replayed-shapes setup script))))

(defun declared-layer (drawing
                       layer-id)
  "Return the command that makes a layer, with its color, as a recorded script writes it the first time.

DRAWING: the drawing.
LAYER-ID: the id of the layer."
  (script::layer-command (dom:find-layer drawing layer-id)))

;;; Drawing

(test a-recorded-drawing-is-the-commands-that-draw-it
  (multiple-value-bind (script drawing)
      (recorded-session nil
                        "(line 0 0 100 0)"
                        "(circle 50 50 20)"
                        "(rect-size 0 0 30 40)")
    (is (equal (list (declared-layer drawing dom:+default-layer-id+)
                     "(line 0 0 100 0)"
                     "(circle 50 50 20)"
                     "(rect-size 0 0 30 40)")
               (script-lines script)))
    (is (= 3 (dom:drawing-entity-count drawing)))
    (is (equal (entity-shapes drawing)
               (replayed-shapes nil script)))))

(test a-script-begins-with-the-comment-that-says-what-it-is
  (let ((script (recorded-session nil "(line 0 0 1 1)")))
    (is (search "recorded in the editor" script))
    (is (search "as it was when the recording began" script))))

(test a-session-with-nothing-in-it-is-a-script-with-no-commands
  (is (null (script-lines (script:steps-script '() (dom:make-drawing))))))

(test entities-are-drawn-on-the-layer-they-are-on
  (multiple-value-bind (script drawing)
      (recorded-session nil
                        "(layer \"Walls\" :color :red) (line 0 0 100 0)"
                        "(layer \"Doors\") (circle 5 5 2) (layer \"Walls\") (line 0 9 9 9)")
    (is (member (declared-layer drawing (dom:layer-id (dom:find-layer-by-name drawing "Walls")))
                (script-lines script)
                :test #'string=))
    (is (member (declared-layer drawing (dom:layer-id (dom:find-layer-by-name drawing "Doors")))
                (script-lines script)
                :test #'string=))
    (is (member "(layer \"Walls\")" (script-lines script) :test #'string=))
    (is (equal (entity-shapes drawing)
               (replayed-shapes nil script)))))

;;; Changing

(test an-entity-that-was-already-there-is-named-by-its-number
  (let ((setup "(line 0 0 100 0) (circle 5 5 3)"))
    (multiple-value-bind (script drawing) (recorded-session setup "(move 1 10 20)" "(erase 2)")
      (is (equal '("(move 1 10 20)" "(erase 2)")
                 (script-lines script)))
      (is (equal (entity-shapes drawing)
                 (replayed-shapes setup script))))))

(test an-entity-the-recording-made-is-named-by-a-variable
  (multiple-value-bind (script drawing)
      (recorded-session nil "(line 0 0 10 0)" "(move 1 5 5)" "(erase 1)")
    (is (equal (list (declared-layer drawing dom:+default-layer-id+)
                     "(defparameter *line-1* (line 0 0 10 0))"
                     "(move *line-1* 5 5)"
                     "(erase *line-1*)")
               (script-lines script)))
    (is (zerop (dom:drawing-entity-count drawing)))
    (is (equal (entity-shapes drawing)
               (replayed-shapes nil script)))))

(test a-variable-does-not-depend-on-the-number-the-entity-had
  (is (records-the-session-p nil
                             "(line 0 0 10 0)"
                             "(erase 1)"
                             "(circle 0 0 4)"
                             "(move 2 3 3)")))

(test entities-moved-together-are-moved-by-one-command
  (let ((setup "(line 0 0 10 0) (line 0 5 10 5) (line 0 9 10 9) (circle 50 50 2)"))
    (multiple-value-bind (script drawing) (recorded-session setup "(move '(1 2 3) 15 -5)")
      (is (equal '("(move '(1 2 3) 15 -5)") (script-lines script)))
      (is (equal (entity-shapes drawing)
                 (replayed-shapes setup script))))))

(test entities-erased-together-are-erased-by-one-command
  (let ((setup "(line 0 0 10 0) (line 0 5 10 5) (line 0 9 10 9)"))
    (is (equal '("(erase '(1 2 3))")
               (script-lines (recorded-session setup "(erase '(1 2 3))"))))))

(test a-move-by-a-fraction-is-written-as-the-distance-moved
  (let ((setup "(line 0 0 10 0)"))
    (multiple-value-bind (script drawing) (recorded-session setup "(move 1 0.1 0.2)")
      (is (equal '("(move 1 0.1 0.2)") (script-lines script)))
      (is (equal (entity-shapes drawing)
                 (replayed-shapes setup script))))))

(test a-changed-property-is-an-edit
  (let ((setup "(circle 5 5 3) (text 0 0 \"Hall\" :height 100)"))
    (multiple-value-bind (script drawing)
        (recorded-session setup "(edit 1 :radius 40)" "(edit 2 :string \"Kitchen\" :height 200)")
      (is (equal '("(edit 1 :radius 40)" "(edit 2 :string \"Kitchen\" :height 200)")
                 (script-lines script)))
      (is (equal (entity-shapes drawing)
                 (replayed-shapes setup script))))))

(test an-angle-in-an-edit-is-written-in-degrees
  (let ((setup "(text 0 0 \"Hall\" :height 100)"))
    (multiple-value-bind (script drawing) (recorded-session setup "(edit 1 :angle 30)")
      (is (equal '("(edit 1 :angle 30)") (script-lines script)))
      (is (equal (entity-shapes drawing)
                 (replayed-shapes setup script))))))

(test a-change-no-property-says-redraws-the-entity-in-its-place
  (let ((setup "(rect-size 0 0 100 50) (circle 5 5 3)"))
    (multiple-value-bind (script drawing) (recorded-session setup "(rotate 1 30)")
      (is (= 1 (length (script-lines script))))
      (is (eql 0 (search "(redraw 1 (pline " (first (script-lines script)))))
      (is (equal '(1 2) (mapcar #'dom:entity-id (dom:drawing-entities drawing))))
      (is (equal (entity-shapes drawing)
                 (replayed-shapes setup script))))))

(test an-entity-moved-to-another-layer-is-moved-to-it
  (let ((setup "(line 0 0 10 0) (line 0 5 10 5)"))
    (multiple-value-bind (script drawing)
        (recorded-session setup "(layer \"Walls\" :color :red) (move-to-layer '(1 2) \"Walls\")")
      (is (equal (list (declared-layer drawing (dom:layer-id (dom:find-layer-by-name drawing "Walls")))
                       "(move-to-layer '(1 2) \"Walls\")")
                 (script-lines script)))
      (is (equal (entity-shapes drawing)
                 (replayed-shapes setup script))))))

(test a-mirror-a-scale-and-a-copy-can-be-recorded
  (is (records-the-session-p "(line 0 0 100 0) (circle 50 50 20) (text 3 4 \"A\" :height 50)"
                             "(mirror '(1 2 3) 0 0 0 100)"
                             "(scale '(1 2) 2 :about-x 10 :about-y 10)"
                             "(rotate '(1 3) 45 :about-x 5 :about-y 5)"
                             "(copy '(1 2) 7 7)")))

;;; Blocks

(test a-placed-block-is-made-before-it-is-placed
  (multiple-value-bind (script drawing)
      (recorded-session nil
                        "(with-block \"tree\" (circle 0 0 50) (line 0 0 0 -200))"
                        "(insert \"tree\" 100 100 :scale 2 :rotation 30)"
                        "(move 1 10 10)")
    (is (eql 0 (search ";;;;" script)))
    (let ((lines (script-lines script)))
      (is (string= "(with-block \"tree\"" (first lines)))
      (is (member "(move *insert-1* 10 10)" lines :test #'string=)))
    (is (equal (entity-shapes drawing)
               (replayed-shapes nil script)))))

(test a-block-placed-and-exploded-can-be-recorded
  (is (records-the-session-p nil
                             "(with-block \"post\" (line 0 0 0 100) (circle 0 100 10))"
                             "(insert \"post\" 0 0)"
                             "(insert \"post\" 500 0 :rotation 90)"
                             "(explode 1)")))

;;; In the program

(defun recording-app ()
  "Return an application with the console open, to record what is typed in it."
  (let ((application (make-test-app)))
    (open-console application)
    application))

(test recording-begins-and-ends-with-the-same-command
  (let ((application (recording-app)))
    (is (not (app:app-recording-p application)))
    (app:operation-toggle-recording application)
    (is (app:app-recording-p application))
    (is (search "Recording" (message-text application)))
    (run-command application "(line 0 0 100 0)")
    (app:operation-toggle-recording application)
    (is (not (app:app-recording-p application)))
    (is (string= "Save the recorded actions as a script" (dialog-title-text application)))))

(test the-recording-is-the-steps-made-while-it-lasted
  (let ((application (recording-app)))
    (run-command application "(circle 0 0 10)")
    (app:operation-start-recording application)
    (run-command application "(line 0 0 100 0)")
    (run-command application "(move 2 5 5)")
    (app:operation-stop-recording application)
    (let ((lines (script-lines (app:app-recorded-script application))))
      (is (= 3 (length lines)))
      (is (equal "(defparameter *line-2* (line 0 0 100 0))" (second lines)))
      (is (equal "(move *line-2* 5 5)" (third lines))))))

(test an-action-that-was-undone-is-not-in-the-recording
  (let ((application (recording-app)))
    (app:operation-start-recording application)
    (run-command application "(line 0 0 100 0)")
    (run-command application "(circle 0 0 10)")
    (app:operation-undo application)
    (app:operation-stop-recording application)
    (let ((lines (script-lines (app:app-recorded-script application))))
      (is (member "(line 0 0 100 0)" lines :test #'string=))
      (is (notany (lambda (line) (search "circle" line)) lines)))))

(test an-action-that-was-done-again-is-in-the-recording
  (let ((application (recording-app)))
    (app:operation-start-recording application)
    (run-command application "(line 0 0 100 0)")
    (app:operation-undo application)
    (app:operation-redo application)
    (app:operation-stop-recording application)
    (is (member "(line 0 0 100 0)" (script-lines (app:app-recorded-script application)) :test #'string=))))

(test a-recording-with-nothing-in-it-says-so-and-asks-for-no-file
  (let ((application (recording-app)))
    (app:operation-start-recording application)
    (app:operation-stop-recording application)
    (is (null (app:app-dialog application)))
    (is (string= "Nothing was recorded" (prompt-title-of application)))
    (is (not (app:app-recording-p application)))))

(test a-recording-ends-when-the-drawing-is-replaced
  (let ((application (recording-app)))
    (app:operation-start-recording application)
    (run-command application "(line 0 0 100 0)")
    (app:operation-new application)
    (when (app:app-prompt application)
      (choose-in-prompt application 1))
    (app:operation-stop-recording application)
    (is (null (app:app-dialog application)))
    (is (string= "The recording was lost" (prompt-title-of application)))))

(test undoing-what-came-before-the-recording-loses-it
  (let ((application (recording-app)))
    (run-command application "(circle 0 0 10)")
    (app:operation-start-recording application)
    (run-command application "(line 0 0 100 0)")
    (app:operation-undo application)
    (app:operation-undo application)
    (app:operation-stop-recording application)
    (is (string= "The recording was lost" (prompt-title-of application)))))

(test the-recording-is-saved-and-the-file-draws-the-same-again
  (with-temporary-directory (directory)
    (let ((application (recording-app))
          (path (merge-pathnames "session.lisp" directory)))
      (app:operation-start-recording application)
      (run-command application "(layer \"walls\" :color :red) (rect-size 0 0 4000 3000)")
      (run-command application "(text 100 100 \"Sala\" :height 150)")
      (run-command application "(move 1 100 0)")
      (app:operation-stop-recording application)
      (is (app:save-recording-from-app application (uiop:native-namestring path)))
      (is (probe-file path))
      (is (string= "Recording saved" (prompt-title-of application)))
      (let ((again (make-test-app)))
        (run-script-question again path)
        (choose-in-prompt again 0)
        (is (equal (entity-shapes (app:app-drawing application))
                   (entity-shapes (app:app-drawing again))))))))

(test the-recording-can-be-started-from-the-status-bar-the-menu-and-the-keyboard
  (let ((application (make-test-app)))
    (is (find-widget application :record))
    (click-widget application :record)
    (is (app:app-recording-p application))
    (is (ui:widget-active (find-widget application :record)))
    (press application :f9)
    (is (not (app:app-recording-p application)))
    (is (app:app-prompt application))
    (app:close-prompt application)
    (click-widget application :more)
    (is (member '(:menu-item :record) (mapcar #'ui:widget-id (menu-rows application)) :test #'equal))
    (click-widget application '(:menu-item :record))
    (is (app:app-recording-p application))
    (click-widget application :more)
    (is (equal "Stop recording..." (ui:widget-label (find '(:menu-item :record)
                                                          (menu-rows application)
                                                          :key #'ui:widget-id
                                                          :test #'equal))))))
