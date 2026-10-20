(in-package #:borba-cad/tests)

(def-suite parametric
  :description "Blocks that code draws from numbers: generators, placements whose numbers change, how they are saved."
  :in all)

(in-suite parametric)

;;; Helpers

(defparameter *grille*
  "(defblock grade ((largura 1000) (altura 600) (passo 100))
  \"A grille: a frame and the bars across it\"
  (rect-size 0 0 largura altura)
  (dotimes (index (1- (floor largura passo)))
    (line (* (1+ index) passo) 0 (* (1+ index) passo) altura)))"
  "The definition of a generator that draws a frame with bars.")

(defun grille-env (&optional (drawing (dom:make-drawing)))
  "Return a script context on DRAWING in which the grille generator is defined; customizations must be bound.

DRAWING: the drawing."
  (let ((context (script-env drawing)))
    (ran-ok context *grille*)
    context))

(defun refusal-text (thunk)
  "Return the sentence that THUNK is refused with, or NIL when it is not.

THUNK: function of no arguments."
  (handler-case (progn (funcall thunk) nil)
    (base:edit-refused (condition)
      (princ-to-string condition))))

(defun entities-in-block (drawing name)
  "Return how many entities a block of the drawing holds, or NIL when there is no such block.

DRAWING: the drawing.
NAME: the name of the block."
  (let ((definition (dom:find-block drawing name)))
    (and definition (length (dom:block-definition-entities definition)))))

;;; Defining generators

(test a-generator-has-a-name-a-description-and-parameters-with-or-without-defaults
  (with-customizations
    (let ((context (script-env)))
      (is (equal '("\"grade\"") (ran-ok context *grille*)))
      (let ((generator (script:find-block-generator script:*customizations* "GRADE")))
        (is (equal "grade" (script:block-generator-name generator)))
        (is (equal "A grille: a frame and the bars across it" (script:block-generator-description generator)))
        (is (equal '(("largura" . 1000d0) ("altura" . 600d0) ("passo" . 100d0))
                   (script:block-generator-parameters generator))))
      (ran-ok context "(defblock \"Post\" (height) (line 0 0 0 height))")
      (let ((generator (script:find-block-generator script:*customizations* "post")))
        (is (equal "Post" (script:block-generator-name generator)) "a string keeps its case")
        (is (equal '(("height")) (script:block-generator-parameters generator)) "no default")
        (is (equal "" (script:block-generator-description generator))))
      (is (equal (list (concatenate 'string
                                    "((\"grade\" \"A grille: a frame and the bars across it\" "
                                    "((\"largura\" 1000.0) (\"altura\" 600.0) (\"passo\" 100.0))) "
                                    "(\"Post\" \"\" (\"height\")))"))
                 (ran-ok context "(generators)")))
      (ran-ok context "(defblock grade (largura) (line 0 0 largura 0))")
      (is (= 2 (length (script:customizations-generators script:*customizations*))) "the same name replaces")
      (is (equal '(("largura")) (script:block-generator-parameters
                                 (script:find-block-generator script:*customizations* "grade")))))))

(test a-block-is-defined-only-where-the-program-keeps-it
  (is (search "only be defined where the program is running"
              (failure-of (script-env) "(defblock grade (largura) (line 0 0 largura 0))"))))

;;; Placing

(test a-placement-draws-the-block-from-the-numbers-it-is-given
  (with-customizations
    (let* ((drawing (dom:make-drawing))
           (context (grille-env drawing)))
      (is (equal '("1") (ran-ok context "(insert \"grade\" 0 0 :with '(1200 900 150))")))
      (let* ((insert (dom:store-find (dom:drawing-store drawing) 1))
             (definition (dom:insert-block insert)))
        (is (equal "grade(1200, 900, 150)" (dom:block-definition-name definition)))
        (is (= 8 (length (dom:block-definition-entities definition))) "the frame and seven bars")
        (is (equal "grade" (dom:block-origin-generator (dom:block-definition-origin definition))))
        (is (equal '(("largura" . 1200d0) ("altura" . 900d0) ("passo" . 150d0))
                   (dom:block-origin-arguments (dom:block-definition-origin definition))))
        (is (approx= 1200d0 (geo:bounds-width (dom:entity-bounds insert))))))))

(test the-numbers-can-be-given-by-name-and-the-others-have-their-defaults
  (with-customizations
    (let* ((drawing (dom:make-drawing))
           (context (grille-env drawing)))
      (ran-ok context "(insert \"grade\" 0 0 :with '(:largura 500))")
      (ran-ok context "(insert \"grade\" 0 0 :with '(\"altura\" 300 :passo 50))")
      (ran-ok context "(insert \"grade\" 0 0 :with '(2000))")
      (is (equal '("grade(1000, 300, 50)" "grade(2000, 600, 100)" "grade(500, 600, 100)")
                 (dom:block-names drawing))))))

(test a-block-that-is-drawn-again-from-the-same-numbers-is-the-same-block
  (with-customizations
    (let* ((drawing (dom:make-drawing))
           (context (grille-env drawing)))
      (ran-ok context "(insert \"grade\" 0 0 :with '(1000 600 100))")
      (ran-ok context "(insert \"grade\" 2000 0 :with '(1000 600 100))")
      (is (= 1 (length (dom:block-names drawing))) "one block, two placements")
      (ran-ok context "(insert \"grade\" 4000 0 :with '(1000 600 200))")
      (is (= 2 (length (dom:block-names drawing)))))))

(test a-placement-that-is-not-told-what-it-needs-says-so
  (with-customizations
    (let ((context (grille-env)))
      (ran-ok context "(defblock two (a b) (line 0 0 a b))")
      (is (search "The parameter b of the block two needs a number"
                  (failure-of context "(insert \"two\" 0 0 :with '(:a 5))")))
      (is (search "has no parameter called other"
                  (failure-of context "(insert \"grade\" 0 0 :with '(:other 5))")))
      (is (search "has 3 parameters, but 4 numbers" (failure-of context "(insert \"grade\" 0 0 :with '(1 2 3 4))")))
      (is (search "no parametric block called nothing" (failure-of context "(insert \"nothing\" 0 0 :with '(1))")))
      (is (search "numbers, or of names and numbers" (failure-of context "(insert \"grade\" 0 0 :with 5)"))))))

(test a-generator-that-fails-or-draws-nothing-or-draws-a-block-is-refused-with-the-reason
  (with-customizations
    (let ((context (grille-env)))
      (ran-ok context "(defblock broken (a) (line 0 0 a 0) (error \"it broke\"))")
      (ran-ok context "(defblock empty (a) a)")
      (ran-ok context "(defblock nested (a) (insert \"other\" 0 0))")
      (is (search "The block broken could not be drawn: it broke"
                  (failure-of context "(insert \"broken\" 0 0 :with '(1))")))
      (is (search "it drew nothing" (failure-of context "(insert \"empty\" 0 0 :with '(1))")))
      (is (search "cannot hold a block" (failure-of context "(insert \"nested\" 0 0 :with '(1))")))
      (is (zerop (dom:drawing-entity-count (script:script-context-drawing context))) "nothing was left behind"))))

(test a-generator-that-takes-too-long-is-stopped
  (with-customizations
    (let ((context (script-env))
          (script:*generator-timeout-seconds* 0.2d0))
      (ran-ok context "(defblock forever (a) (loop))")
      (is (search "took more than 0 seconds" (failure-of context "(insert \"forever\" 0 0 :with '(1))"))))))

(test numbers-that-make-no-sense-are-refused
  (with-customizations
    (let ((generator (progn (ran-ok (script-env) "(defblock post (height) (line 0 0 0 height))")
                            (script:find-block-generator script:*customizations* "post"))))
      (is (search "reasonable size" (refusal-text (lambda () (script:generate-block-definition generator '(1d12))))))
      (is (search "takes 1 number, but 2 were given"
                  (refusal-text (lambda () (script:generate-block-definition generator '(1 2)))))))))

(test a-block-name-that-would-be-too-long-is-shortened-and-stays-the-same-for-the-same-numbers
  (let ((first (script:generated-block-name "a-generator-with-a-name-that-is-quite-long"
                                            '(1234.5678d0 98765.4321d0 11111.1111d0 22222.2222d0)))
        (second (script:generated-block-name "a-generator-with-a-name-that-is-quite-long"
                                             '(1234.5678d0 98765.4321d0 11111.1111d0 22222.2222d0)))
        (other (script:generated-block-name "a-generator-with-a-name-that-is-quite-long"
                                            '(1234.5678d0 98765.4321d0 11111.1111d0 22222.2223d0))))
    (is (<= (length first) dom:+maximum-block-name-length+))
    (is (string= first second))
    (is (string/= first other))
    (is (equal "grade(1.5, 2, 0.25)" (script:generated-block-name "grade" '(1.5d0 2d0 0.25d0))))))

;;; Changing a parameter

(defun drawing-with-a-grille ()
  "Return a drawing that holds a placement of the grille at 1000 by 600 and the script context it was made in."
  (let* ((drawing (dom:make-drawing))
         (context (grille-env drawing)))
    (ran-ok context "(insert \"grade\" 0 0 :with '(1000 600 100))")
    (values drawing context)))

(test the-placement-shows-the-numbers-as-properties
  (with-customizations
    (let* ((drawing (drawing-with-a-grille))
           (insert (dom:store-find (dom:drawing-store drawing) 1))
           (properties (dom:entity-properties insert)))
      (is (equal '("largura" "altura" "passo") (mapcar #'dom:property-label (dom:parameter-properties insert))))
      (let ((property (find '(:parameter "largura") properties :key #'dom:property-key :test #'equal)))
        (is (eq :factor (dom:property-kind property)))
        (is (= 1000d0 (dom:property-value property)))
        (is (dom:property-editable property))))))

(test a-block-drawn-by-hand-has-no-such-properties
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(with-block \"post\" (line 0 0 0 100)) (insert \"post\" 0 0)")
    (is (null (dom:parameter-properties (dom:store-find (dom:drawing-store drawing) 1))))))

(test a-parameter-is-changed-by-drawing-the-block-again
  (with-customizations
    (multiple-value-bind (drawing context) (drawing-with-a-grille)
      (declare (ignore context))
      (let ((changes (script:parameter-changes drawing 1 "largura" 500d0)))
        (is (= 1 (length changes)))
        (dom:apply-changes drawing changes)
        (let* ((insert (dom:store-find (dom:drawing-store drawing) 1))
               (definition (dom:insert-block insert)))
          (is (equal "grade(500, 600, 100)" (dom:block-definition-name definition)))
          (is (= 5 (length (dom:block-definition-entities definition))))
          (is (approx= 500d0 (geo:bounds-width (dom:entity-bounds insert))))
          (is (= 500d0 (cdr (assoc "largura" (dom:block-origin-arguments (dom:block-definition-origin definition))
                                   :test #'string=)))))
        (is (null (script:parameter-changes drawing 1 "largura" 500d0)) "already drawn from that number")))))

(test changing-a-parameter-keeps-the-position-the-scale-and-the-turn
  (with-customizations
    (let* ((drawing (dom:make-drawing))
           (context (grille-env drawing)))
      (ran-ok context "(insert \"grade\" 300 400 :scale 2 :rotation 90 :flipped t :with '(1000 600 100))")
      (dom:apply-changes drawing (script:parameter-changes drawing 1 "altura" 300d0))
      (let ((insert (dom:store-find (dom:drawing-store drawing) 1)))
        (is (= 300d0 (dom:insert-x insert)))
        (is (= 400d0 (dom:insert-y insert)))
        (is (= 2d0 (dom:insert-scale insert)))
        (is (approx= (/ pi 2) (dom:insert-rotation insert)))
        (is (dom:insert-flipped insert))))))

(test a-parameter-that-cannot-be-changed-says-why
  (with-customizations
    (multiple-value-bind (drawing context) (drawing-with-a-grille)
      (ran-ok context "(with-block \"post\" (line 0 0 0 100)) (insert \"post\" 0 0)")
      (flet ((refused (id parameter value)
               (refusal-text (lambda () (script:parameter-changes drawing id parameter value)))))
        (is (search "not a block that a generator drew" (refused 2 "x" 1d0)))
        (is (search "no parameter called nothing" (refused 1 "nothing" 1d0)))
        (is (search "reasonable size" (refused 1 "largura" 1d12)))
        (is (search "not a block that a generator drew" (refused 99 "largura" 1d0)))))))

(test the-generator-must-be-defined-to-draw-the-block-again
  (let ((drawing (with-customizations (nth-value 0 (drawing-with-a-grille)))))
    (with-customizations
      (is (search "The generator grade is not defined here"
                  (refusal-text (lambda () (script:parameter-changes drawing 1 "largura" 500d0))))))))

(test the-console-changes-a-parameter
  (with-customizations
    (multiple-value-bind (drawing context) (drawing-with-a-grille)
      (is (equal '("1") (ran-ok context "(set-parameter 1 :passo 250)")))
      (is (equal "grade(1000, 600, 250)"
                 (dom:block-definition-name (dom:insert-block (dom:store-find (dom:drawing-store drawing) 1)))))
      (is (search "no parameter called x" (failure-of context "(set-parameter 1 \"x\" 5)"))))))

(test the-help-lists-the-parametric-block-commands
  (let ((overview (script:script-result-output (run-script (script-env) "(help)"))))
    (is (search "Parametric blocks" overview))
    (is (search "defblock" overview))
    (is (search "set-parameter" overview))))

;;; Saved with the drawing

(test the-file-says-what-drew-a-block-and-reads-it-back
  (with-customizations
    (let* ((drawing (drawing-with-a-grille))
           (text (persist:drawing-to-string drawing)))
      (is (search (format nil "(format-version ~d)" persist:+supported-format-version+) text))
      (is (search (format nil "  (block (name \"grade(1000, 600, 100)\")~%   (generator \"grade\" ~
                               (arg \"largura\" 1000.0) (arg \"altura\" 600.0) (arg \"passo\" 100.0))~%   (entities")
                  text))
      (let* ((again (persist:parse-drawing-text text))
             (definition (dom:find-block again "grade(1000, 600, 100)")))
        (is (equal '(("largura" . 1000d0) ("altura" . 600d0) ("passo" . 100d0))
                   (dom:block-origin-arguments (dom:block-definition-origin definition))))
        (is (equal '("largura" "altura" "passo")
                   (mapcar #'dom:property-label
                           (dom:parameter-properties (dom:store-find (dom:drawing-store again) 1)))))
        (is (string= text (persist:drawing-to-string again)) "the text is stable")))))

(test a-drawing-opened-where-the-generator-is-not-defined-still-has-its-blocks
  (let* ((text (with-customizations (persist:drawing-to-string (drawing-with-a-grille))))
         (again (persist:parse-drawing-text text))
         (insert (dom:store-find (dom:drawing-store again) 1)))
    (is (= 10 (length (dom:block-definition-entities (dom:insert-block insert)))))
    (is (approx= 1000d0 (geo:bounds-width (dom:entity-bounds insert))))
    (is (search "not defined here"
                (let ((script:*customizations* (script:make-customizations)))
                  (refusal-text (lambda () (script:parameter-changes again 1 "largura" 500d0))))))))

(test a-block-that-a-generator-drew-is-kept-only-while-something-places-it
  (with-customizations
    (multiple-value-bind (drawing context) (drawing-with-a-grille)
      (ran-ok context "(insert \"grade\" 0 0 :with '(2000 600 100)) (with-block \"by-hand\" (line 0 0 5 5))")
      (dom:apply-changes drawing (script:parameter-changes drawing 1 "largura" 500d0))
      (is (= 4 (length (dom:block-names drawing))) "the first grille is still in the drawing, unplaced")
      (let ((text (persist:drawing-to-string drawing)))
        (is (search "grade(500, 600, 100)" text))
        (is (search "grade(2000, 600, 100)" text))
        (is (not (search "grade(1000, 600, 100)" text)) "nothing places it any more")
        (is (search "by-hand" text) "a block drawn by hand is kept whether it is placed or not")
        (is (= 2 (dom:drawing-entity-count (persist:parse-drawing-text text))))))))

(test a-placement-that-undo-brought-back-has-its-block-in-the-file
  (with-customizations
    (multiple-value-bind (drawing context) (drawing-with-a-grille)
      (declare (ignore context))
      (let ((history (dom:make-history)))
        (dom:history-commit history drawing (script:parameter-changes drawing 1 "largura" 500d0))
        (dom:history-undo history drawing)
        (is (search "grade(1000, 600, 100)" (persist:drawing-to-string drawing)))
        (is (= 1 (dom:drawing-entity-count (persist:parse-drawing-text (persist:drawing-to-string drawing)))))))))

(test a-block-that-is-wrong-about-its-generator-is-said-where-it-is
  (with-customizations
    (let ((text (persist:drawing-to-string (drawing-with-a-grille))))
      (flet ((problem (old new)
               (handler-case (progn (persist:parse-drawing-text (replace-once text old new))
                                    nil)
                 (base:drawing-file-content-error (condition)
                   (princ-to-string condition)))))
        (is (search "appears twice" (problem "(arg \"altura\" 600.0)" "(arg \"largura\" 600.0)")))
        (is (search "expected (arg" (problem "(arg \"altura\" 600.0)" "(ark \"altura\" 600.0)")))
        (is (search "expected a quoted string" (problem "(generator \"grade\"" "(generator grade")))
        (is (search "a block has two fields" (problem "(generator \"grade\"" "(origin \"grade\"")))
        (is (search "expected a number" (problem "(arg \"altura\" 600.0)" "(arg \"altura\" \"x\")")))))))

(test a-file-of-version-seven-cannot-have-a-generator-in-a-block
  (with-customizations
    (let ((text (replace-once (persist:drawing-to-string (drawing-with-a-grille))
                              (format nil "(format-version ~d)" persist:+supported-format-version+)
                              "(format-version 7)")))
      (handler-case (progn (persist:parse-drawing-text text)
                           (fail "a generator in a version 7 file was accepted"))
        (base:drawing-file-content-error (condition)
          (is (search "a block has two fields" (princ-to-string condition))))))))

;;; In the program

(defparameter *init-with-grille*
  (concatenate 'string *grille* (string #\Newline) "(defblock post (height) (line 0 0 0 height))")
  "An init file that defines two parametric blocks.")

(defun parametric-app (directory)
  "Return an application whose init file defines the parametric blocks.

DIRECTORY: the configuration directory, in a temporary folder."
  (custom-app directory :init *init-with-grille*))

(test the-palette-lists-the-generators-and-a-form-asks-the-numbers
  (with-temporary-directory (directory)
    (let ((application (parametric-app directory)))
      (press application :k :control)
      (type-text application "block")
      (is (equal '("Block: grade" "Block: post") (remove-if-not (lambda (label) (search "Block:" label))
                                                                (palette-labels application))))
      (type-text application " grade")
      (press application :enter)
      (let ((prompt (app:app-prompt application)))
        (is (eq :form (app:prompt-kind prompt)))
        (is (equal "grade" (app:prompt-title prompt)))
        (is (search "A grille" (app:prompt-message prompt)))
        (is (equal '(("LARGURA" . "1000") ("ALTURA" . "600") ("PASSO" . "100"))
                   (mapcar (lambda (entry) (cons (symbol-name (car entry)) (cdr entry))) (app:form-values prompt))))))))

(test the-numbers-of-the-form-draw-the-block-and-the-insert-tool-places-it
  (with-temporary-directory (directory)
    (let ((application (parametric-app directory)))
      (app:ask-block-parameters application (script:find-block-generator (app:app-customizations application) "grade"))
      (type-text application "1200")
      (press application :tab)
      (type-text application "800")
      (press application :tab)
      (type-text application "200")
      (press application :enter)
      (is (null (app:app-prompt application)))
      (is (eq :insert (app:tool-kind (app:app-tool application))))
      (is (equal '("grade(1200, 800, 200)") (dom:block-names (app:app-drawing application))))
      (click application 1000 500)
      (click application 1000 500)
      (let ((insert (last-entity application)))
        (is (eq :insert (dom:entity-kind insert)))
        (is (= 6 (length (dom:insert-entities insert)))))
      (app:ask-block-parameters application (script:find-block-generator (app:app-customizations application) "grade"))
      (is (equal "1200" (cdr (first (app:form-values (app:app-prompt application))))) "what was typed is the start"))))

(test a-form-with-a-number-that-is-not-one-or-a-block-that-cannot-be-drawn-stays-open
  (with-temporary-directory (directory)
    (let ((application (parametric-app directory)))
      (app:ask-block-parameters application (script:find-block-generator (app:app-customizations application) "grade"))
      (type-text application "abc")
      (press application :enter)
      (is (eq :form (app:prompt-kind (app:app-prompt application))))
      (is (search "Invalid input" (app:prompt-problem (app:app-prompt application))))
      (press application :a :control)
      (type-text application "0")
      (press application :enter)
      (is (eq :form (app:prompt-kind (app:app-prompt application))) "a width of nothing draws a frame that is refused")
      (is (search "could not be drawn" (app:prompt-problem (app:app-prompt application))))
      (is (null (dom:block-names (app:app-drawing application))) "nothing was added"))))

(test the-insert-question-knows-the-generators-too
  (with-temporary-directory (directory)
    (let ((application (parametric-app directory)))
      (app:operation-set-tool application :insert)
      (is (string= "Insert block" (prompt-title-of application)) "there are no blocks, but there are generators")
      (is (search "grade, post" (app:prompt-message (app:app-prompt application))))
      (type-text application "post")
      (press application :enter)
      (is (eq :form (app:prompt-kind (app:app-prompt application))))
      (is (equal "post" (app:prompt-title (app:app-prompt application)))))))

(test with-no-block-and-no-generator-the-insert-says-so
  (let ((application (make-test-app)))
    (app:operation-set-tool application :insert)
    (is (null (app:app-prompt application)))
    (is (search "no blocks yet" (message-text application)))))

(defun placed-grille (directory)
  "Return an application with a placement of the grille, the only entity, selected.

DIRECTORY: the configuration directory, in a temporary folder."
  (let ((application (parametric-app directory)))
    (run-command-in-console application "(insert \"grade\" 0 0 :with '(1000 600 100))")
    (app:select-ids application '(1))
    application))

(defun run-command-in-console (application text)
  "Run TEXT as a console command in APPLICATION.

APPLICATION: the application.
TEXT: the source."
  (let ((result (app::run-source application text)))
    (is (eq :ok (script:script-result-status result)) "~a: ~a" text (script:script-result-message result))))

(test the-inspector-shows-the-numbers-and-editing-one-draws-the-block-again
  (with-temporary-directory (directory)
    (let ((application (placed-grille directory)))
      (is (find-widget application '(:property (:parameter "largura"))))
      (is (find-widget application '(:property (:parameter "passo"))))
      (focus-property-field application '(:parameter "largura"))
      (type-text application "400")
      (press application :enter)
      (let ((insert (last-entity application)))
        (is (equal "grade(400, 600, 100)" (dom:block-definition-name (dom:insert-block insert))))
        (is (approx= 400d0 (geo:bounds-width (dom:entity-bounds insert)))))
      (is (search "drawn again" (message-text application)))
      (is (find-widget application '(:property (:parameter "largura"))) "the numbers are there for another change"))))

(test undo-takes-the-old-block-back
  (with-temporary-directory (directory)
    (let ((application (placed-grille directory)))
      (focus-property-field application '(:parameter "altura"))
      (type-text application "250")
      (press application :enter)
      (is (approx= 250d0 (geo:bounds-height (dom:entity-bounds (last-entity application)))))
      (app:operation-undo application)
      (is (approx= 600d0 (geo:bounds-height (dom:entity-bounds (last-entity application)))))
      (app:operation-redo application)
      (is (approx= 250d0 (geo:bounds-height (dom:entity-bounds (last-entity application))))))))

(test a-number-that-is-not-a-number-in-the-inspector-is-refused
  (with-temporary-directory (directory)
    (let ((application (placed-grille directory)))
      (focus-property-field application '(:parameter "largura"))
      (type-text application "wide")
      (press application :enter)
      (is (eq :error (cdr (app:app-message application))))
      (is (equal "grade(1000, 600, 100)" (dom:block-definition-name (dom:insert-block (last-entity application))))))))

(test a-generator-that-fails-for-a-number-in-the-inspector-leaves-the-block-as-it-was
  (with-temporary-directory (directory)
    (let ((application (placed-grille directory)))
      (focus-property-field application '(:parameter "passo"))
      (type-text application "0")
      (press application :enter)
      (is (eq :error (cdr (app:app-message application))) "a bar every nothing is division by zero")
      (is (equal "grade(1000, 600, 100)" (dom:block-definition-name (dom:insert-block (last-entity application))))))))

(test without-the-generator-the-numbers-are-shown-and-changing-one-says-why
  (with-temporary-directory (directory)
    (let ((application (placed-grille directory)))
      (let ((text (persist:drawing-to-string (app:app-drawing application)))
            (other (make-test-app)))
        (setf (app:app-drawing other) (persist:parse-drawing-text text))
        (app:select-ids other '(1))
        (is (find-widget other '(:property (:parameter "largura"))) "shown")
        (focus-property-field other '(:parameter "largura"))
        (type-text other "400")
        (press other :enter)
        (is (search "not defined here" (message-text other)))
        (is (eq :error (cdr (app:app-message other))))))))

(test an-entity-picked-in-a-drawing-of-blocks-by-hand-has-no-number-rows
  (let ((application (make-test-app)))
    (run-command-in-console application "(with-block \"post\" (line 0 0 0 100)) (insert \"post\" 0 0)")
    (app:select-ids application '(1))
    (is (null (find-widget application '(:property (:parameter "x")))))))
