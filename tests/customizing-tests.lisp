(in-package #:borba-cad/tests)

(def-suite customizing
  :description "The init file, commands, keys and hooks of the user, the command palette, recordings kept as commands."
  :in all)

(in-suite customizing)

;;; Helpers

(defmacro with-customizations (&body body)
  "Run BODY with an empty set of customizations bound, as the program does while it runs a definition.

BODY: the forms to run."
  `(let ((script:*customizations* (script:make-customizations)))
     ,@body))

(defun write-text-file (path
                        text)
  "Write TEXT to the file at PATH, making its folder.

PATH: the file.
TEXT: what it holds."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text stream))
  path)

(defun custom-app (directory
                   &key init
                        (load-init t))
  "Return an application whose configuration directory is DIRECTORY, after it has read its customizations, with an
init file that holds INIT when that is given.

DIRECTORY: the configuration directory, in a temporary folder.
INIT: the text of the init file, or NIL for none.
LOAD-INIT: false to start as with --no-init."
  (when init
    (write-text-file (merge-pathnames "init.lisp" directory) init))
  (let ((config (quiet-config)))
    (setf (base:config-config-directory config) directory
          (base:config-load-init config) load-init)
    (let ((application (app:make-app config
                                     :width 1440d0
                                     :height 900d0)))
      (set-test-view application)
      (app:start-customizations application)
      application)))

(defparameter *bolt-init*
  "(defcommand \"Draw bolt\" \"Draws a bolt of M10, a circle of 5 mm radius\"
  (circle 0 0 5))
(defcommand \"Say hello\" \"Prints a greeting\"
  (say \"hello from the init file\"))
(bind-key \"Ctrl+Alt+B\" \"Draw bolt\")
(bind-key \"Ctrl+Alt+G\" \"View: Show or hide the grid\")
"
  "An init file that defines two commands and binds two keys.")

(defun console-text (application)
  "Return everything the console of APPLICATION shows, oldest first, as one string.

APPLICATION: the application."
  (format nil "~{~a~^~%~}"
          (mapcar #'cdr (reverse (app:console-lines (app:app-console application))))))

(defun command-names (application)
  "Return the names of the commands that were defined, in the order they were.

APPLICATION: the application."
  (mapcar #'script:user-command-name
          (script:customizations-commands (app:app-customizations application))))

(defun palette-labels (application)
  "Return the labels of the commands that the open palette lists for what is typed.

APPLICATION: the application."
  (mapcar #'app:palette-entry-label (app:palette-prompt-matches (app:app-prompt application))))

(defun open-palette (application)
  "Open the command palette with Ctrl+K.

APPLICATION: the application."
  (press application :k :control)
  (is (eq :palette (app:prompt-kind (app:app-prompt application)))))

;;; Key bindings as they are written

(test a-key-binding-is-the-modifiers-and-a-key-joined-by-plus
  (is (equal '(:h (:alt :control)) (multiple-value-list (script:parse-chord "Ctrl+Alt+H"))))
  (is (equal '(:h (:alt :control)) (multiple-value-list (script:parse-chord "alt + CTRL + h")))
      "case and blanks do not matter")
  (is (equal '(:f5 (:shift)) (multiple-value-list (script:parse-chord "Shift+F5"))))
  (is (equal '(:f7 nil) (multiple-value-list (script:parse-chord "F7"))) "a function key needs no modifier")
  (is (equal '(:up (:control)) (multiple-value-list (script:parse-chord "Ctrl+Up"))))
  (is (equal '(:page-down (:control :shift)) (multiple-value-list (script:parse-chord "Shift+Control+PageDown"))))
  (is (equal "Ctrl+Alt+H" (script:chord-text :h '(:alt :control))))
  (is (equal "Ctrl+Shift+Page Down" (script:chord-text :page-down '(:shift :control))))
  (is (equal "F5" (script:chord-text :f5 nil))))

(test a-key-binding-that-would-get-in-the-way-is-refused-and-says-why
  (flet ((problem (text)
           (handler-case (progn (script:parse-chord text) nil)
             (error (condition) (princ-to-string condition)))))
    (is (search "is not Ctrl, Shift or Alt" (problem "Hyper+H")))
    (is (search "is not a key" (problem "Ctrl+Banana")))
    (is (search "the program needs escape" (problem "Ctrl+Escape")))
    (is (search "the program needs enter" (problem "Alt+Enter")))
    (is (search "alone is what the program uses" (problem "H")) "a letter alone is what is typed")
    (is (search "add Ctrl or Alt" (problem "Shift+H")) "Shift and a letter choose tools")
    (is (null (problem "Ctrl+Shift+H")))
    (is (null (problem "Shift+Delete")))))

;;; Defining commands, keys and hooks

(test a-command-is-defined-with-a-name-a-description-and-a-body
  (with-customizations
    (let ((context (script-env)))
      (is (equal '("\"Draw bolt\"") (ran-ok context "(defcommand \"Draw bolt\" \"A bolt\" (circle 0 0 5))")))
      (is (= 0 (dom:drawing-entity-count (script:script-context-drawing context))) "defining draws nothing")
      (let ((command (script:find-user-command script:*customizations* "draw BOLT")))
        (is (equal "Draw bolt" (script:user-command-name command)))
        (is (equal "A bolt" (script:user-command-description command)))
        (is (functionp (script:user-command-function command))))
      (is (equal '("(\"Draw bolt\")") (ran-ok context "(commands)")))
      (ran-ok context "(defcommand \"draw bolt\" \"A better bolt\" (circle 0 0 6))")
      (is (equal '("(\"draw bolt\")") (ran-ok context "(commands)")) "the same name in another case replaces it")
      (is (equal "A better bolt"
                 (script:user-command-description (script:find-user-command script:*customizations* "Draw bolt")))))))

(test a-command-that-is-badly-defined-says-so
  (with-customizations
    (let ((context (script-env)))
      (is (search "needs a name" (failure-of context "(defcommand \"\" \"x\" 1)")))
      (is (search "needs a description" (failure-of context "(defcommand \"a\" 5 1)"))))))

(test definitions-need-the-program-to-keep-them
  (let ((context (script-env)))
    (is (search "only be defined where the program is running"
                (failure-of context "(defcommand \"a\" \"b\" 1)")))))

(test a-key-is-bound-to-a-command-by-its-name-and-found-by-its-modifiers-in-any-order
  (with-customizations
    (let ((context (script-env)))
      (is (equal '("\"Ctrl+Alt+H\"") (ran-ok context "(bind-key \"Alt+Ctrl+H\" \"Draw bolt\")")))
      (is (equal "Draw bolt" (script:find-key-binding script:*customizations* :h '(:control :alt))))
      (is (equal "Draw bolt" (script:find-key-binding script:*customizations* :h '(:alt :control))))
      (is (null (script:find-key-binding script:*customizations* :h '(:control))))
      (is (null (script:find-key-binding script:*customizations* :j '(:control :alt))))
      (ran-ok context "(bind-key \"Ctrl+Alt+H\" \"Another\")")
      (is (equal "Another" (script:find-key-binding script:*customizations* :h '(:control :alt)))
          "a key has one binding")
      (is (equal '("((\"Ctrl+Alt+H\" \"Another\"))") (ran-ok context "(bindings)")))
      (is (search "is not a key binding" (failure-of context "(bind-key \"H\" \"x\")")))
      (is (search "bound to the name of a command" (failure-of context "(bind-key \"Ctrl+H\" 5)"))))))

(test a-hook-is-added-to-an-event-and-the-events-are-checked
  (with-customizations
    (let ((context (script-env)))
      (is (equal '(":DRAWING-OPENED") (ran-ok context "(add-hook :drawing-opened (lambda () 1))")))
      (ran-ok context "(add-hook :drawing-opened \"A command\")")
      (is (= 2 (length (script:hook-functions script:*customizations* :drawing-opened))))
      (is (null (script:hook-functions script:*customizations* :before-save)))
      (is (search "not something a hook can be added to" (failure-of context "(add-hook :whenever (lambda () 1))")))
      (is (search "function of no arguments" (failure-of context "(add-hook :startup 5)"))))))

(test the-help-lists-what-the-user-can-define
  (let ((overview (script:script-result-output (run-script (script-env) "(help)"))))
    (is (search "Customizing" overview))
    (is (search "defcommand" overview))
    (is (search "bind-key" overview))))

;;; The configuration

(test the-configuration-directory-is-the-users-own
  (unless (uiop:os-windows-p)
    (flet ((directory-for (&rest bindings)
             (base:user-config-directory (environment-from bindings))))
      (is (equal #p"/data/cfg/borba-cad/" (directory-for '("XDG_CONFIG_HOME" . "/data/cfg"))))
      (is (equal (merge-pathnames ".config/borba-cad/" (user-homedir-pathname)) (directory-for))
          "without XDG_CONFIG_HOME it is under the home directory"))))

(test the-configuration-directory-and-the-init-file-can-be-set-from-the-environment-and-the-command-line
  (let ((base (base:default-config :project-root #p"/proj/")))
    (is (base:config-load-init base))
    (is (not (null (base:config-config-directory base))))
    (let ((config (base:config-with-environment base (environment-from '(("BORBA_CAD_CONFIG_DIR" . "/cfg")
                                                                         ("BORBA_CAD_NO_INIT" . "1"))))))
      (is (equal #p"/cfg/" (base:config-config-directory config)))
      (is (not (base:config-load-init config))))
    (multiple-value-bind (config remaining)
        (base:config-with-arguments base '("--config-dir" "/c" "--no-init" "--demo"))
      (is (equal #p"/c/" (base:config-config-directory config)))
      (is (not (base:config-load-init config)))
      (is (equal '("--demo") remaining)))))

;;; The init file

(test the-init-file-defines-commands-and-keys-when-the-program-starts
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init*)))
      (is (equal '("Draw bolt" "Say hello")
                 (command-names application)))
      (is (= 2 (length (script:customizations-keys (app:app-customizations application)))))
      (is (zerop (dom:drawing-entity-count (app:app-drawing application))) "nothing was drawn")
      (is (search "Init file ran: commands 2, keys 2, hooks 0" (console-text application)))
      (is (null (app:app-prompt application))))))

(test with-no-init-file-nothing-happens
  (with-temporary-directory (directory)
    (let ((application (custom-app directory)))
      (is (null (script:customizations-commands (app:app-customizations application))))
      (is (null (app:app-prompt application)))
      (is (null (message-text application)))
      (is (equal "" (console-text application))))))

(test an-empty-init-file-is-fine
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init "")))
      (is (null (app:app-prompt application)))
      (is (search "Init file ran: commands 0" (console-text application))))))

(test the-init-file-is-left-alone-when-the-program-was-told-so
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init* :load-init nil)))
      (is (null (script:customizations-commands (app:app-customizations application)))))))

(test an-init-file-that-draws-is-told-it-may-not
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init "(defcommand \"First\" \"x\" 1)
(circle 0 0 5)
(defcommand \"Second\" \"y\" 2)")))
      (is (string= "The init file did not finish" (prompt-title-of application)))
      (is (search "cannot draw" (app:prompt-message (app:app-prompt application))))
      (is (search "defcommand" (app:prompt-message (app:app-prompt application))))
      (is (zerop (dom:drawing-entity-count (app:app-drawing application))))
      (is (equal '("First") (mapcar #'script:user-command-name
                                    (script:customizations-commands (app:app-customizations application))))
          "what came before the mistake stays, what came after was not run"))))

(test a-mistake-in-the-init-file-is-said-and-does-not-stop-the-program
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init "(defcommand \"Fine\" \"x\" 1)
(this-is-not-a-command 1 2)")))
      (is (string= "The init file did not finish" (prompt-title-of application)))
      (is (search "Init file:" (console-text application)))
      (app:close-prompt application)
      (is (equal '("Fine") (mapcar #'script:user-command-name
                                   (script:customizations-commands (app:app-customizations application)))))
      (press application :l)
      (is (eq :line (app:tool-kind (app:app-tool application))) "the program works as before"))))

(test an-init-file-with-unbalanced-parentheses-is-said
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init "(defcommand \"A\" \"b\" (circle 0 0 5)")))
      (is (string= "The init file did not finish" (prompt-title-of application))))))

(test the-init-file-is-run-again-on-request-and-forgets-what-it-defined-before
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init*)))
      (write-text-file (merge-pathnames "init.lisp" directory)
                       "(defcommand \"Only one\" \"x\" (circle 0 0 1))")
      (app:operation-reload-init application)
      (is (equal '("Only one") (mapcar #'script:user-command-name
                                       (script:customizations-commands (app:app-customizations application)))))
      (is (null (script:customizations-keys (app:app-customizations application))))
      (is (search "run again" (message-text application)))
      (delete-file (merge-pathnames "init.lisp" directory))
      (app:operation-reload-init application)
      (is (search "There is no init file" (message-text application)))
      (is (eq :error (cdr (app:app-message application)))))))

;;; Running commands

(test a-command-draws-with-the-commands-of-the-console-and-is-one-step-of-undo
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init*)))
      (is (app:run-command-by-name application "draw bolt"))
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
      (is (approx= 5d0 (dom:circle-radius (first (entities-of application)))))
      (is (search "1 change made" (message-text application)))
      (app:operation-undo application)
      (is (zerop (dom:drawing-entity-count (app:app-drawing application)))))))

(test what-a-command-prints-goes-to-the-console
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init*)))
      (app:run-command-by-name application "Say hello")
      (is (app:console-open-p application) "the console opens to show it")
      (is (search "hello from the init file" (console-text application))))))

(test a-command-that-fails-says-so-and-leaves-the-drawing-as-it-was
  (with-temporary-directory (directory)
    (let ((application (custom-app directory
                                   :init "(defcommand \"Half\" \"x\" (circle 0 0 5) (error \"it broke\"))")))
      (app:run-command-by-name application "Half")
      (is (zerop (dom:drawing-entity-count (app:app-drawing application))) "the circle was taken back")
      (is (eq :error (cdr (app:app-message application))))
      (is (search "it broke" (message-text application)))
      (is (search "Half: it broke" (console-text application))))))

(test a-name-that-is-nothing-is-said
  (with-temporary-directory (directory)
    (let ((application (custom-app directory)))
      (is (not (app:run-command-by-name application "Nothing like it")))
      (is (search "There is no command called Nothing like it" (message-text application))))))

(test a-key-runs-the-command-it-is-bound-to-and-a-key-bound-to-the-program-does-what-the-program-does
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init*)))
      (press application :b :control :alt)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
      (let ((grid (dom:drawing-grid-visible (app:app-drawing application))))
        (press application :g :control :alt)
        (is (not (eq grid (dom:drawing-grid-visible (app:app-drawing application)))) "a command of the program"))
      (press application :b :control)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application))) "Ctrl+B alone is not bound")
      (press application :b)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application))) "nor B"))))

(test a-key-is-not-taken-while-something-else-has-the-keyboard
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init*)))
      (app:open-text-prompt application "Name" "?" "" (lambda (text) (declare (ignore text))))
      (press application :b :control :alt)
      (is (zerop (dom:drawing-entity-count (app:app-drawing application))))
      (app:close-prompt application)
      (open-console application)
      (press application :b :control :alt)
      (is (zerop (dom:drawing-entity-count (app:app-drawing application))) "nor in the field of the console"))))

(test a-key-bound-to-a-function-runs-it
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init "(bind-key \"F6\" (lambda () (circle 0 0 7)))")))
      (press application :f6)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
      (is (approx= 7d0 (dom:circle-radius (first (entities-of application))))))))

(test a-key-bound-to-a-name-that-is-nothing-says-so
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init "(bind-key \"F6\" \"Missing\")")))
      (press application :f6)
      (is (search "There is no command called Missing" (message-text application))))))

;;; Hooks

(test hooks-run-when-the-program-has-started-and-a-drawing-is-opened
  (with-temporary-directory (directory)
    (let ((application (custom-app directory
                                   :init "(add-hook :startup (lambda () (say \"started\")))
(add-hook :drawing-opened (lambda () (layer \"notes\" :color :green)))")))
      (is (search "started" (console-text application)))
      (is (null (dom:find-layer-by-name (app:app-drawing application) "notes")))
      (app:operation-new application)
      (is (dom:find-layer-by-name (app:app-drawing application) "notes") "the hook ran on the new drawing"))))

(test hooks-run-before-and-after-a-drawing-is-saved
  (with-temporary-directory (directory)
    (let ((application (custom-app directory
                                   :init "(add-hook :before-save (lambda () (text 0 0 \"stamp\" :height 100)))
(add-hook :after-save (lambda () (say \"after save\")))"))
          (path (merge-pathnames "plan.bcad" directory)))
      (is (app:save-app-drawing application (uiop:native-namestring path)))
      (is (search "after save" (console-text application)))
      (let ((saved (persist:load-drawing path)))
        (is (= 1 (dom:drawing-entity-count saved)) "what the first hook made is in the file")
        (is (eq :text (dom:entity-kind (first (dom:drawing-entities saved)))))))))

(test a-hook-that-fails-is-said-and-the-others-still-run
  (with-temporary-directory (directory)
    (let ((application (custom-app directory
                                   :init "(add-hook :after-save (lambda () (error \"bad hook\")))
(add-hook :after-save (lambda () (say \"second hook\")))"))
          (path (merge-pathnames "plan.bcad" directory)))
      (is (app:save-app-drawing application (uiop:native-namestring path)) "saving did not fail")
      (is (probe-file path))
      (is (search "A hook of :after-save failed: bad hook" (console-text application)))
      (is (search "second hook" (console-text application))))))

(test a-hook-can-name-a-command
  (with-temporary-directory (directory)
    (let ((application (custom-app directory
                                   :init "(defcommand \"Greet\" \"x\" (say \"greetings\"))
(add-hook :startup \"Greet\")
(add-hook :drawing-opened \"Nobody\")")))
      (is (search "greetings" (console-text application)))
      (app:operation-new application)
      (is (search "There is no command called Nobody" (console-text application))))))

;;; The command palette

(test control-k-opens-the-palette-and-escape-closes-it
  (let ((application (make-test-app)))
    (open-palette application)
    (is (string= "Command palette" (prompt-title-of application)))
    (is (ui:widget-edit (find-widget application :palette-field)) "the field has the keyboard")
    (press application :escape)
    (is (null (app:app-prompt application)))))

(test the-palette-lists-what-the-program-does-by-menu
  (let ((application (make-test-app)))
    (open-palette application)
    (let ((labels (palette-labels application)))
      (is (member "Draw: Line" labels :test #'string=))
      (is (member "Modify: Rotate" labels :test #'string=))
      (is (member "Modify: Array..." labels :test #'string=))
      (is (member "Measure: Quantities..." labels :test #'string=))
      (is (member "More: Console" labels :test #'string=))
      (is (member "File: Save" labels :test #'string=))
      (is (member "View: Zoom to fit" labels :test #'string=))
      (is (member "Settings: Run the init file again" labels :test #'string=))
      (is (= (length labels) (length (remove-duplicates labels :test #'string=))) "no label twice"))
    (is (find-widget application '(:palette-row 0)))
    (is (null (find-widget application (list :palette-row app:+palette-visible-rows+))) "only a few are in view")))

(test what-is-typed-narrows-the-list-and-the-words-need-not-be-together
  (let ((application (make-test-app)))
    (open-palette application)
    (type-text application "rot")
    (is (equal '("Modify: Rotate") (palette-labels application)))
    (press application :a :control)
    (type-text application "zoom fit")
    (is (equal '("View: Zoom to fit") (palette-labels application)) "two words, in any order and place")
    (press application :a :control)
    (type-text application "FIT zoom")
    (is (equal '("View: Zoom to fit") (palette-labels application)) "in any case and any order")
    (press application :a :control)
    (type-text application "xyzzy")
    (is (null (palette-labels application)))
    (is (find-widget application :palette-empty))
    (press application :enter)
    (is (eq :palette (app:prompt-kind (app:app-prompt application))) "nothing to run: it stays")))

(test a-name-begun-by-what-is-typed-comes-before-a-name-that-has-it-inside
  (let ((application (make-test-app)))
    (open-palette application)
    (type-text application "line")
    (let ((labels (palette-labels application)))
      (is (< 1 (length labels)))
      (is (equal "Draw: Line" (first labels)) "the one whose name says line comes first"))))

(test accents-and-case-do-not-matter-in-the-palette
  (with-temporary-directory (directory)
    (let* ((name (format nil "Fa~ca a viga" (code-char 231)))
           (application (custom-app directory
                                    :init (format nil "(defcommand ~s \"Desenha uma viga\" (rect-size 0 0 100 50))"
                                                  name))))
      (open-palette application)
      (type-text application "faca VIGA")
      (is (equal (list name) (palette-labels application))))))

(test the-arrows-choose-and-go-round-and-enter-runs-what-is-chosen
  (let ((application (make-test-app)))
    (open-palette application)
    (type-text application "modify")
    (let ((count (length (palette-labels application)))
          (prompt (app:app-prompt application)))
      (is (< 5 count))
      (is (= 0 (app:prompt-active prompt)))
      (press application :down)
      (is (= 1 (app:prompt-active prompt)))
      (press application :up)
      (press application :up)
      (is (= (1- count) (app:prompt-active prompt)) "from the first up goes to the last")
      (press application :down)
      (is (= 0 (app:prompt-active prompt)) "and from the last down to the first")
      (press application :page-down)
      (is (= app:+palette-visible-rows+ (app:prompt-active prompt)))
      (is (<= (app:prompt-scroll prompt) (app:prompt-active prompt)))
      (is (< (app:prompt-active prompt) (+ (app:prompt-scroll prompt) app:+palette-visible-rows+)) "it stays in view"))
    (press application :escape)
    (open-palette application)
    (type-text application "ellipse")
    (press application :enter)
    (is (null (app:app-prompt application)) "the palette closed")
    (is (eq :ellipse (app:tool-kind (app:app-tool application))) "and the tool that was chosen is the one in use")))

(test the-wheel-scrolls-the-list-and-a-click-runs-a-command
  (let ((application (make-test-app)))
    (open-palette application)
    (let ((prompt (app:app-prompt application))
          (first-label (first (palette-labels application))))
      (send application (app:make-wheel-event -2d0 700d0 300d0))
      (is (= 2 (app:prompt-scroll prompt)))
      (send application (app:make-wheel-event 5d0 700d0 300d0))
      (is (= 0 (app:prompt-scroll prompt)) "not above the first")
      (is (equal first-label (ui:widget-label (find-widget application '(:palette-row 0))))))
    (type-text application "ellipse")
    (click-widget application '(:palette-row 0))
    (is (null (app:app-prompt application)))
    (is (eq :ellipse (app:tool-kind (app:app-tool application))))))

(test a-click-outside-the-palette-closes-it
  (let ((application (make-test-app)))
    (open-palette application)
    (send application
          (app:make-pointer-event :move nil 5d0 700d0)
          (app:make-pointer-event :down :left 5d0 700d0)
          (app:make-pointer-event :up :left 5d0 700d0))
    (is (null (app:app-prompt application)))))

(test the-commands-of-the-user-come-first-and-are-found-by-their-description
  (with-temporary-directory (directory)
    (let ((application (custom-app directory :init *bolt-init*)))
      (open-palette application)
      (is (equal '("Draw bolt" "Say hello") (subseq (palette-labels application) 0 2)))
      (type-text application "greeting")
      (is (equal '("Say hello") (palette-labels application)) "the description is searched too")
      (is (equal "Prints a greeting" (ui:widget-hint (find-widget application '(:palette-row 0)))))
      (press application :enter)
      (is (search "hello from the init file" (console-text application)))
      (open-palette application)
      (type-text application "bolt")
      (press application :enter)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application)))))))

(test the-palette-shows-the-keys-of-what-it-lists
  (let ((application (make-test-app)))
    (open-palette application)
    (type-text application "select all")
    (is (equal "Ctrl+A" (ui:widget-shortcut (find-widget application '(:palette-row 0)))))))

(test the-palette-stays-in-a-small-window
  (let ((application (make-test-app :width 500d0 :height 300d0)))
    (open-palette application)
    (let ((card (find-widget application :prompt)))
      (is (<= 0 (ui:widget-x card)))
      (is (<= (+ (ui:widget-x card) (ui:widget-width card)) 500))
      (is (<= (+ (ui:widget-y card) (ui:widget-height card)) 300)))))

;;; Recordings kept as commands

(defun record-a-circle (application)
  "Record the drawing of a circle of radius 12 in APPLICATION and stop, which asks what to do with the recording.

APPLICATION: the application."
  (when (app:console-open-p application)
    (open-console application))
  (open-console application)
  (app:operation-start-recording application)
  (run-command application "(circle 0 0 12)")
  (app:operation-stop-recording application))

(test a-recording-can-be-kept-as-a-command-that-the-palette-runs
  (with-temporary-directory (directory)
    (let ((application (custom-app directory)))
      (record-a-circle application)
      (is (string= "Recording ready" (prompt-title-of application)))
      (choose-in-prompt application 0)
      (is (string= "Keep as a command" (prompt-title-of application)))
      (is (eq :text (app:prompt-kind (app:app-prompt application))))
      (type-text application "Draw a ring")
      (press application :enter)
      (is (string= "Command saved" (prompt-title-of application)))
      (is (probe-file (merge-pathnames "commands/Draw a ring.lisp" directory)))
      (is (equal '("Draw a ring") (app:app-recorded-commands application)))
      (app:close-prompt application)
      (app:operation-undo application)
      (is (zerop (dom:drawing-entity-count (app:app-drawing application))))
      (open-palette application)
      (type-text application "ring")
      (is (equal '("Recorded: Draw a ring") (palette-labels application)))
      (press application :enter)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
      (is (approx= 12d0 (dom:circle-radius (first (entities-of application)))))
      (app:operation-undo application)
      (is (zerop (dom:drawing-entity-count (app:app-drawing application))) "the command is one step of undo"))))

(test a-command-kept-from-a-recording-is-found-when-the-program-starts-again
  (with-temporary-directory (directory)
    (let ((application (custom-app directory)))
      (record-a-circle application)
      (choose-in-prompt application 0)
      (type-text application "Ring")
      (press application :enter))
    (let ((again (custom-app directory)))
      (is (equal '("Ring") (app:app-recorded-commands again)))
      (is (app:run-command-by-name again "ring") "by its name, in any case")
      (is (= 1 (dom:drawing-entity-count (app:app-drawing again)))))))

(test a-key-can-be-bound-to-a-recorded-command
  (with-temporary-directory (directory)
    (let ((first (custom-app directory)))
      (record-a-circle first)
      (choose-in-prompt first 0)
      (type-text first "Ring")
      (press first :enter))
    (let ((again (custom-app directory :init "(bind-key \"F6\" \"Ring\")")))
      (press again :f6)
      (is (= 1 (dom:drawing-entity-count (app:app-drawing again)))))))

(test a-name-already-used-or-of-no-use-is-asked-again
  (with-temporary-directory (directory)
    (let ((application (custom-app directory)))
      (record-a-circle application)
      (choose-in-prompt application 0)
      (type-text application "Ring")
      (press application :enter)
      (app:close-prompt application)
      (record-a-circle application)
      (choose-in-prompt application 0)
      (type-text application "ring")
      (press application :enter)
      (is (eq :text (app:prompt-kind (app:app-prompt application))) "asked again")
      (is (search "already exists" (app:prompt-message (app:app-prompt application))))
      (press application :a :control)
      (type-text application "///")
      (press application :enter)
      (is (search "needs a name" (app:prompt-message (app:app-prompt application))) "nothing is left of the name")
      (press application :a :control)
      (type-text application "a/b:c")
      (press application :enter)
      (is (string= "Command saved" (prompt-title-of application)))
      (is (probe-file (merge-pathnames "commands/a-b-c.lisp" directory))
          "what a file name cannot have is made a hyphen"))))

(test a-recording-can-still-be-saved-as-a-script-or-thrown-away
  (with-temporary-directory (directory)
    (let ((application (custom-app directory)))
      (record-a-circle application)
      (choose-in-prompt application 1)
      (is (string= "Save the recorded actions as a script" (dialog-title-text application)))
      (app:close-file-dialog application))
    (let ((application (custom-app directory)))
      (record-a-circle application)
      (choose-in-prompt application 2)
      (is (null (app:app-prompt application)))
      (is (null (app:app-dialog application)))
      (is (null (app:app-recorded-commands application))))))

(test a-recording-that-changes-what-was-there-says-it-only-works-where-that-is-the-same
  (with-temporary-directory (directory)
    (let ((application (custom-app directory)))
      (open-console application)
      (run-command application "(circle 0 0 10)")
      (app:operation-start-recording application)
      (run-command application "(move 1 5 5)")
      (app:operation-stop-recording application)
      (choose-in-prompt application 0)
      (type-text application "Nudge")
      (press application :enter)
      (is (search "already in the drawing" (app:prompt-message (app:app-prompt application)))))
    (let ((application (custom-app directory)))
      (record-a-circle application)
      (choose-in-prompt application 0)
      (type-text application "Plain")
      (press application :enter)
      (is (not (search "already in the drawing" (app:prompt-message (app:app-prompt application))))))))

(test a-recording-needs-a-folder-for-commands
  (let ((application (make-test-app)))
    (record-a-circle application)
    (choose-in-prompt application 0)
    (type-text application "Ring")
    (press application :enter)
    (is (eq :text (app:prompt-kind (app:app-prompt application))) "asked again")
    (is (search "no folder for commands" (app:prompt-message (app:app-prompt application))))))

(test the-name-of-a-command-becomes-a-name-of-a-file
  (is (equal "Draw a ring" (app:command-file-name "  Draw a ring  ")))
  (is (equal "a-b-c-d" (app:command-file-name "a/b:c*d")))
  (is (equal "x" (app:command-file-name "..x..")))
  (signals base:edit-refused (app:command-file-name "   "))
  (signals base:edit-refused (app:command-file-name (make-string 65 :initial-element #\a))))

;;; The example that comes with the program

(test the-example-init-file-runs-and-its-commands-work
  (with-temporary-directory (directory)
    (let ((application (custom-app directory
                                   :init (persist:read-file-text
                                          (asdf:system-relative-pathname "borba-cad" "examples/init.lisp")))))
      (is (null (app:app-prompt application)) "it ran without a complaint")
      (is (equal '("Draw a fence post" "Draw a row of posts" "How many things?")
                 (command-names application)))
      (is (search "Init file ready" (console-text application)))
      (is (equal '("grade") (mapcar #'script:block-generator-name (app:block-generators application))))
      (press application :p :control :alt)
      (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
      (app:run-command-by-name application "Draw a row of posts")
      (is (= 7 (dom:drawing-entity-count (app:app-drawing application))))
      (is (dom:find-layer-by-name (app:app-drawing application) "posts"))
      (app:run-command-by-name application "how many things?")
      (is (search "7 entities" (console-text application)))
      (app:operation-new application)
      (when (app:app-prompt application)
        (choose-in-prompt application 1))
      (is (dom:find-layer-by-name (app:app-drawing application) "notes") "the hook made the layer"))))
