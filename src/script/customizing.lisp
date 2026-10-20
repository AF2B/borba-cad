(in-package #:borba-cad/script)

;;;; What the user makes the program do: commands that they define, keys that they bind to them, and functions that
;;;; run when something happens (a drawing is opened, saved). They are written in the init file, which is Lisp like
;;;; everything else the user types, and the definitions are kept in a CUSTOMIZATIONS that the application owns and
;;;; binds while it runs the file. A command is a function of no arguments: it draws and edits with the same commands
;;;; as the console, and is one step of undo.

(defparameter *hook-events* '(:startup :drawing-opened :before-save :after-save)
  "What a hook can be added to, and when it runs: when the program has started, after a drawing is opened or a new one
is made, before a drawing is saved and after it is saved.")

(defparameter *function-key-names* '("f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12")
  "The function keys, as they are written in a key binding.")

(defparameter *named-key-names*
  '(("delete" . :delete) ("del" . :delete) ("home" . :home) ("end" . :end) ("pageup" . :page-up)
    ("pagedown" . :page-down) ("left" . :left) ("right" . :right) ("up" . :up) ("down" . :down))
  "The keys besides letters and function keys that a binding may use, with the names that they go by.")

(defparameter *reserved-key-names* '("escape" "esc" "enter" "return" "tab" "space" "backspace")
  "Keys that a binding may not take, because the program needs them as they are.")

(defparameter *modifier-names*
  '(("ctrl" . :control) ("control" . :control) ("shift" . :shift) ("alt" . :alt))
  "The modifier keys, as they are written in a key binding.")

(defstruct (user-command (:constructor make-user-command (name
                                                          description
                                                          function)))
  "A command that the user defined.

NAME: what it is called, with the case the user gave it.
DESCRIPTION: what it does, in a sentence.
FUNCTION: function of no arguments that does it."
  name description function)

(defstruct (block-generator (:constructor make-block-generator (name
                                                                description
                                                                parameters
                                                                function)))
  "The code that draws a parametric block.

NAME: what it is called, a string; the name of the parameter of a block is that of the generator that drew it.
DESCRIPTION: what it draws, in a sentence.
PARAMETERS: list of (name . default), each name a string and each default a number, or NIL when it has none.
FUNCTION: function of one number for each parameter that draws the block, with the commands of the console."
  name description parameters function)

(defstruct (customizations (:constructor make-customizations ()))
  "What the user's own files defined.

GENERATORS: the BLOCK-GENERATORs, in the order they were defined.
COMMANDS: the USER-COMMANDs, in the order they were defined.
KEYS: list of (key modifiers target), the target being the name of a command or a function.
HOOKS: list of (event . functions), each function being a function of no arguments or the name of a command."
  (generators '() :type list)
  (commands '() :type list)
  (keys '() :type list)
  (hooks '() :type list))

(defvar *customizations* nil
  "The customizations that the definitions being run are added to.")

(defun current-customizations ()
  "Return the customizations that a definition is added to, or signal an error when there are none."
  (or *customizations*
      (error "Commands, key bindings and hooks can only be defined where the program is running.")))

(defun command-key (name)
  "Return what the names of commands are compared by: without blanks around them and without case.

NAME: the name of a command."
  (string-downcase (string-trim '(#\Space #\Tab) name)))

(defun find-user-command (customizations
                          name)
  "Return the command with a name, or NIL.

CUSTOMIZATIONS: what the user defined.
NAME: the name of the command, in any case."
  (find (command-key name)
        (customizations-commands customizations)
        :key (lambda (command) (command-key (user-command-name command)))
        :test #'string=))

(defun register-command (name
                         description
                         function)
  "Add a command, replacing one of the same name. Returns its name.

NAME: what it is called.
DESCRIPTION: what it does, in a sentence.
FUNCTION: function of no arguments that does it."
  (unless (and (stringp name) (plusp (length (string-trim '(#\Space #\Tab) name))))
    (error "A command needs a name, a string, but ~s was given." name))
  (unless (stringp description)
    (error "A command needs a description, a string, but ~s was given." description))
  (let* ((customizations (current-customizations))
         (command (make-user-command (string-trim '(#\Space #\Tab) name)
                                     description
                                     function))
         (existing (find-user-command customizations name)))
    (if existing
        (setf (customizations-commands customizations)
              (substitute command existing (customizations-commands customizations)))
        (setf (customizations-commands customizations)
              (append (customizations-commands customizations) (list command))))
    (user-command-name command)))

(defmacro defcommand (name
                      description
                      &body body)
  "Define a command that is run from the command palette (Ctrl+K) or from a key bound to it. It takes no arguments, does
what BODY does, which can draw and edit with any command, and is one step of undo. Returns its name.

NAME: what it is called, a string.
DESCRIPTION: what it does, a string.
BODY: the forms it runs."
  `(register-command ,name
                     ,description
                     (lambda () ,@body)))

;;; Keys

(defun parse-chord (text)
  "Return the key and the modifiers that a key binding names, as two values: the keyword of the key and the list of
:control, :shift and :alt. The text is the modifiers and then the key, joined by +, in any case: Ctrl+Alt+H,
Shift+F5, F7, Ctrl+Up. A key may be a letter, F1 to F12, Delete, Home, End, PageUp, PageDown or an arrow; the keys
that the program needs as they are (Escape, Enter, Tab, Space, Backspace) may not be bound. A letter needs Ctrl or
Alt, because it is what the user types otherwise. Signals an error when the text is none of that.

TEXT: a key binding as it is written, such as \"Ctrl+Shift+K\"."
  (let* ((parts (mapcar (lambda (part) (string-downcase (string-trim '(#\Space) part)))
                        (uiop:split-string text :separator '(#\+))))
         (key-name (car (last parts)))
         (modifiers '()))
    (dolist (name (butlast parts))
      (let ((modifier (cdr (assoc name *modifier-names* :test #'string=))))
        (unless modifier
          (error "~s is not a key binding: ~s is not Ctrl, Shift or Alt." text name))
        (pushnew modifier modifiers)))
    (setf modifiers (sort modifiers #'string< :key #'symbol-name))
    (let ((key (cond ((member key-name *reserved-key-names* :test #'string=)
                      (error "~s is not a key binding: the program needs ~a as it is." text key-name))
                     ((and (= 1 (length key-name)) (char<= #\a (char key-name 0) #\z))
                      (intern (string-upcase key-name) :keyword))
                     ((member key-name *function-key-names* :test #'string=)
                      (intern (string-upcase key-name) :keyword))
                     ((cdr (assoc key-name *named-key-names* :test #'string=)))
                     (t (error "~s is not a key binding: ~s is not a key. Use a letter, F1 to F12, Delete, Home, End, ~
PageUp, PageDown or an arrow."
                               text
                               key-name)))))
      (when (and (null modifiers) (not (member key-name *function-key-names* :test #'string=)))
        (error "~s is not a key binding: ~a alone is what the program uses to type and to choose tools; ~
add Ctrl, Alt or Shift."
               text
               key-name))
      (when (and (equal modifiers '(:shift)) (= 1 (length key-name)))
        (error "~s is not a key binding: Shift and a letter choose the tools of the program; add Ctrl or Alt." text))
      (values key modifiers))))

(defun chord-text (key
                   modifiers)
  "Return a key binding as it is written: Ctrl+Alt+H.

KEY: the keyword of the key.
MODIFIERS: the list of :control, :shift and :alt held with it."
  (format nil "~{~a+~}~a"
          (loop for (modifier . name) in '((:control . "Ctrl") (:alt . "Alt") (:shift . "Shift"))
                when (member modifier modifiers)
                  collect name)
          (let ((name (symbol-name key)))
            (cond ((and (= 1 (length name)) (alpha-char-p (char name 0))) name)
                  ((char= #\F (char name 0)) name)
                  (t (string-capitalize (substitute #\Space #\- name)))))))

(defun bind-key (chord
                 target)
  "Bind a key to a command, replacing what it was bound to. The command may be defined after the binding. Returns the
binding as it is written.

CHORD: the key and its modifiers written with +, such as \"Ctrl+Alt+H\" or \"F7\".
TARGET: the name of a command that was defined with DEFCOMMAND, or one of the program (as the command palette lists
it), or a function of no arguments."
  (unless (or (stringp target) (functionp target))
    (error "A key is bound to the name of a command or to a function, but ~s was given." target))
  (multiple-value-bind (key modifiers) (parse-chord chord)
    (let ((customizations (current-customizations)))
      (setf (customizations-keys customizations)
            (cons (list key modifiers target)
                  (remove-if (lambda (binding) (and (eq (first binding) key)
                                                    (equal (second binding) modifiers)))
                             (customizations-keys customizations))))
      (chord-text key modifiers))))

(defun find-key-binding (customizations
                         key
                         modifiers)
  "Return the command or function that a key is bound to, or NIL.

CUSTOMIZATIONS: what the user defined.
KEY: the keyword of the key that was pressed.
MODIFIERS: the modifier keywords that were held."
  (let ((wanted (sort (remove-duplicates (remove-if-not (lambda (modifier) (member modifier '(:control :shift :alt)))
                                                        modifiers))
                      #'string<
                      :key #'symbol-name)))
    (third (find-if (lambda (binding) (and (eq (first binding) key)
                                           (equal (second binding) wanted)))
                    (customizations-keys customizations)))))

;;; Hooks

(defun add-hook (event
                 function)
  "Make a function run when something happens: :startup, when the program has started; :drawing-opened, after a drawing
is opened or a new one is made; :before-save, before a drawing is saved; :after-save, after it is saved. Each runs as
a command of its own, so what it changes is a step of undo, and a hook that fails says so and does no harm. Returns
the event.

EVENT: one of :startup, :drawing-opened, :before-save and :after-save.
FUNCTION: function of no arguments, or the name of a command."
  (unless (member event *hook-events*)
    (error "~s is not something a hook can be added to; use one of ~{~s~^, ~}." event *hook-events*))
  (unless (or (functionp function) (stringp function))
    (error "A hook is a function of no arguments or the name of a command, but ~s was given." function))
  (let* ((customizations (current-customizations))
         (entry (assoc event (customizations-hooks customizations))))
    (if entry
        (setf (cdr entry) (append (cdr entry) (list function)))
        (setf (customizations-hooks customizations)
              (append (customizations-hooks customizations) (list (list event function)))))
    event))

(defun hook-functions (customizations
                       event)
  "Return what has been added to an event, in the order it was added.

CUSTOMIZATIONS: what the user defined.
EVENT: the event."
  (cdr (assoc event (customizations-hooks customizations))))

;;; Looking at what is defined

(defun commands ()
  "Return the names of the commands that were defined with DEFCOMMAND, in the order they were defined."
  (mapcar #'user-command-name (customizations-commands (current-customizations))))

(defun bindings ()
  "Return the key bindings that were made, as a list of (key command): the key as it is written, and the name of the
command or a function."
  (mapcar (lambda (binding)
            (list (chord-text (first binding) (second binding))
                  (third binding)))
          (customizations-keys (current-customizations))))

;;; Blocks that are drawn by code

(defparameter *generator-timeout-seconds* 10d0
  "Longest a generator may take to draw a block.")

(defconstant +default-parameter-value+ 100d0
  "What a parameter that has no default is given when a form asks for it before the user has typed anything.")

(defun find-block-generator (customizations
                             name)
  "Return the generator of that name, or NIL.

CUSTOMIZATIONS: what the user defined.
NAME: the name of the generator, in any case."
  (find (command-key name)
        (customizations-generators customizations)
        :key (lambda (generator) (command-key (block-generator-name generator)))
        :test #'string=))

(defun register-block-generator (name
                                 description
                                 parameters
                                 function)
  "Add a generator, replacing one of the same name. Returns its name.

NAME: what it is called.
DESCRIPTION: what it draws.
PARAMETERS: list of (name . default).
FUNCTION: function of one number for each parameter."
  (unless (and (stringp name) (plusp (length (string-trim '(#\Space #\Tab) name))))
    (error "A block needs a name, but ~s was given." name))
  (let* ((customizations (current-customizations))
         (generator (make-block-generator (string-trim '(#\Space #\Tab) name)
                                          description
                                          parameters
                                          function))
         (existing (find-block-generator customizations name)))
    (if existing
        (setf (customizations-generators customizations)
              (substitute generator existing (customizations-generators customizations)))
        (setf (customizations-generators customizations)
              (append (customizations-generators customizations) (list generator))))
    (block-generator-name generator)))

(defmacro defblock (name
                    (&rest parameters)
                    &body body)
  "Define a parametric block: a block that BODY draws, with the commands of the console, from the numbers that its
parameters have. It is drawn when it is placed, and again when a parameter is changed in the inspector. A parameter is a
name, or a list of a name and the number it has unless it is given another. The name of the block is a symbol or a
string. Returns its name.

NAME: what the block is called.
PARAMETERS: the names of the parameters, each with its default when it is written as a list.
BODY: a description of what it draws, as a string, if there is more than one form, and then the forms that draw it."
  (let* ((text (and (stringp (first body)) (rest body) (first body)))
         (forms (if text (rest body) body))
         (variables (mapcar (lambda (parameter) (if (consp parameter) (first parameter) parameter)) parameters)))
    `(register-block-generator ,(if (stringp name) name (string-downcase (symbol-name name)))
                               ,(or text "")
                               (list ,@(mapcar (lambda (parameter)
                                                 `(cons ,(string-downcase (symbol-name (if (consp parameter)
                                                                                           (first parameter)
                                                                                           parameter)))
                                                        ,(and (consp parameter) `(float ,(second parameter) 1d0))))
                                               parameters))
                               (lambda (,@variables)
                                 (declare (ignorable ,@variables))
                                 ,@forms))))

(defun tidy-number (value)
  "Return a number as short text, without zeros at the end of its decimals.

VALUE: a number."
  (let ((text (base:format-decimal value 4)))
    (if (find #\. text)
        (string-right-trim "." (string-right-trim "0" text))
        text)))

(defun generated-block-name (generator-name
                             numbers)
  "Return the name of the block that a generator draws from some numbers: the name of the generator and the numbers,
as in grade(1200, 900, 100).

GENERATOR-NAME: the name of the generator.
NUMBERS: the number of each parameter."
  (let* ((arguments (format nil "~{~a~^, ~}" (mapcar #'tidy-number numbers)))
         (name (format nil "~a(~a)" generator-name arguments)))
    (if (<= (length name) dom:+maximum-block-name-length+)
        name
        (format nil "~a#~36r"
                (subseq generator-name 0 (min (length generator-name) 40))
                (sxhash arguments)))))

(defun generation-failure (generator
                           reason)
  "Signal EDIT-REFUSED saying that a block could not be drawn.

GENERATOR: the generator.
REASON: why, as text."
  (error 'base:edit-refused
         :reason (i18n:trf "The block ~a could not be drawn: ~a"
                           (block-generator-name generator)
                           reason)))

(defun generate-block-definition (generator
                                  numbers)
  "Return the block definition that a generator draws from some numbers, which is not in any drawing. Signals
EDIT-REFUSED, with the reason, when the numbers are not right, the generator fails, draws nothing, or takes too long.

GENERATOR: the generator.
NUMBERS: a number for each of its parameters, in order."
  (unless (= (length numbers) (length (block-generator-parameters generator)))
    (generation-failure generator
                        (i18n:trf "it takes ~d ~:*~[numbers~;number~:;numbers~], but ~d were given"
                                  (length (block-generator-parameters generator))
                                  (length numbers))))
  (unless (every (lambda (number) (and (realp number) (< (abs number) base:+maximum-coordinate-magnitude+))) numbers)
    (generation-failure generator (i18n:tr "its numbers must be numbers of a reasonable size")))
  (let* ((numbers (mapcar (lambda (number) (float number 1d0)) numbers))
         (scratch (dom:make-drawing))
         (collector (list nil)))
    (handler-case
        (let ((*context* (make-script-context scratch))
              (*block-collector* collector)
              (*redraw* nil))
          (sbcl:call-with-timeout *generator-timeout-seconds*
                                  (lambda () (apply (block-generator-function generator) numbers))))
      (sbcl:timeout ()
        (generation-failure generator
                            (i18n:trf "it took more than ~d seconds" (round *generator-timeout-seconds*))))
      (base:edit-refused (condition)
        (error condition))
      (error (condition)
        (generation-failure generator (failure-message condition))))
    (when (null (car collector))
      (generation-failure generator (i18n:tr "it drew nothing")))
    (handler-case (dom:make-block-definition
                   :name (generated-block-name (block-generator-name generator) numbers)
                   :entities (reverse (car collector))
                   :origin (dom:make-block-origin (block-generator-name generator)
                                                  (loop for (name) in (block-generator-parameters generator)
                                                        for number in numbers
                                                        collect (cons name number))))
      (base:borba-cad-error (condition)
        (generation-failure generator (princ-to-string condition))))))

(defun same-origin-p (first
                      second)
  "Return true when two block origins are of the same generator with the same numbers.

FIRST: a block origin, or NIL.
SECOND: another."
  (and first
       second
       (string-equal (dom:block-origin-generator first) (dom:block-origin-generator second))
       (equalp (dom:block-origin-arguments first) (dom:block-origin-arguments second))))

(defun parametric-block (drawing
                         generator
                         numbers)
  "Return the block of the drawing that a generator draws from some numbers, drawing it and adding it to the drawing
when the drawing has none like it.

DRAWING: the drawing.
GENERATOR: the generator.
NUMBERS: a number for each of its parameters, in order."
  (let* ((definition (generate-block-definition generator numbers))
         (existing (dom:find-block drawing (dom:block-definition-name definition))))
    (cond ((and existing (same-origin-p (dom:block-definition-origin existing)
                                        (dom:block-definition-origin definition)))
           existing)
          (t
           (dom:define-block drawing definition)
           definition))))

(defun argument-numbers (generator
                         given)
  "Return the number of each parameter of a generator: what GIVEN says for it, or its default. GIVEN is a list of
numbers in the order of the parameters, or a list of names and numbers; signals an error saying what is wrong.

GENERATOR: the generator.
GIVEN: the numbers, or names and numbers."
  (let ((parameters (block-generator-parameters generator)))
    (cond ((and (listp given) (every #'realp given) given)
           (when (> (length given) (length parameters))
             (error "The block ~a has ~d ~:*~[parameters~;parameter~:;parameters~], but ~d numbers were given."
                    (block-generator-name generator)
                    (length parameters)
                    (length given)))
           (loop for (name . default) in parameters
                 for index from 0
                 collect (or (and (< index (length given)) (nth index given))
                             default
                             (error "The parameter ~a of the block ~a needs a number."
                                    name
                                    (block-generator-name generator)))))
          ((and (listp given) (evenp (length given)))
           (let ((named (loop for (key value) on given by #'cddr
                              collect (cons (string-downcase (if (stringp key) key (symbol-name key))) value))))
             (dolist (entry named)
               (unless (assoc (car entry) parameters :test #'string=)
                 (error "The block ~a has no parameter called ~a; it has ~{~a~^, ~}."
                        (block-generator-name generator)
                        (car entry)
                        (mapcar #'car parameters))))
             (loop for (name . default) in parameters
                   collect (or (cdr (assoc name named :test #'string=))
                               default
                               (error "The parameter ~a of the block ~a needs a number."
                                      name
                                      (block-generator-name generator))))))
          (t (error "A block is given its numbers as a list of numbers, or of names and numbers, but ~s was given."
                    given)))))

(defun parameter-changes (drawing
                          id
                          parameter
                          value)
  "Return the change set that gives a parameter of a parametric block, as it is placed, another number, which draws the
block again: the placement takes the new block. Signals EDIT-REFUSED, with the reason, when the placement is not
one of a parametric block, the generator is not defined, or the block cannot be drawn.

DRAWING: the drawing.
ID: the id of the placement.
PARAMETER: the name of the parameter.
VALUE: its new number."
  (let* ((insert (dom:store-find (dom:drawing-store drawing) id))
         (origin (and (typep insert 'dom:insert-entity)
                      (dom:block-definition-origin (dom:insert-block insert)))))
    (unless origin
      (error 'base:edit-refused :reason (i18n:tr "That is not a block that a generator drew.")))
    (unless (assoc parameter (dom:block-origin-arguments origin) :test #'string-equal)
      (error 'base:edit-refused :reason (i18n:trf "The block has no parameter called ~a." parameter)))
    (let ((generator (find-block-generator (current-customizations) (dom:block-origin-generator origin))))
      (unless generator
        (error 'base:edit-refused
               :reason (i18n:trf "The generator ~a is not defined here; define it in the init file to draw it again."
                                 (dom:block-origin-generator origin))))
      (unless (and (realp value) (< (abs value) base:+maximum-coordinate-magnitude+))
        (error 'base:edit-refused :reason (i18n:tr "A parameter is a number of a reasonable size.")))
      (let* ((numbers (loop for (name . number) in (dom:block-origin-arguments origin)
                            collect (if (string-equal name parameter) (float value 1d0) number)))
             (block (parametric-block drawing
                                      generator
                                      numbers)))
        (if (eq block (dom:insert-block insert))
            '()
            (list (dom:make-change id
                                   insert
                                   (dom:update-insert insert :block block))))))))

(defun generators ()
  "Return the generators of parametric blocks that were defined, each as a list of its name, what it draws and its
parameters, each a name or a list of a name and its default."
  (loop for generator in (customizations-generators (current-customizations))
        collect (list (block-generator-name generator)
                      (block-generator-description generator)
                      (loop for (name . default) in (block-generator-parameters generator)
                            collect (if default (list name default) name)))))
