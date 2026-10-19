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

(defstruct (customizations (:constructor make-customizations ()))
  "What the user's own files defined.

COMMANDS: the USER-COMMANDs, in the order they were defined.
KEYS: list of (key modifiers target), the target being the name of a command or a function.
HOOKS: list of (event . functions), each function being a function of no arguments or the name of a command."
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
