(in-package #:borba-cad/app)

;;;; The user's own configuration: the init file, which is Lisp that defines commands, key bindings and hooks, and the
;;;; folder of commands that recordings were saved as. Both are in the configuration directory and are the user's own,
;;;; like the rc file of a shell, so they are run without asking; the program can be started without the init file
;;;; (--no-init) if a mistake in it gets in the way, and a mistake is said, never fatal. The init file can define but
;;;; not draw: what draws is a command, run when it is called.

(defparameter *init-file-name* "init.lisp"
  "Name of the file in the configuration directory that is run when the program starts.")

(defparameter *commands-folder-name* "commands/"
  "Name of the folder in the configuration directory that holds the commands saved from recordings.")

(defparameter *command-file-extension* "lisp"
  "Extension of a file that holds a command saved from a recording.")

(defconstant +maximum-command-name-length+ 64
  "Longest name a command saved from a recording may have.")

(defparameter *forbidden-file-name-characters* "/\\:*?\"<>|"
  "Characters that a file name may not have on some systems, which a command's name is cleaned of.")

(defun config-file-path (app
                         name)
  "Return a path inside the configuration directory, or NIL when there is no such directory.

APP: the application.
NAME: the name of the file or folder."
  (let ((directory (base:config-config-directory (app-config app))))
    (and directory
         (merge-pathnames name directory))))

(defun init-file-path (app)
  "Return the path of the init file, or NIL when there is no configuration directory.

APP: the application."
  (config-file-path app *init-file-name*))

(defun commands-directory (app)
  "Return the folder of the commands saved from recordings, or NIL when there is no configuration directory.

APP: the application."
  (config-file-path app *commands-folder-name*))

;;; The commands saved from recordings

(defun command-file-name (name)
  "Return the name of the file that a command called NAME is kept in, without the extension: the name, trimmed and
with the characters that a file name may not have turned into hyphens. Signals EDIT-REFUSED when nothing is left or
it is too long.

NAME: what the user typed."
  (let ((cleaned (string-trim " .-"
                              (map 'string
                                   (lambda (character)
                                     (if (or (find character *forbidden-file-name-characters*)
                                             (< (char-code character) 32))
                                         #\-
                                         character))
                                   (string-trim " " name)))))
    (when (zerop (length cleaned))
      (error 'base:edit-refused :reason (i18n:tr "A command needs a name.")))
    (when (> (length cleaned) +maximum-command-name-length+)
      (error 'base:edit-refused
             :reason (i18n:trf "A command name has at most ~d characters." +maximum-command-name-length+)))
    cleaned))

(defun recorded-command-path (app
                              name)
  "Return the path of the file that a command saved from a recording is kept in.

APP: the application.
NAME: the name of the command."
  (merge-pathnames (make-pathname :name name
                                  :type *command-file-extension*)
                   (commands-directory app)))

(defun scan-recorded-commands (app)
  "Read the names of the commands saved from recordings from their folder, in alphabetical order, and keep them.

APP: the application."
  (let ((directory (commands-directory app)))
    (setf (app-recorded-commands app)
          (and directory
               (probe-file directory)
               (sort (loop for file in (uiop:directory-files directory)
                           when (string-equal (pathname-type file) *command-file-extension*)
                             collect (pathname-name file))
                     #'string-lessp)))))

(defun find-recorded-command (app
                              name)
  "Return the name of the command saved from a recording that is called NAME, with the case it was saved in, or NIL.

APP: the application.
NAME: the name, in any case."
  (find name
        (app-recorded-commands app)
        :test #'string-equal))

;;; Telling what came of a command

(defun show-command-result (app
                            name
                            result
                            changed)
  "Tell what came of running a command: what it printed goes to the console, which opens for it; a failure is said in
the console and in the status bar, and a command that changed nothing says it ran.

APP: the application.
NAME: the name of the command.
RESULT: the SCRIPT-RESULT.
CHANGED: how many entities it changed."
  (let ((output (script:script-result-output result)))
    (when (plusp (length output))
      (set-console-open app t)
      (console-say app
                   :output
                   output)))
  (case (script:script-result-status result)
    (:ok
     (when (zerop changed)
       (set-message app
                    (i18n:trf "Ran ~a." name))))
    (t
     (console-say app
                  :error
                  (format nil "~a: ~a" name (script:script-result-message result)))
     (set-message app
                  (i18n:trf "~a did not finish: ~a"
                            name
                            (script:script-result-message result))
                  :error))))

(defun run-user-command (app
                         command)
  "Run a command that the init file or the console defined, as one step of undo.

APP: the application.
COMMAND: the USER-COMMAND."
  (multiple-value-bind (result changed) (run-function app
                                                      (script:user-command-function command)
                                                      :event "command.run")
    (show-command-result app
                         (script:user-command-name command)
                         result
                         changed)))

(defun run-recorded-command (app
                             name)
  "Run a command that was saved from a recording, as one step of undo.

APP: the application.
NAME: the name of the command."
  (handler-case
      (let ((text (read-script-file (recorded-command-path app name))))
        (multiple-value-bind (result changed) (run-evaluation app
                                                              (lambda (context)
                                                                (script:evaluate-source context text))
                                                              "command.run")
          (show-command-result app
                               name
                               result
                               changed)))
    (base:drawing-file-error (condition)
      (console-say app
                   :error
                   (format nil "~a: ~a" name condition))
      (set-message app
                   (remove #\Newline (princ-to-string condition))
                   :error))))

(defun run-command-by-name (app
                            name)
  "Run the command called NAME: one that was defined, one saved from a recording, or one of the program as the command
palette lists it. Returns true when there is one, and says so when there is not.

APP: the application.
NAME: what it is called, in any case."
  (let ((command (script:find-user-command (app-customizations app) name))
        (recorded (find-recorded-command app name))
        (entry (find-palette-entry app name)))
    (cond (command
           (run-user-command app command)
           t)
          (recorded
           (run-recorded-command app recorded)
           t)
          (entry
           (funcall (palette-entry-action entry))
           t)
          (t
           (set-message app
                        (i18n:trf "There is no command called ~a." name)
                        :error)
           nil))))

;;; Keys

(defun run-key-target (app
                       target)
  "Run what a key was bound to: a function, or the name of a command.

APP: the application.
TARGET: a function of no arguments or a string."
  (if (functionp target)
      (multiple-value-bind (result changed) (run-function app
                                                          target
                                                          :event "key.run")
        (show-command-result app
                             (i18n:tr "The key")
                             result
                             changed))
      (run-command-by-name app target)))

(defun run-user-key (app
                     key
                     modifiers)
  "Run what the user bound a key to. Returns true when the key was bound.

APP: the application.
KEY: the key keyword.
MODIFIERS: the modifier keys held."
  (let ((target (script:find-key-binding (app-customizations app)
                                         key
                                         modifiers)))
    (when target
      (run-key-target app target)
      t)))

;;; Hooks

(defun run-hooks (app
                  event)
  "Run what was added to an event, each as a command of its own. A hook that fails is said in the console and the
status bar, and the others still run.

APP: the application.
EVENT: :startup, :drawing-opened, :before-save or :after-save."
  (dolist (target (script:hook-functions (app-customizations app) event))
    (let ((function (cond ((functionp target) target)
                          ((script:find-user-command (app-customizations app) target)
                           (script:user-command-function (script:find-user-command (app-customizations app)
                                                                                    target))))))
      (if function
          (multiple-value-bind (result changed) (run-function app
                                                              function
                                                              :event (format nil "hook.~(~a~)" event))
            (declare (ignore changed))
            (let ((output (script:script-result-output result)))
              (when (plusp (length output))
                (console-say app
                             :output
                             output)))
            (unless (eq (script:script-result-status result) :ok)
              (report-hook-failure app
                                   event
                                   (script:script-result-message result))))
          (report-hook-failure app
                               event
                               (i18n:trf "There is no command called ~a." target))))))

(defun report-hook-failure (app
                            event
                            message)
  "Say that a hook did not run to the end.

APP: the application.
EVENT: the event it was added to.
MESSAGE: what went wrong."
  (let ((text (i18n:trf "A hook of ~(~s~) failed: ~a"
                        event
                        message)))
    (console-say app
                 :error
                 text)
    (set-message app
                 text
                 :error)))

;;; The init file

(defun init-failure-message (message)
  "Return what went wrong when the init file was run, in words that say what the init file may do.

MESSAGE: what the evaluator said."
  (if (search "at most 0 changes" message)
      (i18n:tr "The init file defines commands, keys and hooks; it cannot draw. Draw inside a command (defcommand).")
      message))

(defun run-init-text (app
                      text)
  "Run the text of the init file, which may define but not draw, and say what it defined or why it stopped.

APP: the application.
TEXT: the Lisp source of the init file."
  (let ((result (let ((script:*maximum-changes* 0))
                  (run-source app
                              text
                              :timeout script:+script-timeout-seconds+
                              :event "init.run")))
        (customizations (app-customizations app)))
    (let ((output (script:script-result-output result)))
      (when (plusp (length output))
        (console-say app
                     :output
                     output)))
    (cond ((eq (script:script-result-status result) :ok)
           (console-say app
                        :note
                        (i18n:trf "Init file ran: commands ~d, keys ~d, hooks ~d."
                                  (length (script:customizations-commands customizations))
                                  (length (script:customizations-keys customizations))
                                  (length (script:customizations-hooks customizations))))
           t)
          (t
           (let ((message (init-failure-message (script:script-result-message result))))
             (console-say app
                          :error
                          (i18n:trf "Init file: ~a" message))
             (show-notice app
                          (i18n:tr "The init file did not finish")
                          message
                          +report-message-lines+))
           nil))))

(defun load-init-file (app)
  "Run the init file, if there is one and the program was not told to leave it. Returns :loaded when it ran,
:failed when it could not be read or stopped half way, and NIL when there is nothing to run.

APP: the application."
  (let ((path (init-file-path app)))
    (cond ((or (not (base:config-load-init (app-config app)))
               (null path)
               (not (eq :file (sbcl:file-kind path))))
           nil)
          (t
           (handler-case
               (let ((text (read-script-text path)))
                 (if (run-init-text app text)
                     :loaded
                     :failed))
             (base:drawing-file-error (condition)
               (console-say app
                            :error
                            (i18n:trf "Init file: ~a" condition))
               (show-notice app
                            (i18n:tr "The init file did not finish")
                            (remove #\Newline (princ-to-string condition))
                            +report-message-lines+)
               :failed))))))

(defun start-customizations (app)
  "Read the user's own configuration at startup: the commands saved from recordings, the init file, and then what was
added to :startup.

APP: the application."
  (setf (app-customizations app) (script:make-customizations))
  (scan-recorded-commands app)
  (load-init-file app)
  (run-hooks app :startup))

(defun operation-reload-init (app)
  "Forget what the init file defined and run it again, and read the folder of commands again, so that a change to
either is in force without leaving the program.

APP: the application."
  (setf (app-customizations app) (script:make-customizations))
  (scan-recorded-commands app)
  (case (load-init-file app)
    (:loaded
     (set-message app
                  (i18n:tr "The init file was run again.")))
    (:failed nil)
    (t
     (set-message app
                  (if (init-file-path app)
                      (i18n:trf "There is no init file at ~a." (uiop:native-namestring (init-file-path app)))
                      (i18n:tr "There is no folder for the init file."))
                  :error)))
  (app-request-redraw app))
