(in-package #:borba-cad/persistence)

;;;; What the user chose in the program and wants kept, such as the language: one `name=value` line
;;;; per setting in a text file in the state directory. Like the recent list it is a convenience,
;;;; so a file that cannot be read or written only means the choices are not remembered; the
;;;; functions say so in their results and never signal.

(defparameter *settings-name* "settings.txt"
  "Name of the file, in the state directory, that holds the settings.")

(defparameter *setting-separator* #\=
  "The character between the name of a setting and its value.")

(defun settings-file (state-directory)
  "Return the pathname of the settings file.

STATE-DIRECTORY: the folder where the program keeps what it remembers."
  (merge-pathnames *settings-name*
                   (uiop:ensure-directory-pathname state-directory)))

(defun load-settings (state-directory)
  "Return the saved settings as a list of (name . value) conses of strings, in file order. The
list is empty when there is none, when the file cannot be read, or when STATE-DIRECTORY is NIL.

STATE-DIRECTORY: the folder where the program keeps what it remembers, or NIL."
  (let ((file (and state-directory (settings-file state-directory))))
    (when (and file (eq :file (sbcl:file-kind file)))
      (handler-case
          (with-open-file (stream file :external-format :utf-8)
            (loop for line = (read-line stream nil nil)
                  for separator = (and line (position *setting-separator* line))
                  while line
                  when (and separator (plusp separator))
                    collect (cons (string-trim *blank-characters* (subseq line 0 separator))
                                  (string-trim *blank-characters* (subseq line (1+ separator))))))
        (file-error ()
          nil)
        (stream-error ()
          nil)))))

(defun load-setting (state-directory
                     name)
  "Return the saved value of the setting NAME as a string, or NIL when it was never saved.

STATE-DIRECTORY: the folder where the program keeps what it remembers, or NIL.
NAME: the name of the setting."
  (cdr (assoc name (load-settings state-directory) :test #'string=)))

(defun save-setting (state-directory
                     name
                     value)
  "Save the setting NAME with VALUE, keeping the other settings. Returns true when the file was
written, and NIL when it could not be (or STATE-DIRECTORY is NIL, or the text has a line break).

STATE-DIRECTORY: the folder where the program keeps what it remembers, or NIL.
NAME: the name of the setting, with no `=` in it.
VALUE: its value."
  (when (and state-directory
             (plusp (length name))
             (not (find *setting-separator* name))
             (not (find #\Newline name))
             (not (find #\Newline value)))
    (let ((file (settings-file state-directory))
          (others (remove name (load-settings state-directory) :key #'car :test #'string=)))
      (handler-case
          (progn
            (ensure-directories-exist file)
            (with-open-file (stream file
                                    :direction :output
                                    :if-exists :supersede
                                    :external-format :utf-8)
              (loop for (other . other-value) in others
                    do (format stream "~a~c~a~%" other *setting-separator* other-value))
              (format stream "~a~c~a~%" name *setting-separator* value))
            t)
        (file-error ()
          nil)
        (stream-error ()
          nil)))))
