(in-package #:borba-cad/persistence)

;;;; The list of recently used drawings: one absolute path per line in a text file in the state
;;;; directory. It is a convenience, so a list that cannot be read or written is only a shorter
;;;; list; the functions say so in their results and never signal.

(defconstant +recent-file-limit+ 12
  "Most drawings the recent list remembers.")

(defparameter *recent-files-name* "recent-files.txt"
  "Name of the file, in the state directory, that holds the recent list.")

(defun recent-files-file (state-directory)
  "Return the pathname of the recent list.

STATE-DIRECTORY: the folder where the program keeps what it remembers."
  (merge-pathnames *recent-files-name*
                   (uiop:ensure-directory-pathname state-directory)))

(defun load-recent-files (state-directory)
  "Return the recently used drawings as pathnames, the latest first. The list is empty when
there is none, when it cannot be read, or when STATE-DIRECTORY is NIL.

STATE-DIRECTORY: the folder where the program keeps what it remembers, or NIL."
  (let ((file (and state-directory (recent-files-file state-directory))))
    (when (and file (eq :file (sbcl:file-kind file)))
      (handler-case
          (with-open-file (stream file :external-format :utf-8)
            (loop for line = (read-line stream nil nil)
                  while line
                  when (plusp (length (string-trim *blank-characters* line)))
                    collect (uiop:parse-native-namestring line)))
        (file-error ()
          nil)
        (stream-error ()
          nil)))))

(defun remember-recent-file (state-directory
                             path)
  "Put the drawing PATH at the head of the recent list, dropping its older mention and the
entries beyond the limit. Returns true when the list was written, and NIL when it could not be
(or STATE-DIRECTORY is NIL).

STATE-DIRECTORY: the folder where the program keeps what it remembers, or NIL.
PATH: the drawing's pathname."
  (let ((entry (uiop:native-namestring path)))
    (when (and state-directory
               (not (find #\Newline entry)))
      (let* ((file (recent-files-file state-directory))
             (older (remove entry
                            (mapcar #'uiop:native-namestring (load-recent-files state-directory))
                            :test #'string=))
             (kept (subseq (cons entry older)
                           0
                           (min +recent-file-limit+ (1+ (length older))))))
        (handler-case
            (progn
              (ensure-directories-exist file)
              (with-open-file (stream file
                                      :direction :output
                                      :if-exists :supersede
                                      :external-format :utf-8)
                (dolist (line kept)
                  (write-line line stream)))
              t)
          (file-error ()
            nil)
          (stream-error ()
            nil))))))
