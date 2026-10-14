(in-package #:borba-cad/persistence)

;;;; Paths as people write them. A path arrives pasted with quotes from a file manager, dropped as
;;;; a file:// address, padded with blanks, without an extension, or naming a folder. This file
;;;; turns the text into a pathname and says plainly when it cannot be a drawing file.

(defparameter *drawing-extension* "bcad"
  "Extension that a drawing file name gets when it does not already end in one that the program
writes.")

(defparameter *known-drawing-extensions* (list "bcad")
  "Extensions of the file formats that the program writes. A name that ends in one of them is
saved as typed; any other name, including one with a different extension such as plan.txt or
v1.2 final, gets .bcad after it.")

(defparameter *blank-characters* (list #\Space #\Tab #\Newline #\Return)
  "Characters trimmed from both ends of a typed path.")

(defparameter *quote-characters* (list #\" #\')
  "Quote characters that may surround a pasted path.")

(defparameter *file-uri-prefix* "file://"
  "Start of a file address, as a file manager gives it when a file is dropped or copied.")

(defparameter *local-host-name* "localhost"
  "Host name that a file address may carry and that means this machine.")

(defconstant +percent-escape-length+ 3
  "Characters of a percent escape such as %20.")

(defun path-failure (text
                     control
                     &rest arguments)
  "Signal INVALID-INPUT for the typed path TEXT.

TEXT: what the user typed.
CONTROL: format control string for the reason.
ARGUMENTS: format arguments."
  (error 'base:invalid-input
         :text text
         :reason (apply #'i18n:trf control arguments)))

(defun starts-with-p (prefix
                      text)
  "Return true when TEXT starts with PREFIX, ignoring case.

PREFIX: the expected beginning.
TEXT: the string to look at."
  (and (>= (length text) (length prefix))
       (string-equal prefix text :end2 (length prefix))))

(defun percent-escape-at (text
                          index)
  "Return the byte that the percent escape at INDEX stands for, or NIL when there is none.

TEXT: the string.
INDEX: position of a possible percent sign."
  (and (char= #\% (char text index))
       (<= (+ index +percent-escape-length+) (length text))
       (let ((high (digit-char-p (char text (+ index 1)) 16))
             (low (digit-char-p (char text (+ index 2)) 16)))
         (and high
              low
              (+ (* 16 high) low)))))

(defun decode-percent-escapes (text)
  "Return TEXT with every percent escape (%20 and the like) replaced by the character it stands
for, the escaped bytes read as UTF-8. When the escapes are not valid UTF-8 the text is returned
unchanged.

TEXT: a path from a file address."
  (let ((result (make-string-output-stream))
        (pending '())
        (index 0))
    (flet ((flush ()
             (when pending
               (let ((decoded (sbcl:utf-8-octets->string (nreverse pending))))
                 (unless decoded
                   (return-from decode-percent-escapes text))
                 (write-string decoded result)
                 (setf pending '())))))
      (loop while (< index (length text))
            do (let ((byte (percent-escape-at text index)))
                 (cond (byte
                        (push byte pending)
                        (incf index +percent-escape-length+))
                       (t
                        (flush)
                        (write-char (char text index) result)
                        (incf index)))))
      (flush))
    (get-output-stream-string result)))

(defun file-uri-path-text (uri)
  "Return the path that a file:// address names: the host is dropped, escapes are decoded, and
the slash before a Windows drive letter is removed.

URI: an address that starts with file://."
  (let* ((rest (subseq uri (length *file-uri-prefix*)))
         (without-host (if (starts-with-p *local-host-name* rest)
                           (subseq rest (length *local-host-name*))
                           rest))
         (decoded (decode-percent-escapes without-host)))
    (if (and (>= (length decoded) 3)
             (char= #\/ (char decoded 0))
             (alpha-char-p (char decoded 1))
             (char= #\: (char decoded 2)))
        (subseq decoded 1)
        decoded)))

(defun clean-path-text (text)
  "Return the path that TEXT means: blanks around it and one pair of quotes around it are
removed, and a file:// address becomes the path it names. Signals INVALID-INPUT when nothing is
left.

TEXT: what the user typed, pasted or dropped."
  (let* ((trimmed (string-trim *blank-characters* text))
         (unquoted (if (and (>= (length trimmed) 2)
                            (member (char trimmed 0) *quote-characters*)
                            (char= (char trimmed 0) (char trimmed (1- (length trimmed)))))
                       (string-trim *blank-characters* (subseq trimmed 1 (1- (length trimmed))))
                       trimmed))
         (cleaned (if (starts-with-p *file-uri-prefix* unquoted)
                      (file-uri-path-text unquoted)
                      unquoted)))
    (when (zerop (length cleaned))
      (path-failure text "type the name of a file"))
    cleaned))

(defun native-path (text)
  "Return the pathname that the user-typed TEXT names. Blanks, quotes and file:// addresses are
dealt with (see CLEAN-PATH-TEXT), characters such as * and [ are taken literally, a leading ~
means the home directory, and relative names are resolved against the current directory, so the
result is always absolute.

TEXT: a file name as typed by the user."
  (let* ((cleaned (clean-path-text text))
         (home (uiop:native-namestring (user-homedir-pathname)))
         (path (uiop:parse-native-namestring
                (cond ((string= cleaned "~")
                       home)
                      ((and (> (length cleaned) 1)
                            (char= #\~ (char cleaned 0))
                            (find (char cleaned 1) "/\\"))
                       (concatenate 'string home (subseq cleaned 2)))
                      (t cleaned)))))
    (if (uiop:absolute-pathname-p path)
        path
        (merge-pathnames path (uiop:getcwd)))))

(defun file-path-with-extension (text
                                 extension
                                 known-extensions)
  "Return the pathname of the file that TEXT names. A name that does not end in one of
KNOWN-EXTENSIONS gets EXTENSION after it (plan becomes plan.bcad and v1.2 final becomes
v1.2 final.bcad); a trailing dot is dropped first. A path that names a folder is returned as it is,
for the caller to refuse.

TEXT: a file name as typed by the user.
EXTENSION: the extension to add, without the dot.
KNOWN-EXTENSIONS: the extensions that need nothing added."
  (let* ((path (native-path text))
         (type (pathname-type path)))
    (cond ((null (pathname-name path))
           path)
          ((and type (member type known-extensions :test #'string-equal))
           path)
          ((and type (plusp (length type)))
           (make-pathname :name (concatenate 'string (pathname-name path) "." type)
                          :type extension
                          :defaults path))
          (t
           (make-pathname :type extension
                          :defaults path)))))

(defun drawing-file-path (text)
  "Return the pathname of the drawing file that TEXT names. A name that does not end in an
extension that the program writes gets .bcad after it (plan becomes plan.bcad and v1.2 final
becomes v1.2 final.bcad); a trailing dot is dropped first. A path that names a folder is returned
as it is, for the caller to refuse.

TEXT: a file name as typed by the user."
  (file-path-with-extension text
                            *drawing-extension*
                            *known-drawing-extensions*))

(defun directory-of (path)
  "Return the folder that contains the file PATH, as a directory pathname.

PATH: a pathname."
  (make-pathname :name nil
                 :type nil
                 :version nil
                 :defaults path))

(defun user-directories-entry (key)
  "Return the folder that the user's XDG directories file assigns to KEY, or NIL. It is how a
Brazilian Portuguese desktop learns that the documents folder is called Documentos.

KEY: the variable name, for example \"XDG_DOCUMENTS_DIR\"."
  (let* ((home (uiop:native-namestring (user-homedir-pathname)))
         (configuration (or (sbcl:environment-variable "XDG_CONFIG_HOME")
                            (concatenate 'string home ".config/")))
         (file (merge-pathnames "user-dirs.dirs"
                                (uiop:ensure-directory-pathname configuration))))
    (when (eq :file (sbcl:file-kind file))
      (with-open-file (stream file :external-format :utf-8 :if-does-not-exist nil)
        (when stream
          (loop for line = (read-line stream nil nil)
                while line
                do (let ((prefix (concatenate 'string key "=\"")))
                     (when (starts-with-p prefix line)
                       (let* ((end (position #\" line :start (length prefix) :from-end t))
                              (value (subseq line (length prefix) end))
                              (expanded (if (starts-with-p "$HOME" value)
                                            (concatenate 'string home "/" (string-left-trim "/" (subseq value 5)))
                                            value)))
                         (return (uiop:ensure-directory-pathname expanded)))))))))))

(defun default-drawings-directory ()
  "Return the folder where a drawing is offered to be saved or opened when nothing else is known:
the user's documents folder, in whatever language the desktop calls it, or the home folder.
The folder is the first that exists among the XDG documents directory, Documents and Documentos."
  (let ((home (user-homedir-pathname)))
    (or (find-if (lambda (candidate)
                   (and candidate
                        (eq :directory (sbcl:file-kind candidate))))
                 (list (user-directories-entry "XDG_DOCUMENTS_DIR")
                       (merge-pathnames "Documents/" home)
                       (merge-pathnames "Documentos/" home)))
        home)))
