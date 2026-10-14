(in-package #:borba-cad/sbcl-support)

;;;; File-system operations written with standard Common Lisp and UIOP only. Where the POSIX
;;;; calls of filesystem.lisp exist they are used instead, because they see more (permissions,
;;;; devices, links, the exact error number); where they do not (Windows), these answer. They are
;;;; always compiled, so the tests can hold them to the same answers as the POSIX ones on the
;;;; machine that has both.

(defun portable-file-kind (path)
  "Return :directory or :file for what PATH names, or NIL when nothing can be found there.
Devices and other special files are not told apart from files.

PATH: a pathname."
  (cond ((uiop:directory-exists-p path) :directory)
        ((uiop:file-exists-p path) :file)
        (t nil)))

(defun portable-entry (path
                       kind)
  "Return what a folder entry is as (name kind size modified), or NIL when it cannot be examined.

PATH: the entry's pathname.
KIND: :directory or :file."
  (handler-case
      (list (if (eq kind :directory)
                (car (last (pathname-directory path)))
                (file-namestring path))
            kind
            (and (eq kind :file) (file-size-bytes path))
            (file-write-date path))
    (file-error ()
      nil)))

(defun portable-directory-listing (directory)
  "Return the entries of the folder DIRECTORY like DIRECTORY-LISTING does, using UIOP: a list of
(name kind size modified), and a second value that is NIL or a sentence that says why the folder
cannot be read.

DIRECTORY: a pathname."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (if (not (uiop:directory-exists-p directory))
        (values nil "This folder cannot be read: No such file or directory.")
        (handler-case
            (values (remove nil
                            (append (mapcar (lambda (path) (portable-entry path :directory))
                                            (uiop:subdirectories directory))
                                    (mapcar (lambda (path) (portable-entry path :file))
                                            (uiop:directory-files directory))))
                    nil)
          (file-error (condition)
            (values nil (format nil "This folder cannot be read: ~a." (describe-os-error condition))))))))

(defun portable-parent-directory (path)
  "Return the folder that holds PATH, as a directory pathname, or NIL for a root.

PATH: a pathname."
  (let ((components (pathname-directory (uiop:ensure-directory-pathname path))))
    (and (rest components)
         (make-pathname :directory (butlast components)
                        :name nil
                        :type nil
                        :version nil
                        :defaults path))))

(defun portable-make-directory (path)
  "Create the folder PATH like MAKE-DIRECTORY does, using UIOP: it must not exist yet and its
parent must. Returns NIL on success, and otherwise a sentence that says why not.

PATH: a pathname."
  (let ((parent (portable-parent-directory path)))
    (cond ((portable-file-kind path)
           "The folder cannot be created: File exists.")
          ((not (and parent (uiop:directory-exists-p parent)))
           "The folder cannot be created: No such file or directory.")
          (t
           (handler-case (progn
                           (ensure-directories-exist (uiop:ensure-directory-pathname path))
                           nil)
             (file-error (condition)
               (format nil "The folder cannot be created: ~a." (describe-os-error condition))))))))
