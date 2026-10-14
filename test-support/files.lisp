(in-package #:borba-cad/test-support)

;;;; Scratch directories for tests that write files.

(defun make-temporary-directory ()
  "Create and return a fresh, empty directory below the system's temporary directory."
  (let ((directory (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "borba-cad-test-~d-~d/"
                                             (borba-cad/sbcl-support:process-id)
                                             (random 100000000 (make-random-state t)))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist directory)
    directory))

(defmacro with-temporary-directory ((variable) &body body)
  "Run BODY with VARIABLE bound to a fresh temporary directory, deleted afterwards.

VARIABLE: symbol bound to the directory pathname.
BODY: forms to run."
  `(let ((,variable (make-temporary-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,variable :validate t :if-does-not-exist :ignore))))
