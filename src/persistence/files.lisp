(in-package #:borba-cad/persistence)

(defconstant +maximum-file-bytes+ (* 64 1024 1024)
  "Largest drawing file that will be read. A 100,000-entity drawing is about 12 MB; the limit
bounds the memory used to read an untrusted file (NFR-7).")

(defun native-path (text)
  "Return the pathname that the user-typed TEXT names. Characters such as * and [ are taken
literally, a leading ~ means the home directory, and relative names are resolved against
the current directory.

TEXT: a file name as typed by the user."
  (let ((home (uiop:native-namestring (user-homedir-pathname))))
    (merge-pathnames
     (uiop:parse-native-namestring
      (cond ((string= text "~") home)
            ((and (> (length text) 1) (string= "~/" text :end2 2))
             (concatenate 'string home (subseq text 2)))
            (t text))))))

(defun io-failure (path
                   control
                   &rest arguments)
  "Signal a DRAWING-FILE-IO-ERROR.

PATH: the file involved.
CONTROL: format control string for the reason.
ARGUMENTS: format arguments."
  (error 'base:drawing-file-io-error
         :path (uiop:native-namestring path)
         :reason (apply #'format nil control arguments)))

(defun temporary-path-for (target)
  "Return a unique sibling path next to TARGET for write-then-rename saving.

TARGET: the final pathname."
  (uiop:parse-native-namestring
   (format nil "~a.tmp-~d-~d"
           (uiop:native-namestring target)
           (sbcl:process-id)
           (sbcl:monotonic-nanoseconds))))

(defun save-drawing (drawing
                     path
                     &key before-commit)
  "Save DRAWING atomically: write a sibling temporary file, flush it to disk, then rename it
over PATH. A failure at any point leaves any existing file at PATH untouched and removes the
temporary file. Returns two values: the saved pathname and a property list with :bytes,
:serialize-nanoseconds (including the disk flush) and :rename-nanoseconds. Signals
DRAWING-FILE-IO-ERROR on failure.

DRAWING: the drawing to save.
PATH: destination pathname.
BEFORE-COMMIT: optional function called with the temporary pathname just before the rename;
a test seam for injecting failures."
  (let* ((target (merge-pathnames path))
         (temporary (temporary-path-for target))
         (committed nil)
         (started (sbcl:monotonic-nanoseconds))
         (written started))
    (unwind-protect
         (handler-case
             (progn
               (with-open-file (stream temporary
                                       :direction :output
                                       :if-exists :error
                                       :external-format :utf-8)
                 (write-drawing drawing stream)
                 (sbcl:sync-output-stream stream))
               (setf written (sbcl:monotonic-nanoseconds))
               (when before-commit
                 (funcall before-commit temporary))
               (sbcl:rename-over temporary target)
               (setf committed t))
           (base:drawing-file-error (condition)
             (error condition))
           (error (condition)
             (io-failure target
                         "cannot be written: ~a"
                         condition)))
      (unless committed
        (sbcl:delete-file-if-exists temporary)))
    (values target
            (list :bytes (sbcl:file-size-bytes target)
                  :serialize-nanoseconds (- written started)
                  :rename-nanoseconds (- (sbcl:monotonic-nanoseconds) written)))))

(defun read-file-text (file)
  "Read a whole UTF-8 text file into a string, enforcing the size limit.

FILE: pathname of an existing file."
  (handler-case
      (let ((size (sbcl:file-size-bytes file)))
        (when (> size +maximum-file-bytes+)
          (io-failure file
                      "is larger than the ~d MiB limit"
                      (floor +maximum-file-bytes+ (* 1024 1024))))
        (with-open-file (stream file :external-format :utf-8)
          (let* ((buffer (make-string size))
                 (count (read-sequence buffer stream)))
            (if (= count size)
                buffer
                (subseq buffer 0 count)))))
    (base:drawing-file-error (condition)
      (error condition))
    (file-error (condition)
      (io-failure file
                  "cannot be opened: ~a"
                  condition))
    (stream-error (condition)
      (io-failure file
                  "cannot be read as UTF-8 text: ~a"
                  condition))))

(defun load-drawing (path)
  "Read and validate the drawing file at PATH. Returns two values: the drawing and a property
list with :bytes, :read-nanoseconds and :parse-nanoseconds (parsing includes validation and
building). Signals a DRAWING-FILE-ERROR subclass on any problem; no partial result is returned.

PATH: pathname of the drawing file."
  (let* ((file (merge-pathnames path))
         (started (sbcl:monotonic-nanoseconds)))
    (unless (probe-file file)
      (io-failure file "does not exist"))
    (let* ((text (read-file-text file))
           (read-done (sbcl:monotonic-nanoseconds))
           (drawing (parse-drawing-text text :path (uiop:native-namestring file))))
      (values drawing
              (list :bytes (sbcl:file-size-bytes file)
                    :read-nanoseconds (- read-done started)
                    :parse-nanoseconds (- (sbcl:monotonic-nanoseconds) read-done))))))
