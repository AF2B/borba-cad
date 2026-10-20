(in-package #:borba-cad/persistence)

(defconstant +maximum-file-bytes+ (* 96 1024 1024)
  "Largest drawing file that will be read. A 100,000-entity drawing is about 12 MB, and the pictures under a drawing
may take 40 MiB of file, which as text is a third more; the limit bounds the memory used to read an untrusted file
(NFR-7).")

(defun io-failure (path
                   control
                   &rest arguments)
  "Signal a DRAWING-FILE-IO-ERROR.

PATH: the file involved.
CONTROL: format control string for the reason.
ARGUMENTS: format arguments."
  (error 'base:drawing-file-io-error
         :path (uiop:native-namestring path)
         :reason (apply #'i18n:trf control arguments)))

(defun temporary-path-for (target)
  "Return a unique sibling path next to TARGET for write-then-rename saving.

TARGET: the final pathname."
  (uiop:parse-native-namestring
   (format nil "~a.tmp-~d-~d"
           (uiop:native-namestring target)
           (sbcl:process-id)
           (sbcl:monotonic-nanoseconds))))

(defun check-save-target (target)
  "Signal DRAWING-FILE-IO-ERROR, with a reason a person can act on, when a drawing cannot be
saved at TARGET: it names a folder, its folder does not exist or may not be written, or the
file exists and is read-only.

TARGET: the pathname to save to."
  (let ((folder (directory-of target)))
    (cond ((null (pathname-name target))
           (io-failure target "is a folder; type a file name"))
          ((eq :directory (sbcl:file-kind target))
           (io-failure target "is a folder, not a file"))
          ((not (eq :directory (sbcl:file-kind folder)))
           (io-failure target
                       "cannot be saved: the folder ~a does not exist"
                       (uiop:native-namestring folder)))
          ((not (sbcl:path-accessible-p folder :write))
           (io-failure target "cannot be saved: the folder is not writable (permission denied)"))
          ((and (eq :file (sbcl:file-kind target))
                (not (sbcl:path-accessible-p target :write)))
           (io-failure target "cannot be saved: the file is read-only")))))

(defun save-text-file (path
                       writer
                       &key before-commit
                            (external-format :utf-8)
                            (element-type 'character))
  "Write a file atomically, a text file unless ELEMENT-TYPE says bytes: write a sibling temporary file, flush
it to disk, then rename it over PATH. A failure at any point leaves any existing file at PATH untouched and removes the
temporary file. When PATH is a symbolic link the file it points to is replaced, not the link,
and a replaced file keeps its permissions. Returns two values: the saved pathname and a
property list with :bytes, :serialize-nanoseconds (including the disk flush) and
:rename-nanoseconds. Signals DRAWING-FILE-IO-ERROR on failure.

PATH: destination pathname.
WRITER: function of the output stream that writes the contents.
BEFORE-COMMIT: optional function called with the temporary pathname just before the rename;
a test seam for injecting failures.
EXTERNAL-FORMAT: the encoding of the file.
ELEMENT-TYPE: CHARACTER for a text file, or (UNSIGNED-BYTE 8) for one of bytes."
  (let* ((target (sbcl:true-path (merge-pathnames path)))
         (temporary (temporary-path-for target))
         (permissions (and (eq :file (sbcl:file-kind target))
                           (sbcl:file-permissions target)))
         (committed nil)
         (started (sbcl:monotonic-nanoseconds))
         (written started))
    (check-save-target target)
    (unwind-protect
         (handler-case
             (progn
               (with-open-file (stream temporary
                                       :direction :output
                                       :if-exists :error
                                       :element-type element-type
                                       :external-format external-format)
                 (funcall writer stream)
                 (sbcl:sync-output-stream stream))
               (setf written (sbcl:monotonic-nanoseconds))
               (when permissions
                 (sbcl:set-file-permissions temporary permissions))
               (when before-commit
                 (funcall before-commit temporary))
               (sbcl:rename-over temporary target)
               (setf committed t))
           (base:drawing-file-error (condition)
             (error condition))
           (error (condition)
             (io-failure target
                         "cannot be written: ~a"
                         (sbcl:describe-os-error condition))))
      (unless committed
        (sbcl:delete-file-if-exists temporary)))
    (values target
            (list :bytes (sbcl:file-size-bytes target)
                  :serialize-nanoseconds (- written started)
                  :rename-nanoseconds (- (sbcl:monotonic-nanoseconds) written)))))

(defun save-binary-file (path
                         octets
                         &key before-commit)
  "Write a file of bytes atomically, as SAVE-TEXT-FILE writes a text one, and return what it returns.

PATH: destination pathname.
OCTETS: the bytes of the file.
BEFORE-COMMIT: optional function called with the temporary pathname just before the rename."
  (save-text-file path
                  (lambda (stream) (write-sequence octets stream))
                  :before-commit before-commit
                  :element-type '(unsigned-byte 8)))

(defun save-drawing (drawing
                     path
                     &key before-commit)
  "Save DRAWING atomically in the .bcad format: see SAVE-TEXT-FILE for how, and for what is
returned. Signals DRAWING-FILE-IO-ERROR on failure.

DRAWING: the drawing to save.
PATH: destination pathname.
BEFORE-COMMIT: optional function called with the temporary pathname just before the rename;
a test seam for injecting failures."
  (save-text-file path
                  (lambda (stream) (write-drawing drawing stream))
                  :before-commit before-commit))

(defun check-open-source (file)
  "Signal DRAWING-FILE-IO-ERROR, with a reason a person can act on, when FILE cannot be opened as
a drawing: it does not exist, is a folder or another kind of file, or may not be read.

FILE: the pathname to open."
  (case (sbcl:file-kind file)
    ((nil)
     (io-failure file "does not exist"))
    (:directory
     (io-failure file "is a folder, not a drawing file"))
    (:other
     (io-failure file "is not a regular file")))
  (unless (sbcl:path-accessible-p file :read)
    (io-failure file "cannot be opened: permission denied")))

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
                  (sbcl:describe-os-error condition)))
    (stream-error (condition)
      (io-failure file
                  "is not a text file that Borba CAD can read (it must be UTF-8 text): ~a"
                  (sbcl:describe-os-error condition)))))

(defun read-file-octets (file)
  "Read a whole file into a vector of bytes, enforcing the size limit.

FILE: pathname of an existing file."
  (handler-case
      (let ((size (sbcl:file-size-bytes file)))
        (when (> size +maximum-file-bytes+)
          (io-failure file
                      "is larger than the ~d MiB limit"
                      (floor +maximum-file-bytes+ (* 1024 1024))))
        (with-open-file (stream file :element-type '(unsigned-byte 8))
          (let* ((buffer (make-array size :element-type '(unsigned-byte 8)))
                 (count (read-sequence buffer stream)))
            (if (= count size)
                buffer
                (subseq buffer 0 count)))))
    (base:drawing-file-error (condition)
      (error condition))
    (file-error (condition)
      (io-failure file
                  "cannot be opened: ~a"
                  (sbcl:describe-os-error condition)))))

(defun load-drawing (path)
  "Read and validate the drawing file at PATH. Returns two values: the drawing and a property
list with :bytes, :read-nanoseconds and :parse-nanoseconds (parsing includes validation and
building). Signals a DRAWING-FILE-ERROR subclass on any problem; no partial result is returned.

PATH: pathname of the drawing file."
  (let* ((file (merge-pathnames path))
         (started (sbcl:monotonic-nanoseconds)))
    (check-open-source file)
    (let* ((text (read-file-text file))
           (read-done (sbcl:monotonic-nanoseconds))
           (drawing (parse-drawing-text text :path (uiop:native-namestring file))))
      (values drawing
              (list :bytes (sbcl:file-size-bytes file)
                    :read-nanoseconds (- read-done started)
                    :parse-nanoseconds (- (sbcl:monotonic-nanoseconds) read-done))))))
