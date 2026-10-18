(in-package #:borba-cad/base)

(defparameter *entity-kind-labels*
  (list (cons :line (i18n:mark "line"))
        (cons :rectangle (i18n:mark "rectangle"))
        (cons :circle (i18n:mark "circle"))
        (cons :polyline (i18n:mark "polyline"))
        (cons :text (i18n:mark "text"))
        (cons :dimension (i18n:mark "dimension"))
        (cons :arc (i18n:mark "arc"))
        (cons :ellipse (i18n:mark "ellipse"))
        (cons :polygon (i18n:mark "polygon"))
        (cons :radial-dimension (i18n:mark "radial dimension"))
        (cons :angular-dimension (i18n:mark "angle dimension")))
  "The name of each kind of entity, in English, as an error message says it. It is translated where
it is shown.")

(defun entity-kind-label (kind)
  "Return the name of an entity kind in the current language, in lower case, for example \"line\".

KIND: an entity kind keyword."
  (let ((label (cdr (assoc kind *entity-kind-labels*))))
    (if label
        (i18n:tr label)
        (string-downcase (symbol-name kind)))))

(define-condition borba-cad-error (error)
  ()
  (:documentation "Root of every error that Borba CAD signals deliberately. Its report is a
complete, user-facing message in the language of the interface."))

(define-condition invalid-entity (borba-cad-error)
  ((kind :initarg :kind :reader invalid-entity-kind)
   (reason :initarg :reason :reader invalid-entity-reason))
  (:report (lambda (condition stream)
             (write-string (i18n:trf "Invalid ~a: ~a"
                                     (entity-kind-label (invalid-entity-kind condition))
                                     (invalid-entity-reason condition))
                           stream)))
  (:documentation "An entity violates one of its invariants. KIND is the entity kind keyword."))

(define-condition invalid-input (borba-cad-error)
  ((text :initarg :text :reader invalid-input-text)
   (reason :initarg :reason :reader invalid-input-reason))
  (:report (lambda (condition stream)
             (write-string (i18n:trf "Invalid input ~s: ~a"
                                     (invalid-input-text condition)
                                     (invalid-input-reason condition))
                           stream)))
  (:documentation "Text typed by the user could not be interpreted."))

(define-condition layer-error (borba-cad-error)
  ((reason :initarg :reason :reader layer-error-reason))
  (:report (lambda (condition stream)
             (write-string (i18n:trf "Layer error: ~a" (layer-error-reason condition)) stream)))
  (:documentation "A layer operation is not allowed."))

(define-condition edit-refused (borba-cad-error)
  ((reason :initarg :reason :reader edit-refused-reason))
  (:report (lambda (condition stream)
             (write-string (edit-refused-reason condition) stream)))
  (:documentation "An edit that changes existing entities, such as trimming or filleting, cannot be done as asked.
REASON is a complete sentence in the language of the interface."))

(define-condition drawing-file-error (borba-cad-error)
  ((path :initarg :path :initform nil :reader drawing-file-error-path)
   (reason :initarg :reason :reader drawing-file-error-reason))
  (:report (lambda (condition stream)
             (write-string (i18n:trf "Drawing file ~a: ~a"
                                     (or (drawing-file-error-path condition) (i18n:tr "(unnamed)"))
                                     (drawing-file-error-reason condition))
                           stream)))
  (:documentation "Reading or writing a drawing file failed."))

(define-condition drawing-file-io-error (drawing-file-error)
  ()
  (:documentation "The file could not be opened, read, written or renamed."))

(defgeneric drawing-file-error-line (condition)
  (:documentation "Return the 1-based line of the drawing file at which the problem was found.

CONDITION: a drawing-file-syntax-error or one of its subclasses."))

(defgeneric drawing-file-error-column (condition)
  (:documentation "Return the 1-based column of the drawing file at which the problem was found.

CONDITION: a drawing-file-syntax-error or one of its subclasses."))

(define-condition drawing-file-syntax-error (drawing-file-error)
  ((line :initarg :line :reader drawing-file-error-line)
   (column :initarg :column :reader drawing-file-error-column))
  (:report (lambda (condition stream)
             (write-string (i18n:trf "Drawing file ~a, line ~d, column ~d: ~a"
                                     (or (drawing-file-error-path condition) (i18n:tr "(unnamed)"))
                                     (drawing-file-error-line condition)
                                     (drawing-file-error-column condition)
                                     (drawing-file-error-reason condition))
                           stream)))
  (:documentation "The file text is not well-formed."))

(define-condition drawing-file-content-error (drawing-file-syntax-error)
  ()
  (:documentation "The file is well-formed but its content is invalid or unsupported."))

(define-condition drawing-file-version-error (drawing-file-content-error)
  ()
  (:documentation "The file declares a format version this program cannot read."))

(define-condition configuration-error (borba-cad-error)
  ((reason :initarg :reason :reader configuration-error-reason))
  (:report (lambda (condition stream)
             (format stream "Configuration error: ~a" (configuration-error-reason condition))))
  (:documentation "An environment variable or command-line option is invalid."))
