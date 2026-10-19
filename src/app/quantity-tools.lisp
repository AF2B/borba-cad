(in-package #:borba-cad/app)

;;;; Quantities in the application: a form asks which report, for what (the whole drawing or the selection) and in what
;;;; format a CSV file is written; the report is then shown as a table to read, with a button that writes it as a CSV
;;;; file. The reports are made by the domain (quantities.lisp); this file asks, shows and writes.

(defparameter *csv-extensions* (list "csv")
  "Extensions of the CSV files that the dialog lists.")

(defconstant +byte-order-mark+ #xFEFF
  "The character that begins a UTF-8 file that Excel is to read as UTF-8.")

(defun csv-file-path (text)
  "Return the pathname of the CSV file that TEXT names, with the extension .csv added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    (first *csv-extensions*)
                                    *csv-extensions*))

(defun quantity-scope-ids (app
                           scope)
  "Return the ids that a report is made of: those of the selection, or NIL for the whole drawing.

APP: the application.
SCOPE: :selection or :all."
  (and (eq scope :selection)
       (selection-ids app)))

(defun quantity-info (app
                      values)
  "Return the line under the quantities form: how many entities it counts, or that nothing is selected.

APP: the application.
VALUES: the alist of the form."
  (let ((scope (form-value values :scope)))
    (if (and (eq scope :selection) (zerop (hash-table-count (app-selection app))))
        (values (i18n:tr "Nothing is selected.") t)
        (values (i18n:trf "Counts ~d ~:*~[entities~;entity~:;entities~]."
                          (dom:counted-entity-count (app-drawing app) (quantity-scope-ids app scope)))
                nil))))

;;; Writing the CSV file

(defun export-quantities-from-app (app
                                   path-text)
  "Write the report that is waiting to be exported as a CSV file at PATH-TEXT (.csv is added to a name without an
extension) and tell the user how it went in a modal. On failure the previous file is untouched. Returns true on
success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds))
        (pending (app-quantity-export app)))
    (handler-case
        (let* ((excel (eq (cdr pending) :excel))
               (text (dom:quantity-report-csv (car pending)
                                              (display-unit-of app)
                                              :separator (if excel #\; #\,)
                                              :decimal-mark (if excel #\, #\.))))
          (multiple-value-bind (saved stats)
              (persist:save-text-file (csv-file-path path-text)
                                      (lambda (stream)
                                        (when excel
                                          (write-char (code-char +byte-order-mark+) stream))
                                        (write-string text stream)))
            (obs:record-operation-result (app-telemetry app)
                                         "file.export-quantities"
                                         (- (sbcl:monotonic-nanoseconds) started)
                                         :info
                                         :bytes (getf stats :bytes)
                                         :result :success)
            (show-notice app
                         (i18n:tr "Quantities saved")
                         (i18n:trf "~a was saved (~a)."
                                   (file-namestring saved)
                                   (base:format-byte-size (getf stats :bytes))))
            t))
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-export-operation app
                               "file.export-quantities"
                               started
                               condition)))))

(defun operation-export-quantities (app)
  "Ask where to write the report that is shown, as a CSV file.

APP: the application."
  (open-file-dialog app
                    :save
                    nil
                    :quantities))

;;; The report

(defun quantity-report-title (kind)
  "Return the heading of the modal that shows a report.

KIND: :layer, :kind, :block or :cut."
  (ecase kind
    (:layer (i18n:tr "Quantities: totals by layer"))
    (:kind (i18n:tr "Quantities: totals by kind of entity"))
    (:block (i18n:tr "Quantities: blocks placed"))
    (:cut (i18n:tr "Quantities: cut list"))))

(defun quantity-report-message (kind
                                scope)
  "Return what the modal that shows a report says under its heading.

KIND: :layer, :kind, :block or :cut.
SCOPE: :selection or :all."
  (format nil "~a~@[ ~a~]"
          (ecase scope
            (:all (i18n:tr "The whole drawing; layers that are hidden are not counted."))
            (:selection (i18n:tr "The selection.")))
          (and (eq kind :cut)
               (i18n:tr "Each line, curve, polyline or shape is one bar; explode a shape to count its sides."))))

(defun show-quantity-report (app
                             report
                             scope
                             format)
  "Show a report as a table, with a button that writes it as a CSV file.

APP: the application.
REPORT: the report.
SCOPE: :selection or :all.
FORMAT: :standard or :excel, how the CSV file is written."
  (setf (app-quantity-export app) (cons report format))
  (open-report-prompt app
                      (quantity-report-title (dom:quantity-report-kind report))
                      (quantity-report-message (dom:quantity-report-kind report) scope)
                      report
                      (list (cons (i18n:tr "Export CSV...")
                                  (lambda () (operation-export-quantities app)))
                            (cons (i18n:tr "Close") nil))))

(defun make-quantity-report (app
                             values)
  "Make the report that a form asks for and show it. Returns a sentence when it cannot be made, so that the form stays
open.

APP: the application.
VALUES: the alist of the form."
  (let ((scope (form-value values :scope)))
    (handler-case
        (progn
          (when (and (eq scope :selection) (zerop (hash-table-count (app-selection app))))
            (error 'base:edit-refused :reason (i18n:tr "Nothing is selected.")))
          (let ((report (dom:quantity-report (app-drawing app)
                                             (form-value values :report)
                                             :ids (quantity-scope-ids app scope))))
            (remember-form-settings app (remove :scope values :key #'car))
            (show-quantity-report app
                                  report
                                  scope
                                  (form-value values :format))
            nil))
      (base:borba-cad-error (condition)
        (remove #\Newline (princ-to-string condition))))))

(defun operation-quantities (app)
  "Ask which report of the quantities of the drawing to make, and show it.

APP: the application."
  (open-form-prompt app
                    (i18n:tr "Quantities")
                    (i18n:tr "Count and measure what the drawing holds, to read here or to write as a CSV file.")
                    (list (list :report
                                (i18n:tr "Report")
                                (form-setting app
                                              :report
                                              :layer)
                                (list (cons :layer (i18n:tr "Totals by layer"))
                                      (cons :kind (i18n:tr "Totals by kind of entity"))
                                      (cons :block (i18n:tr "Blocks placed"))
                                      (cons :cut (i18n:tr "Cut list: bars by length"))))
                          (list :scope
                                (i18n:tr "Count")
                                (if (zerop (hash-table-count (app-selection app)))
                                    :all
                                    :selection)
                                (list (cons :all (i18n:tr "The whole drawing"))
                                      (cons :selection (i18n:tr "Only the selection"))))
                          (list :format
                                (i18n:tr "CSV file")
                                (form-setting app
                                              :format
                                              (if (eq i18n:*language* :pt-br)
                                                  :excel
                                                  :standard))
                                (list (cons :standard (i18n:tr "Comma, decimal point"))
                                      (cons :excel (i18n:tr "Semicolon, decimal comma (Excel in Portuguese)")))))
                    (lambda (values) (make-quantity-report app values))
                    :info (lambda (values) (quantity-info app values))))
