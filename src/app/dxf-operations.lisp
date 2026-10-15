(in-package #:borba-cad/app)

;;;; Importing a DXF file as a new drawing and exporting the drawing as a DXF file. What could not be
;;;; carried over is told to the user, and logged, and never silently dropped.

(defparameter *dxf-extension* "dxf"
  "The extension of a DXF file.")

(defun dxf-file-path (text)
  "Return the pathname of the DXF file that TEXT names, with the extension .dxf added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    *dxf-extension*
                                    (list *dxf-extension*)))

(defun log-dxf-report (app
                       event
                       report)
  "Write what an import or an export did and left out to the log.

APP: the application.
EVENT: the event name.
REPORT: the DXF report."
  (when (app-telemetry app)
    (obs:log-event (obs:telemetry-logger (app-telemetry app))
                   :info
                   event
                   :entities (dxf:report-imported-total report)
                   :ignored (dxf:report-ignored-total report)
                   :ignored-types (format nil "~{~a~^,~}"
                                          (loop for (type . count) in (dxf:report-ignored report)
                                                collect (format nil "~a=~d" type count)))
                   :notes (length (dxf:report-notes report)))))

(defun show-dxf-report (app
                        path
                        report)
  "Tell the user how an import went: a line in the status bar when everything was carried over, and
otherwise a question with the whole report to read.

APP: the application.
PATH: the DXF file.
REPORT: the DXF report."
  (if (and (zerop (dxf:report-ignored-total report)) (null (dxf:report-notes report)))
      (set-message app
                   (i18n:trf "Imported ~a: ~a"
                             (file-namestring path)
                             (dxf:report-summary report)))
      (open-choice-prompt app
                          (i18n:trf "Imported ~a" (file-namestring path))
                          (dxf:report-summary report)
                          (list (cons (i18n:tr "OK") nil))
                          +report-message-lines+)))

(defun finish-dxf-import (app
                          path
                          document
                          units
                          started)
  "Make the drawing of a DXF document the one being edited, as a new drawing that is not saved yet, and
say how it went. Returns true on success.

APP: the application.
PATH: the DXF file, for messages.
DOCUMENT: the parsed DXF document.
UNITS: the unit of its numbers.
STARTED: the monotonic time at which the operation began."
  (handler-case
      (multiple-value-bind (drawing report)
          (dxf:import-document document :units units)
        (obs:record-operation-result (app-telemetry app)
                                     "file.import-dxf"
                                     (- (sbcl:monotonic-nanoseconds) started)
                                     :info
                                     :entity-count (dom:drawing-entity-count drawing)
                                     :result :success)
        (log-dxf-report app
                        "dxf.import"
                        report)
        (reset-document app
                        drawing
                        nil)
        (setf (app-saved-revision app) -1)
        (show-dxf-report app
                         path
                         report)
        t)
    ((or base:drawing-file-error base:invalid-input) (condition)
      (fail-file-operation app
                           "file.import-dxf"
                           started
                           condition))))

(defun ask-for-dxf-units (app
                          path
                          document
                          started)
  "Ask in what unit the numbers of a DXF file are, and go on with the import when it is answered; the
last button, which Escape chooses, gives the import up.

APP: the application.
PATH: the DXF file.
DOCUMENT: the parsed DXF document.
STARTED: the monotonic time at which the operation began."
  (open-choice-prompt app
                      (i18n:tr "Units of the DXF file")
                      (i18n:tr "The file does not say what its numbers are. In what unit was it drawn?")
                      (append (loop for (label unit) in (list (list (i18n:tr "Millimeters") :millimeter)
                                                              (list (i18n:tr "Centimeters") :centimeter)
                                                              (list (i18n:tr "Meters") :meter))
                                    collect (let ((unit unit))
                                              (cons label
                                                    (lambda ()
                                                      (finish-dxf-import app
                                                                         path
                                                                         document
                                                                         unit
                                                                         started)))))
                              (list (cons (i18n:tr "Cancel") nil)))))

(defun import-dxf-into-app (app
                            path-text)
  "Read the DXF file at PATH-TEXT and make a new drawing of it. A file that does not say the unit of its
numbers is followed by a question, and the import goes on when it is answered. On failure the current
drawing is untouched and the reason is shown. Returns true on success, or when the question was asked.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (let* ((path (resolve-open-path path-text #'dxf-file-path))
               (document (dxf:read-dxf-file path))
               (units (dxf:document-units document)))
          (if units
              (finish-dxf-import app
                                 path
                                 document
                                 units
                                 started)
              (progn
                (ask-for-dxf-units app
                                   path
                                   document
                                   started)
                t)))
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "file.import-dxf"
                             started
                             condition)))))

(defun export-dxf-from-app (app
                            path-text)
  "Save the drawing as a DXF file at PATH-TEXT (.dxf is added to a name without an extension) and say what
changed shape in the writing. On failure the previous file is untouched and the reason is shown. Returns
true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (multiple-value-bind (saved stats report)
            (dxf:save-dxf (app-drawing app) (dxf-file-path path-text))
          (obs:record-operation-result (app-telemetry app)
                                       "file.export-dxf"
                                       (- (sbcl:monotonic-nanoseconds) started)
                                       :info
                                       :bytes (getf stats :bytes)
                                       :entity-count (dom:drawing-entity-count (app-drawing app))
                                       :result :success)
          (log-dxf-report app
                          "dxf.export"
                          report)
          (show-notice app
                       (i18n:tr "DXF exported")
                       (format nil "~a ~a"
                               (i18n:trf "~a was saved (~a)."
                                         (file-namestring saved)
                                         (base:format-byte-size (getf stats :bytes)))
                               (dxf:export-summary report))
                       +report-message-lines+)
          t)
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-export-operation app
                               "file.export-dxf"
                               started
                               condition)))))

(defun operation-import-dxf (app)
  "Open the dialog where the user finds a DXF file to import as a new drawing, after offering to save the
current one.

APP: the application."
  (confirm-discard app
                   (lambda ()
                     (open-file-dialog app
                                       :open
                                       nil
                                       :dxf))))

(defun operation-export-dxf (app)
  "Open the dialog where the user picks a folder and a name, and export the drawing there as a DXF file.

APP: the application."
  (open-file-dialog app
                    :save
                    nil
                    :dxf))
