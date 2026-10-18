(in-package #:borba-cad/app)

;;;; Exporting the drawing as a sheet to print: a PDF file or an SVG file, to a scale on a sheet of a chosen size. A few
;;;; questions come first, one after another, then the dialog where the file is named; what is chosen is remembered
;;;; for the next export.

(defparameter *pdf-extensions* (list "pdf")
  "Extensions of the PDF files that the dialog lists.")

(defparameter *svg-extensions* (list "svg")
  "Extensions of the SVG files that the dialog lists.")

(defparameter *plot-paper-choices* '(:a4 :a3 :a2 :a1)
  "The sheets the first question offers, the first being the one that Enter chooses.")

(defparameter *fit-scale-text* "fit"
  "What the scale question starts with, and what it says for a drawing made as large as the sheet allows.")

(defun pdf-file-path (text)
  "Return the pathname of the PDF file that TEXT names, with the extension .pdf added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    (first *pdf-extensions*)
                                    *pdf-extensions*))

(defun svg-file-path (text)
  "Return the pathname of the SVG file that TEXT names, with the extension .svg added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    (first *svg-extensions*)
                                    *svg-extensions*))

;;; Saving

(defun plot-notice (saved
                    stats
                    plan
                    replaced)
  "Return what the notice after a plot says: the file, the sheet and the scale, and what the user should know.

SAVED: the pathname that was written.
STATS: the property list of the saving.
PLAN: the plot plan that was used.
REPLACED: how many characters of text became a question mark."
  (let ((settings (plot:plot-plan-settings plan)))
    (format nil "~a~@[ ~a~]~@[ ~a~]"
            (i18n:trf "~a was saved (~a). Sheet ~a, scale ~a."
                      (file-namestring saved)
                      (base:format-byte-size (getf stats :bytes))
                      (plot:paper-name (plot:plot-settings-paper settings))
                      (plot:scale-text (plot:plot-plan-ratio plan)))
            (and (not (plot:plot-plan-fits plan))
                 (i18n:tr "The drawing is larger than the sheet at this scale, so part of it is outside the margins."))
            (and (plusp replaced)
                 (i18n:trf "~d characters could not be written in the PDF font and became question marks."
                           replaced)))))

(defun export-plot-from-app (app
                             path
                             event
                             title
                             renderer
                             saver)
  "Render the drawing with the remembered settings, save it, and tell the user how it went in a modal. On failure the
previous file is untouched. Returns true on success.

APP: the application.
PATH: the pathname to write.
EVENT: the dotted name of the operation, for the log.
TITLE: the heading of the notice.
RENDERER: function of the drawing and the settings that returns what is to be written, the plot plan, and how many
characters of text were replaced.
SAVER: function of the pathname and what was rendered that writes it and returns what SAVE-TEXT-FILE returns."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (multiple-value-bind (content plan replaced) (funcall renderer
                                                              (app-drawing app)
                                                              (or (app-plot-settings app)
                                                                  (plot:make-plot-settings)))
          (multiple-value-bind (saved stats) (funcall saver
                                                      path
                                                      content)
            (obs:record-operation-result (app-telemetry app)
                                         event
                                         (- (sbcl:monotonic-nanoseconds) started)
                                         :info
                                         :bytes (getf stats :bytes)
                                         :entity-count (plot:plot-plan-entity-count plan)
                                         :result :success)
            (show-notice app
                         title
                         (plot-notice saved
                                      stats
                                      plan
                                      (or replaced 0))
                         +report-message-lines+)
            t))
      ((or base:drawing-file-error base:invalid-input base:export-refused) (condition)
        (fail-export-operation app
                               event
                               started
                               condition)))))

(defun export-pdf-from-app (app
                            path-text)
  "Save the drawing as a PDF file at PATH-TEXT (.pdf is added to a name without an extension), on the sheet and at the
scale that were chosen. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (export-plot-from-app app
                        (pdf-file-path path-text)
                        "file.export-pdf"
                        (i18n:tr "PDF exported")
                        #'plot:render-pdf
                        #'persist:save-binary-file))

(defun export-svg-from-app (app
                            path-text)
  "Save the drawing as an SVG file at PATH-TEXT (.svg is added to a name without an extension), on the sheet and at the
scale that were chosen. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (export-plot-from-app app
                        (svg-file-path path-text)
                        "file.export-svg"
                        (i18n:tr "SVG exported")
                        #'plot:render-svg
                        (lambda (path
                                 text)
                          (persist:save-text-file path
                                                  (lambda (stream)
                                                    (write-string text stream))))))

;;; The questions

(defun plot-format-title (format)
  "Return the heading of the questions of an export.

FORMAT: :pdf or :svg."
  (ecase format
    (:pdf (i18n:tr "Export as PDF"))
    (:svg (i18n:tr "Export as SVG"))))

(defun remembered-scale-text (app)
  "Return the scale of the last export as text, or \"fit\" when there was none or it fitted.

APP: the application."
  (let ((settings (app-plot-settings app)))
    (if (and settings (plot:plot-settings-ratio settings))
        (plot:scale-text (plot:plot-settings-ratio settings))
        *fit-scale-text*)))

(defun ask-for-plot-colors (app
                            format
                            paper
                            ratio)
  "Ask whether to print in black or in the colors of the layers, and go on to the dialog where the file is named.

APP: the application.
FORMAT: :pdf or :svg.
PAPER: the sheet chosen.
RATIO: the scale chosen, or NIL to fit."
  (flet ((choose (monochrome)
           (lambda ()
             (setf (app-plot-settings app) (plot:make-plot-settings :paper paper
                                                                    :ratio ratio
                                                                    :monochrome monochrome))
             (open-file-dialog app
                               :save
                               nil
                               format))))
    (open-choice-prompt app
                        (plot-format-title format)
                        (i18n:tr "Print in black, or in the color of each layer? Very light colors are printed black.")
                        (list (cons (i18n:tr "Black") (choose t))
                              (cons (i18n:tr "Layer colors") (choose nil))
                              (cons (i18n:tr "Cancel") nil)))))

(defun ask-for-plot-scale (app
                           format
                           paper
                           &optional problem)
  "Ask for the scale, as text, and go on to the question about colors; a text that is not a scale is said and the
question asked again.

APP: the application.
FORMAT: :pdf or :svg.
PAPER: the sheet chosen.
PROBLEM: what was wrong with the last answer, or NIL."
  (let ((instructions
          (i18n:tr "Type the scale, such as 1:50, or fit to make the drawing as large as the sheet allows.")))
    (open-text-prompt app
                      (plot-format-title format)
                      (if problem
                          (format nil "~a ~a" problem instructions)
                          instructions)
                      (remembered-scale-text app)
                      (lambda (text)
                        (handler-case (ask-for-plot-colors app
                                                           format
                                                           paper
                                                           (plot:parse-plot-scale text))
                          (base:invalid-input (condition)
                            (ask-for-plot-scale app
                                                format
                                                paper
                                                (base:invalid-input-reason condition))))))))

(defun begin-plot-export (app
                          format)
  "Start exporting the drawing as a sheet: ask which sheet, then the scale and the colors, then the file name.

APP: the application.
FORMAT: :pdf or :svg."
  (if (zerop (dom:drawing-entity-count (app-drawing app)))
      (set-message app
                   (i18n:tr "There is nothing to export: the drawing is empty.")
                   :error)
      (open-choice-prompt app
                          (plot-format-title format)
                          (i18n:tr "Choose the size of the sheet. The drawing goes in the middle of it.")
                          (append (loop for paper in *plot-paper-choices*
                                        collect (let ((paper paper))
                                                  (cons (plot:paper-name paper)
                                                        (lambda ()
                                                          (ask-for-plot-scale app
                                                                              format
                                                                              paper)))))
                                  (list (cons (i18n:tr "Cancel") nil))))))

(defun operation-export-pdf (app)
  "Ask how the drawing is to be put on the sheet, then open the dialog where the user picks a folder and a name, and
export it there as a PDF file.

APP: the application."
  (begin-plot-export app :pdf))

(defun operation-export-svg (app)
  "Ask how the drawing is to be put on the sheet, then open the dialog where the user picks a folder and a name, and
export it there as an SVG file.

APP: the application."
  (begin-plot-export app :svg))
