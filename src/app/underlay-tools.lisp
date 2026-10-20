(in-package #:borba-cad/app)

;;;; Underlays in the application: a PNG or JPEG file, or a page of a PDF, put under the drawing to be traced over,
;;;; and calibrated by two points of the picture whose distance is known. Adding one reads the file, asks which page
;;;; of a PDF and at what resolution, puts the picture in the middle of the view and leaves the calibration tool
;;;; waiting for its first point. The pictures are read once and kept by number, as pixels the renderer draws; an
;;;; underlay is saved with the drawing, as the file it came from, and is read again when the drawing is opened.

(defconstant +millimeters-per-inch+ 25.4d0
  "Millimetres in an inch, which turns the resolution of a PDF page into the size of its pixels.")

(defconstant +underlay-view-share+ 0.8d0
  "How much of the view, along the side that limits it, a picture that has no size of its own fills when added.")

(defconstant +percent+ 100d0
  "The number that a share is multiplied by to say it as a percent.")

(defconstant +pixel-size-digits+ 6
  "Decimals of the size of a pixel, in millimetres, that the settings of an underlay show.")

(defconstant +underlay-form-digits+ 4
  "Decimals of the opacity and the turn of an underlay that the settings show.")

(defconstant +minimum-pdf-dpi+ 36
  "The lowest resolution a page of a PDF may be read at, in dots per inch.")

(defconstant +maximum-pdf-dpi+ 600
  "The highest resolution a page of a PDF may be read at, in dots per inch.")

(defconstant +default-pdf-dpi+ 150
  "The resolution shown for a page of a PDF that has none recorded.")

;;; The pictures that are read

(defstruct (picture-entry (:constructor make-picture-entry (key
                                                            raster
                                                            signature
                                                            problem)))
  "What is known of the picture of an underlay: the number the renderer knows its pixels by, the pixels, what it was
read from (to tell when the underlay shows another picture) and, when it could not be read, why.

KEY: the number of the pixels.
RASTER: the pixels, or NIL when it could not be read.
SIGNATURE: the file, the page and the resolution it was read from.
PROBLEM: a sentence that says why it could not be read, or NIL."
  key raster signature problem)

(defun underlay-signature (underlay)
  "Return what the picture of an underlay is read from: its file, its page and its resolution.

UNDERLAY: the underlay."
  (list (dom:underlay-data underlay)
        (dom:underlay-page underlay)
        (dom:underlay-dpi underlay)))

(defun same-signature-p (first
                         second)
  "Return true when two signatures name the same picture.

FIRST: a signature.
SECOND: a signature."
  (and (eq (first first) (first second))
       (eql (second first) (second second))
       (eql (third first) (third second))))

(defun cache-picture (app
                      underlay
                      raster
                      problem)
  "Keep the picture of an underlay, or the reason it could not be read, under a new number.

APP: the application.
UNDERLAY: the underlay.
RASTER: the pixels, or NIL.
PROBLEM: a sentence, or NIL."
  (setf (gethash (dom:underlay-id underlay) (app-pictures app))
        (make-picture-entry (incf (app-picture-counter app))
                            raster
                            (underlay-signature underlay)
                            problem)))

(defun read-underlay-picture (app
                              underlay)
  "Read the picture of an underlay from its file and keep it. A picture that cannot be read is kept as the reason, and
said in the status bar once.

APP: the application.
UNDERLAY: the underlay."
  (handler-case
      (multiple-value-bind (raster dpi)
          (image:read-picture (dom:underlay-data underlay)
                              :page (dom:underlay-page underlay)
                              :dpi (dom:underlay-dpi underlay))
        (declare (ignore dpi))
        (cache-picture app
                       underlay
                       raster
                       nil))
    (base:image-error (condition)
      (let ((problem (remove #\Newline (princ-to-string condition))))
        (set-message app
                     (i18n:trf "The underlay ~a cannot be shown: ~a"
                               (dom:underlay-name underlay)
                               problem)
                     :error)
        (cache-picture app
                       underlay
                       nil
                       problem)))))

(defun underlay-picture (app
                         underlay)
  "Return the picture of an underlay, reading it first when it has not been read or the underlay shows another picture
now.

APP: the application.
UNDERLAY: the underlay."
  (let ((entry (gethash (dom:underlay-id underlay) (app-pictures app))))
    (if (and entry (same-signature-p (picture-entry-signature entry) (underlay-signature underlay)))
        entry
        (read-underlay-picture app underlay))))

(defun correct-underlay-size (app
                              underlay
                              raster)
  "Return the underlay with the size of its picture as it was read, which the drawing is given when the file said
another size; the underlay as it was when the size was right.

APP: the application.
UNDERLAY: the underlay.
RASTER: the pixels of its picture."
  (if (and (= (dom:underlay-pixel-width underlay) (image:raster-width raster))
           (= (dom:underlay-pixel-height underlay) (image:raster-height raster)))
      underlay
      (let ((corrected (dom:underlay-with-picture underlay
                                                  :pixel-width (image:raster-width raster)
                                                  :pixel-height (image:raster-height raster))))
        (dom:replace-underlay (app-drawing app) corrected)
        corrected)))

(defun scene-underlays (app)
  "Return the underlays that show and whose pictures could be read, lowest first, each as a list of the underlay, the
number of its pixels and the pixels, for the scene.

APP: the application."
  (loop for underlay in (dom:drawing-underlays (app-drawing app))
        when (dom:underlay-visible underlay)
          append (let ((entry (underlay-picture app underlay)))
                   (when (picture-entry-raster entry)
                     (list (list (correct-underlay-size app
                                                        underlay
                                                        (picture-entry-raster entry))
                                 (picture-entry-key entry)
                                 (picture-entry-raster entry)))))))

;;; Where a picture goes

(defun view-bounds (app)
  "Return the part of the drawing that the canvas shows, as bounds.

APP: the application."
  (view:viewport-visible-bounds (app-viewport app)))

(defun fitted-pixel-size (app
                          pixel-width
                          pixel-height)
  "Return how many millimetres a pixel of a picture is so that the picture fills most of the view.

APP: the application.
PIXEL-WIDTH: width of the picture in pixels.
PIXEL-HEIGHT: height of the picture in pixels."
  (let ((bounds (view-bounds app)))
    (max dom:+minimum-underlay-scale+
         (min dom:+maximum-underlay-scale+
              (* +underlay-view-share+
                 (min (/ (geo:bounds-width bounds) pixel-width)
                      (/ (geo:bounds-height bounds) pixel-height)))))))

(defun new-underlay (app
                     name
                     kind
                     bytes
                     page
                     dpi
                     raster)
  "Return the underlay that a picture just read makes, standing in the middle of the view: the size a page of a PDF has
on paper, and for other pictures one that fills most of the view.

APP: the application.
NAME: what it is called.
KIND: :png, :jpeg or :pdf.
BYTES: the file.
PAGE: which page, for a PDF.
DPI: the resolution a PDF page was read at, or NIL.
RASTER: the pixels."
  (let* ((width (image:raster-width raster))
         (height (image:raster-height raster))
         (scale (if dpi
                    (/ +millimeters-per-inch+ dpi)
                    (fitted-pixel-size app
                                       width
                                       height)))
         (bounds (view-bounds app)))
    (dom:make-underlay :name name
                       :kind kind
                       :data bytes
                       :page page
                       :dpi dpi
                       :pixel-width width
                       :pixel-height height
                       :x (- (geo:bounds-center-x bounds) (/ (* width scale) 2))
                       :y (- (geo:bounds-center-y bounds) (/ (* height scale) 2))
                       :scale scale)))

(defun zoom-to-underlay (app
                         underlay)
  "Fit the view to an underlay.

APP: the application.
UNDERLAY: the underlay."
  (setf (app-viewport app) (view:fit-viewport-to-bounds (app-viewport app) (dom:underlay-bounds underlay)))
  (app-request-redraw app))

;;; Adding one

(defun finish-adding-underlay (app
                               path
                               bytes
                               page
                               dpi
                               started)
  "Read the picture and put it under the drawing, in the middle of the view, and wait for the two points of the
calibration. Returns true on success.

APP: the application.
PATH: the file, for its name.
BYTES: the file.
PAGE: which page, for a PDF.
DPI: the resolution wanted for a PDF page, or NIL to let the program choose.
STARTED: the monotonic time at which the operation began."
  (handler-case
      (multiple-value-bind (raster used-dpi)
          (image:read-picture bytes
                              :page page
                              :dpi dpi)
        (let* ((underlay (dom:add-underlay (app-drawing app)
                                           (new-underlay app
                                                         (file-namestring path)
                                                         (image:picture-kind bytes)
                                                         bytes
                                                         page
                                                         used-dpi
                                                         raster)))
               (entry (cache-picture app
                                     underlay
                                     raster
                                     nil)))
          (declare (ignore entry))
          (zoom-to-underlay app underlay)
          (start-calibration app underlay)
          (set-message app
                       (i18n:trf "Added ~a. To set its scale, click two points of the picture whose distance you know."
                                 (dom:underlay-name underlay)))
          t))
    ((or base:drawing-file-error base:invalid-input base:image-error base:edit-refused) (condition)
      (fail-file-operation app
                           "underlay.add"
                           started
                           condition))))

(defun parse-page-and-resolution (values
                                  page-count)
  "Return the page and the resolution that the form of a PDF asks for, as two values: the resolution is NIL when the
program is to choose it. Signals INVALID-INPUT when either is not usable.

VALUES: the alist of the form.
PAGE-COUNT: how many pages the PDF has."
  (let* ((page-text (form-value values :page))
         (page (base:parse-decimal page-text))
         (dpi-text (string-trim " " (form-value values :resolution))))
    (unless (and (= page (round page)) (<= 1 page page-count))
      (error 'base:invalid-input
             :text page-text
             :reason (i18n:trf "a page from 1 to ~d" page-count)))
    (values (round page)
            (if (or (zerop (length dpi-text)) (string-equal dpi-text "auto")
                    (string-equal dpi-text (i18n:tr "auto")))
                nil
                (let ((dpi (base:parse-decimal dpi-text)))
                  (unless (<= +minimum-pdf-dpi+ dpi +maximum-pdf-dpi+)
                    (error 'base:invalid-input
                           :text dpi-text
                           :reason (i18n:trf "a resolution from ~d to ~d dots per inch"
                                             +minimum-pdf-dpi+
                                             +maximum-pdf-dpi+)))
                  dpi)))))

(defun ask-pdf-page (app
                     path
                     bytes
                     page-count
                     started)
  "Ask which page of a PDF to put under the drawing and at what resolution, and go on when it is answered.

APP: the application.
PATH: the file.
BYTES: the file.
PAGE-COUNT: how many pages it has.
STARTED: the monotonic time at which the operation began."
  (open-form-prompt app
                    (i18n:tr "Underlay: PDF page")
                    (format nil
                            "~a ~a"
                            (i18n:trf "Pages in ~a: ~d."
                                      (file-namestring path)
                                      page-count)
                            (i18n:tr "\"auto\" reads a scan at its own resolution and a drawing of lines at 150."))
                    (list (list :page
                                (i18n:tr "Page")
                                "1")
                          (list :resolution
                                (i18n:tr "Resolution (dots per inch)")
                                (i18n:tr "auto")))
                    (lambda (values)
                      (handler-case
                          (multiple-value-bind (page dpi)
                              (parse-page-and-resolution values page-count)
                            (if (finish-adding-underlay app
                                                        path
                                                        bytes
                                                        page
                                                        dpi
                                                        started)
                                nil
                                (i18n:tr "That page could not be read.")))
                        (base:invalid-input (condition)
                          (remove #\Newline (princ-to-string condition)))))))

(defun add-underlay-from-app (app
                              path-text)
  "Read the picture or PDF at PATH-TEXT and put it under the drawing. On failure the drawing is untouched and the
reason is shown. Returns true on success, or when the question about a PDF was asked.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (let* ((path (resolve-open-path path-text #'underlay-file-path))
               (bytes (persist:read-file-octets path))
               (kind (image:picture-kind bytes)))
          (cond ((null kind)
                 (error 'base:image-error :reason (i18n:tr "This file is not a PNG, JPEG or PDF picture.")))
                ((eq kind :pdf)
                 (ask-pdf-page app
                               path
                               bytes
                               (image:picture-page-count bytes)
                               started)
                 t)
                (t
                 (finish-adding-underlay app
                                         path
                                         bytes
                                         1
                                         nil
                                         started))))
      ((or base:drawing-file-error base:invalid-input base:image-error) (condition)
        (fail-file-operation app
                             "underlay.add"
                             started
                             condition)))))

(defun operation-add-underlay (app)
  "Open the dialog where the user picks a picture or a PDF to put under the drawing.

APP: the application."
  (close-menu app)
  (open-file-dialog app
                    :open
                    nil
                    :underlay))

;;; Calibrating

(defclass calibrate-tool (tool)
  ((underlay-id :initarg :underlay-id :initform nil :accessor calibrate-underlay-id)
   (first-point :initform nil :accessor calibrate-first-point))
  (:documentation "Takes two clicks on a picture, where something is whose length is known, and asks what that length
is and how to turn the picture. UNDERLAY-ID is the underlay it works on."))

(defmethod tool-kind ((tool calibrate-tool)) :calibrate)

(defmethod tool-prompt ((tool calibrate-tool) app)
  (declare (ignore app))
  (if (calibrate-first-point tool)
      (i18n:tr "Calibrate: click the second point (Esc cancels)")
      (i18n:tr "Calibrate: click a point of the picture whose distance to another you know")))

(defmethod tool-in-progress-p ((tool calibrate-tool))
  (and (calibrate-first-point tool) t))

(defmethod tool-anchor ((tool calibrate-tool))
  (calibrate-first-point tool))

(defmethod tool-cancel ((tool calibrate-tool) app)
  (declare (ignore app))
  (setf (calibrate-first-point tool) nil))

(defmethod tool-pointer-move ((tool calibrate-tool) app cursor)
  (declare (ignore cursor))
  (when (calibrate-first-point tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool calibrate-tool) app)
  (let* ((first (calibrate-first-point tool))
         (cursor (app-cursor app))
         (second (and cursor (cons (cursor-raw-x cursor) (cursor-raw-y cursor)))))
    (when (and first second)
      (let ((line (measure-line app
                                (car first)
                                (cdr first)
                                (car second)
                                (cdr second))))
        (when line
          (remove nil
                  (list line
                        (measure-label app
                                       (/ (+ (car first) (car second)) 2)
                                       (/ (+ (cdr first) (cdr second)) 2)
                                       (length-text app (geo:distance (car first)
                                                                      (cdr first)
                                                                      (car second)
                                                                      (cdr second)))))))))))

(defun calibration-alignment-options ()
  "Return the choices of the line of a calibration that says how to turn the picture, as (key . label) pairs."
  (list (cons :keep (i18n:tr "Do not turn it"))
        (cons :horizontal (i18n:tr "Make the line level"))
        (cons :vertical (i18n:tr "Make the line upright"))
        (cons :angle (i18n:tr "Aim the line at an angle"))))

(defun calibration-from-form (app
                              underlay
                              first
                              second
                              values)
  "Return the underlay that a form of a calibration makes of another one, or signal an error that says why it cannot.

APP: the application.
UNDERLAY: the underlay.
FIRST: the first point, as an (x . y) cons.
SECOND: the second point.
VALUES: the alist of the form."
  (let ((alignment (form-value values :alignment)))
    (dom:calibrated-underlay underlay
                             (car first)
                             (cdr first)
                             (car second)
                             (cdr second)
                             (form-length app
                                          values
                                          :distance)
                             :alignment alignment
                             :angle (if (eq alignment :angle)
                                        (base:parse-decimal (form-value values :angle))
                                        0))))

(defun calibrate-underlay (app
                           underlay-id
                           first
                           second
                           values)
  "Calibrate an underlay as a form says. Returns NIL when it was done, or a sentence that says why it could not be, so
that the form stays.

APP: the application.
UNDERLAY-ID: the number of the underlay.
FIRST: the first point.
SECOND: the second point.
VALUES: the alist of the form."
  (handler-case
      (let ((underlay (dom:find-underlay (app-drawing app) underlay-id)))
        (unless underlay
          (error 'base:edit-refused :reason (i18n:tr "That underlay is no longer there.")))
        (let ((calibrated (calibration-from-form app
                                                 underlay
                                                 first
                                                 second
                                                 values)))
          (dom:replace-underlay (app-drawing app) calibrated)
          (operation-set-tool app :select)
          (app-request-redraw app)
          (set-message app
                       (i18n:trf "Calibrated ~a: the points are ~a apart; a pixel is ~a."
                                 (dom:underlay-name calibrated)
                                 (length-text app (form-length app
                                                               values
                                                               :distance))
                                 (length-text app (dom:underlay-scale calibrated))))
          nil))
    (base:borba-cad-error (condition)
      (remove #\Newline (princ-to-string condition)))))

(defun ask-calibration (app
                        underlay
                        first
                        second)
  "Ask how far apart the two points of the picture really are, and how to turn the picture.

APP: the application.
UNDERLAY: the underlay.
FIRST: the first point, as an (x . y) cons.
SECOND: the second point."
  (let ((measured (geo:distance (car first)
                                (cdr first)
                                (car second)
                                (cdr second))))
    (open-form-prompt app
                      (i18n:tr "Calibrate the underlay")
                      (i18n:trf "The two points are ~a apart now. Type how far apart they really are."
                                (length-text app measured))
                      (list (list :distance
                                  (i18n:tr "Real distance")
                                  "")
                            (list :alignment
                                  (i18n:tr "Turn")
                                  :keep
                                  (calibration-alignment-options))
                            (list :angle
                                  (i18n:tr "Angle of the line (degrees)")
                                  "0"))
                      (lambda (values)
                        (calibrate-underlay app
                                            (dom:underlay-id underlay)
                                            first
                                            second
                                            values))
                      :info (lambda (values)
                              (handler-case
                                  (let ((distance (form-length app
                                                               values
                                                               :distance)))
                                    (if (base:positive-length-p distance)
                                        (values (i18n:trf "A pixel will be ~a."
                                                          (length-text app
                                                                       (* (dom:underlay-scale underlay)
                                                                          (/ distance measured))))
                                                nil)
                                        (values (i18n:tr "The distance must be above zero.") t)))
                                (base:invalid-input (condition)
                                  (values (remove #\Newline (princ-to-string condition)) t)))))))

(defmethod tool-pointer-down ((tool calibrate-tool) app cursor)
  (let ((point (cons (cursor-raw-x cursor) (cursor-raw-y cursor)))
        (first (calibrate-first-point tool)))
    (cond ((null first)
           (if (dom:find-underlay (app-drawing app) (calibrate-underlay-id tool))
               (setf (calibrate-first-point tool) point)
               (set-message app
                            (i18n:tr "That underlay is no longer there.")
                            :error)))
          ((not (base:positive-length-p (geo:distance (car first)
                                                      (cdr first)
                                                      (car point)
                                                      (cdr point))))
           (set-message app
                        (i18n:tr "The two points must be apart.")
                        :error))
          (t
           (let ((underlay (dom:find-underlay (app-drawing app) (calibrate-underlay-id tool))))
             (if underlay
                 (ask-calibration app
                                  underlay
                                  first
                                  point)
                 (set-message app
                              (i18n:tr "That underlay is no longer there.")
                              :error))
             (setf (calibrate-first-point tool) nil))))
    (app-request-redraw app)))

(defun start-calibration (app
                          underlay)
  "Make the tool that calibrates an underlay the active one.

APP: the application.
UNDERLAY: the underlay to calibrate."
  (unless (dom:underlay-visible underlay)
    (set-underlay-visible app
                          underlay
                          t))
  (activate-tool app
                 (make-instance 'calibrate-tool :underlay-id (dom:underlay-id underlay))))

;;; The settings of an underlay

(defun yes-no-options ()
  "Return the two choices of a line that asks whether, as (key . label) pairs."
  (list (cons :yes (i18n:tr "Yes"))
        (cons :no (i18n:tr "No"))))

(defun settings-form-lines (app
                            underlay)
  "Return the lines of the form of the settings of an underlay, as the form takes them.

APP: the application.
UNDERLAY: the underlay."
  (append (list (list :name
                      (i18n:tr "Name")
                      (dom:underlay-name underlay))
                (list :visible
                      (i18n:tr "Shows")
                      (if (dom:underlay-visible underlay) :yes :no)
                      (yes-no-options))
                (list :opacity
                      (i18n:tr "Opacity (percent)")
                      (base:format-decimal (* +percent+ (dom:underlay-opacity underlay)) 0))
                (list :x
                      (i18n:tr "X of the lower left corner")
                      (length-text app (dom:underlay-x underlay)))
                (list :y
                      (i18n:tr "Y of the lower left corner")
                      (length-text app (dom:underlay-y underlay)))
                (list :rotation
                      (i18n:tr "Turn (degrees)")
                      (base:format-decimal (dom:underlay-rotation underlay) +underlay-form-digits+))
                (list :scale
                      (i18n:tr "Size of a pixel (millimetres)")
                      (base:format-decimal (dom:underlay-scale underlay) +pixel-size-digits+)))
          (when (eq :pdf (dom:underlay-kind underlay))
            (list (list :page
                        (i18n:tr "Page")
                        (princ-to-string (dom:underlay-page underlay)))
                  (list :resolution
                        (i18n:tr "Resolution (dots per inch)")
                        (base:format-decimal (or (dom:underlay-dpi underlay) +default-pdf-dpi+) 0))))))

(defun settings-number (values
                        original
                        key
                        parser
                        current)
  "Return the number that a line of the form of settings holds, or CURRENT when the line still holds what it was
shown with, so that opening the form and accepting it does not round what was not touched.

VALUES: the alist of the form.
ORIGINAL: the alist of what the lines were shown with.
KEY: the key of the line.
PARSER: function of the typed text that returns the number, or signals INVALID-INPUT.
CURRENT: the number the underlay has."
  (let ((typed (form-value values key)))
    (if (equal typed (form-value original key))
        current
        (funcall parser typed))))

(defun settings-without-picture-change (app
                                        underlay
                                        values
                                        original)
  "Return the underlay with the position, turn, size of a pixel, opacity, name and visibility that a form of settings
holds, leaving the picture as it is. Signals an error that says what is wrong.

APP: the application.
UNDERLAY: the underlay.
VALUES: the alist of the form.
ORIGINAL: the alist of what its lines were shown with."
  (flet ((length-parser (text)
           (base:parse-length text (display-unit-of app))))
    (dom:underlay-with underlay
                       :name (form-value values :name)
                       :visible (eq (form-value values :visible) :yes)
                       :opacity (settings-number values
                                                 original
                                                 :opacity
                                                 (lambda (text) (/ (base:parse-decimal text) +percent+))
                                                 (dom:underlay-opacity underlay))
                       :x (settings-number values
                                           original
                                           :x
                                           #'length-parser
                                           (dom:underlay-x underlay))
                       :y (settings-number values
                                           original
                                           :y
                                           #'length-parser
                                           (dom:underlay-y underlay))
                       :rotation (settings-number values
                                                  original
                                                  :rotation
                                                  #'base:parse-decimal
                                                  (dom:underlay-rotation underlay))
                       :scale (settings-number values
                                               original
                                               :scale
                                               #'base:parse-decimal
                                               (dom:underlay-scale underlay)))))

(defun settings-with-new-picture (app
                                  underlay
                                  values
                                  original)
  "Return the underlay of a form of settings that names another page or resolution of a PDF: the picture is read again,
and the underlay keeps its place and the size it has on paper. Returns the underlay and the new pixels as two values.
Signals an error that says what is wrong.

APP: the application.
UNDERLAY: the underlay.
VALUES: the alist of the form.
ORIGINAL: the alist of what its lines were shown with."
  (multiple-value-bind (page dpi)
      (parse-page-and-resolution values (image:picture-page-count (dom:underlay-data underlay)))
    (let ((changed (settings-without-picture-change app
                                                    underlay
                                                    values
                                                    original)))
      (if (and (= page (dom:underlay-page underlay))
               (eql dpi (dom:underlay-dpi underlay)))
          (values changed nil)
          (multiple-value-bind (raster used-dpi)
              (image:read-picture (dom:underlay-data underlay)
                                  :page page
                                  :dpi dpi)
            (values (dom:underlay-with-picture changed
                                               :page page
                                               :dpi used-dpi
                                               :pixel-width (image:raster-width raster)
                                               :pixel-height (image:raster-height raster)
                                               :scale (* (dom:underlay-scale changed)
                                                         (/ (or (dom:underlay-dpi underlay) used-dpi) used-dpi)))
                    raster))))))

(defun apply-underlay-settings (app
                                underlay
                                values
                                original)
  "Give an underlay the settings that a form holds. Returns NIL when it was done, or a sentence that says why it could
not be, so that the form stays.

APP: the application.
UNDERLAY: the underlay.
VALUES: the alist of the form.
ORIGINAL: the alist of what its lines were shown with."
  (handler-case
      (multiple-value-bind (changed raster)
          (if (eq :pdf (dom:underlay-kind underlay))
              (settings-with-new-picture app
                                         underlay
                                         values
                                         original)
              (settings-without-picture-change app
                                               underlay
                                               values
                                               original))
        (dom:replace-underlay (app-drawing app) changed)
        (when raster
          (cache-picture app
                         changed
                         raster
                         nil))
        (app-request-redraw app)
        (set-message app
                     (i18n:trf "Changed the settings of ~a." (dom:underlay-name changed)))
        nil)
    (base:borba-cad-error (condition)
      (remove #\Newline (princ-to-string condition)))))

(defun ask-underlay-settings (app
                              underlay)
  "Open the form where the position, turn, size of a pixel, opacity, name and visibility of an underlay are changed.

APP: the application.
UNDERLAY: the underlay."
  (let* ((lines (settings-form-lines app underlay))
         (original (loop for (key nil initial) in lines
                         collect (cons key initial))))
    (open-form-prompt app
                      (i18n:trf "Underlay: ~a" (dom:underlay-name underlay))
                      (i18n:tr "The picture stands by its lower left corner, and turns about it.")
                      lines
                      (lambda (values)
                        (apply-underlay-settings app
                                                 underlay
                                                 values
                                                 original)))))

;;; Choosing, showing, removing

(defun underlay-label (underlay)
  "Return the text a choice of underlays shows for one: its name, and that it is hidden when it is.

UNDERLAY: the underlay."
  (if (dom:underlay-visible underlay)
      (dom:underlay-name underlay)
      (i18n:trf "~a (hidden)" (dom:underlay-name underlay))))

(defun choose-underlay (app
                        title
                        message
                        action)
  "Run ACTION on the underlay of the drawing when there is one, and on the one the user picks in a prompt when there
are several; say so when there are none.

APP: the application.
TITLE: the heading of the prompt.
MESSAGE: what it explains.
ACTION: function of the underlay."
  (let ((underlays (dom:drawing-underlays (app-drawing app))))
    (cond ((null underlays)
           (set-message app
                        (i18n:tr "There are no underlays; add a picture or a PDF first.")))
          ((null (rest underlays))
           (funcall action (first underlays)))
          (t
           (open-choice-prompt app
                               title
                               message
                               (append (loop for underlay in underlays
                                             collect (let ((id (dom:underlay-id underlay)))
                                                       (cons (underlay-label underlay)
                                                             (lambda ()
                                                               (let ((current (dom:find-underlay (app-drawing app)
                                                                                                 id)))
                                                                 (when current
                                                                   (funcall action current)))))))
                                       (list (cons (i18n:tr "Cancel") nil))))))))

(defun set-underlay-visible (app
                             underlay
                             visible)
  "Show an underlay or hide it.

APP: the application.
UNDERLAY: the underlay.
VISIBLE: true to show it."
  (dom:replace-underlay (app-drawing app)
                        (dom:underlay-with underlay :visible visible))
  (app-request-redraw app))

(defun remove-underlay-from-app (app
                                 underlay)
  "Take an underlay away from the drawing, for good: it is not a step of undo.

APP: the application.
UNDERLAY: the underlay."
  (dom:remove-underlay (app-drawing app) (dom:underlay-id underlay))
  (remhash (dom:underlay-id underlay) (app-pictures app))
  (app-request-redraw app)
  (set-message app
               (i18n:trf "Removed ~a." (dom:underlay-name underlay))))

(defun confirm-removing-underlay (app
                                  underlay)
  "Ask whether to take an underlay away, since that cannot be undone.

APP: the application.
UNDERLAY: the underlay."
  (open-choice-prompt app
                      (i18n:tr "Remove the underlay")
                      (i18n:trf "Remove ~a from the drawing? This cannot be undone." (dom:underlay-name underlay))
                      (list (cons (i18n:tr "Remove")
                                  (lambda () (remove-underlay-from-app app underlay)))
                            (cons (i18n:tr "Cancel") nil))))

(defun ask-what-to-do-with-underlay (app
                                     underlay)
  "Ask what to do with an underlay: calibrate it, change its settings, show or hide it, go to it, or remove it.

APP: the application.
UNDERLAY: the underlay."
  (open-choice-prompt app
                      (underlay-label underlay)
                      (i18n:tr "What would you like to do with this underlay?")
                      (list (cons (i18n:tr "Calibrate")
                                  (lambda () (start-calibration app underlay)))
                            (cons (i18n:tr "Settings...")
                                  (lambda () (ask-underlay-settings app underlay)))
                            (cons (if (dom:underlay-visible underlay) (i18n:tr "Hide") (i18n:tr "Show"))
                                  (lambda ()
                                    (set-underlay-visible app
                                                          underlay
                                                          (not (dom:underlay-visible underlay)))))
                            (cons (i18n:tr "Zoom to it")
                                  (lambda () (zoom-to-underlay app underlay)))
                            (cons (i18n:tr "Remove...")
                                  (lambda () (confirm-removing-underlay app underlay)))
                            (cons (i18n:tr "Cancel") nil))))

(defun operation-underlays (app)
  "Ask which underlay to work on, when there are several, and what to do with it.

APP: the application."
  (close-menu app)
  (choose-underlay app
                   (i18n:tr "Underlays")
                   (i18n:tr "Which underlay?")
                   (lambda (underlay) (ask-what-to-do-with-underlay app underlay))))

(defun operation-calibrate-underlay (app)
  "Wait for the two points that calibrate an underlay: the only one, or the one the user picks.

APP: the application."
  (close-menu app)
  (choose-underlay app
                   (i18n:tr "Calibrate")
                   (i18n:tr "Which underlay?")
                   (lambda (underlay) (start-calibration app underlay))))

(defun operation-toggle-underlays (app)
  "Hide the underlays when any of them shows, and show them all when none does.

APP: the application."
  (close-menu app)
  (let* ((underlays (dom:drawing-underlays (app-drawing app)))
         (show (notany #'dom:underlay-visible underlays)))
    (if (null underlays)
        (set-message app (i18n:tr "There are no underlays; add a picture or a PDF first."))
        (progn (dolist (underlay underlays)
                 (set-underlay-visible app
                                       underlay
                                       show))
               (set-message app
                            (if show
                                (i18n:tr "The underlays show.")
                                (i18n:tr "The underlays are hidden.")))))))
