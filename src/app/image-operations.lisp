(in-package #:borba-cad/app)

;;;; Exporting what the canvas shows as a picture. The window program makes the picture (it is the one that
;;;; has the pixels) and the application encodes it, saves it and says how it went.

(defparameter *image-extensions* (list "jpg" "jpeg")
  "Extensions of the picture files that the dialog lists.")

(defun image-file-path (text)
  "Return the pathname of the picture file that TEXT names, with the extension .jpg added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    (first *image-extensions*)
                                    *image-extensions*))

(defun export-image-from-app (app
                              path-text)
  "Save what the canvas shows as a JPEG file at PATH-TEXT (.jpg is added to a name without an extension), and
tell the user how it went in a modal. The picture is the canvas as it is on the screen, without the panels,
the selection and the cursor. On failure the previous file is untouched. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds))
        (path (image-file-path path-text)))
    (handler-case
        (multiple-value-bind (pixels width height)
            (let ((capturer (app-image-capturer app)))
              (unless capturer
                (error 'base:drawing-file-io-error
                       :path (uiop:native-namestring path)
                       :reason (i18n:tr "cannot be made: this window cannot make pictures")))
              (funcall capturer))
          (unless pixels
            (error 'base:drawing-file-io-error
                   :path (uiop:native-namestring path)
                   :reason (i18n:tr "cannot be made: there is nothing on the canvas to take a picture of")))
          (multiple-value-bind (saved stats)
              (persist:save-binary-file path
                                        (image:encode-jpeg width
                                                           height
                                                           pixels))
            (obs:record-operation-result (app-telemetry app)
                                         "file.export-image"
                                         (- (sbcl:monotonic-nanoseconds) started)
                                         :info
                                         :bytes (getf stats :bytes)
                                         :width width
                                         :height height
                                         :result :success)
            (show-notice app
                         (i18n:tr "Image exported")
                         (i18n:trf "~a was saved (~a). It shows what is on the canvas, ~d x ~d pixels."
                                   (file-namestring saved)
                                   (base:format-byte-size (getf stats :bytes))
                                   width
                                   height))
            t))
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-export-operation app
                               "file.export-image"
                               started
                               condition)))))

(defun operation-export-image (app)
  "Open the dialog where the user picks a folder and a name, and export what the canvas shows there as a JPEG.

APP: the application."
  (open-file-dialog app
                    :save
                    nil
                    :image))

(defparameter *underlay-extensions* (list "png" "jpg" "jpeg" "pdf")
  "The extensions of the files an underlay may be made from.")

(defun underlay-file-path (text)
  "Return the pathname of the picture that TEXT names, with the extension of a picture added when it has none.

TEXT: a file name as typed by the user."
  (persist:file-path-with-extension text
                                    (first *underlay-extensions*)
                                    *underlay-extensions*))
