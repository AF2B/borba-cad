(in-package #:borba-cad/dxf)

;;;; Reading a DXF file from disk, and saving a drawing as one.

(defun read-dxf-file (path)
  "Read and parse the ASCII DXF file at PATH, and return its DXF-DOCUMENT. Signals a DRAWING-FILE-ERROR
subclass when the file cannot be read or is not a DXF file.

PATH: pathname of the file."
  (let ((file (merge-pathnames path)))
    (persist:check-open-source file)
    (handler-case
        (parse-dxf-text (decode-dxf-octets (persist:read-file-octets file)))
      (base:drawing-file-error (condition)
        (error 'base:drawing-file-error
               :path (uiop:native-namestring file)
               :reason (base:drawing-file-error-reason condition))))))

(defun load-dxf (path
                 &key units)
  "Read the DXF file at PATH and make a drawing from it. Returns the drawing and the DXF-REPORT of what was
imported and left out. Signals a DRAWING-FILE-ERROR subclass on any problem.

PATH: pathname of the file.
UNITS: the unit of its numbers (:millimeter, :centimeter, :meter, :inch or :foot), or NIL to take the one it
says, and millimetres when it says none."
  (import-document (read-dxf-file path) :units units))

(defun save-dxf (drawing
                 path)
  "Save DRAWING as an ASCII DXF file of version R12 at PATH, atomically as drawings are saved. Returns
three values: the saved pathname, the properties of the save (see PERSIST:SAVE-TEXT-FILE) and the
DXF-REPORT of what was written and what changed shape.

DRAWING: the drawing.
PATH: destination pathname."
  (multiple-value-bind (text report)
      (export-drawing-text drawing)
    (multiple-value-bind (target properties)
        (persist:save-text-file path
                                (lambda (stream) (write-string text stream))
                                :external-format :latin-1)
      (values target properties report))))
