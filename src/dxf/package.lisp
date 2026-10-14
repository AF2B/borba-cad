(defpackage #:borba-cad/dxf
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain)
                    (#:persist #:borba-cad/persistence)
                    (#:sbcl #:borba-cad/sbcl-support))
  (:documentation "Import and export of drawings in the ASCII DXF format: a reader of the groups of a DXF
file, the mapping of its entities to the entities of a drawing with a report of what could not be
mapped, and a writer of a DXF file of the oldest widely read version (AutoCAD R12).")
  (:export
   ;; Colors
   #:aci->rgb
   #:rgb->aci
   ;; Reading
   #:+maximum-groups+
   #:record
   #:record-type
   #:record-pairs
   #:record-line
   #:record-children
   #:dxf-document
   #:dxf-document-header
   #:dxf-document-layers
   #:dxf-document-blocks
   #:dxf-document-entities
   #:parse-dxf-text
   #:decode-dxf-octets
   #:document-units
   ;; Import
   #:dxf-report
   #:make-dxf-report
   #:report-imported
   #:report-ignored
   #:report-notes
   #:report-imported-total
   #:report-ignored-total
   #:report-summary
   #:import-document
   ;; Export
   #:export-drawing-text
   #:export-summary
   ;; Files
   #:read-dxf-file
   #:load-dxf
   #:save-dxf))
