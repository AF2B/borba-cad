(defpackage #:borba-cad/persistence
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:dom #:borba-cad/domain)
                    (#:sbcl #:borba-cad/sbcl-support))
  (:documentation "The .bcad drawing format: a small safe S-expression reader, a writer whose
numbers round-trip exactly, strict validation, and atomic saving.")
  (:export
   #:+supported-format-version+
   #:+maximum-file-bytes+
   #:+maximum-nesting-depth+
   #:+maximum-token-length+
   #:write-drawing
   #:drawing-to-string
   #:parse-drawing-text
   #:native-path
   #:save-drawing
   #:load-drawing
   #:format-double))
