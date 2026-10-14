(defpackage #:borba-cad/persistence
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n)
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
   #:clean-path-text
   #:native-path
   #:drawing-file-path
   #:directory-of
   #:default-drawings-directory
   #:user-directories-entry
   #:load-recent-files
   #:remember-recent-file
   #:+recent-file-limit+
   #:load-settings
   #:load-setting
   #:save-setting
   #:*drawing-extension*
   #:*known-drawing-extensions*
   #:save-drawing
   #:load-drawing
   #:format-double))
