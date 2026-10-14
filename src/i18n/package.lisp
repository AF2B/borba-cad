(defpackage #:borba-cad/i18n
  (:use #:cl)
  (:documentation "The languages of the interface. The English text of the program is the key of every
message, and a catalog gives the translation of a key; English (US) is the default and needs no
catalog, and a text with no translation shows in English.")
  (:export
   #:+default-language+
   #:*language*
   #:supported-languages
   #:language-tag
   #:language-name
   #:parse-language
   #:tr
   #:trf
   #:mark
   #:with-language
   #:catalog-translations
   #:catalog-language-p
   #:format-directives))
