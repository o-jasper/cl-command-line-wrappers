
(defsystem :cl-pinot-command
  :depends-on (:cl-fad :j-seq-utils :j-string-utils :destructuring-regex :external-program :alexandria)
  :description "Functions to parse the pinot standard output.

Pinot is a search engine. https://code.google.com/p/pinot-search/"
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "pinot-parse")))))
