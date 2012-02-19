
(defsystem :cl-pinot-command
  :depends-on (:j-seq-utils :j-string-utils :external-program :alexandria)
  :description "Functions to parse the pinot standard output.

Pinot is a search engine."
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "pinot-parse")))))
