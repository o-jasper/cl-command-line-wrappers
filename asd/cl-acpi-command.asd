
(defsystem :cl-acpi-command
  :depends-on (:cl-acpi-classes :regex-sequence :j-parse-number :j-string-utils :j-commandline :alexandria)
  :description "Wrapper around `acpi` program output.
 (TODO: cffi libacpi, rename cl-top, cl-wmctrl, iw-scan?)"
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "cl-acpi-command")))))
