
(defsystem :cl-acpi-command
  :depends-on (:cl-acpi-classes :destructuring-regex :j-string-utils :j-commandline :alexandria)
  :description "Wrapper around `acpi` program output.
 (TODO: cffi libacpi, rename cl-top, cl-wmctrl, iw-scan?)"
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "cl-acpi-command")))))
