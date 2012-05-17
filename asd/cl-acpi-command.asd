
(defsystem :cl-acpi-command
  :depends-on (:destructuring-regex :j-string-utils :j-commandline :alexandria)
  :description "Wrapper around `acpi` program output.
 (TODO: cffi libacpi, rename cl-top, cl-wmctrl, iw-scan?)"
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "cl-acpi-command")))))
