
(defsystem :cl-acpi-classes
  :depends-on (:defclass-alt)
  :description "Classes for :cl-acpi-command, was looking for a way to properly
document, classes can do it."
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "cl-acpi-classes")))))
