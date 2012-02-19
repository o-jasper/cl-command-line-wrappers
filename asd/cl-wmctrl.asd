
(defsystem :cl-wmctrl
  :depends-on (:j-string-utils :j-commandline :alexandria)
  :description "Thin layer around wmctrl command.
 (C-lib FFI might be better, but seems to work fine.)"
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "cl-wmctrl")))))
