
(defsystem :cl-wmctrl
  :depends-on (:alexandria :j-string-utils :j-commandline)
  :description "Thin layer around wmctrl command.
 (C-lib FFI might be better, but seems to work fine.)"
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "cl-wmctrl")))))
