
(defsystem :cl-wmctrl
  :depends-on (:j-string-utils :j-commandline)
  :description "Thin layer around wmctrl command.
 (C-lib FFI might be better, but seems to work fine.)"
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "cl-wmctrl")))))
