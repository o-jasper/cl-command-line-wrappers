
(defsystem :cl-ps-command
  :depends-on (:j-string-utils :j-commandline)
  :description "Pretty thin layer around the ps command.
 (C-lib FFI would be better)"
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "cl-ps-command")))))
