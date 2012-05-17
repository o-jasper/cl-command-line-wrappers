
(defsystem :cl-ps-command
  :depends-on (:j-string-utils :j-commandline :alexandria)
  :description "Pretty thin layer around the ps command.
 (C-lib FFI would be better)

Warning see `+ps-allowed+`"
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "cl-ps-command")))))
