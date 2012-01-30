
(defsystem :cl-top-command
  :depends-on (:j-seq-utils :j-string-utils :j-general :j-commandline)
  :description "Translates command top output into something more lispy.
 (See cl-ps-command for the ps command.)"
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "cl-top-command")))))
