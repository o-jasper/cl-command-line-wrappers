
(defsystem :j-commandline
  :depends-on (:trivial-shell :j-string-utils)
  :description "Provides way to basic get arguments and external commands
 from clisp or unix-options/trivial-shell.

 (Specific control of the streams not included try external-program for
 that."
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "j-commandline")))))
