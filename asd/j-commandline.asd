
(defsystem :j-commandline
  :depends-on (:trivial-shell :j-string-utils)
  :description "Provides basic way to run external commands
 from using clisp or trivial-shell.

 (Specific control of the streams not included try external-program for
 that."
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "j-commandline")))))
