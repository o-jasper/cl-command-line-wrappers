;;
;;  Copyright (C) 30-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :j-commandline
  (:use :common-lisp :j-string-utils
	#-clisp :trivial-shell)
  (:export command command-str)
  (:documentation "Provides basic way to run external commands
 from using clisp or trivial-shell.

 (Specific control of the streams not included try external-program for\
 that."))

(in-package :j-commandline)

(defun command-str (&rest input)
  "Run command string.
 (Arguments are turned into strings with princ and concatenated.)"
  (let ((str (concat-princ-list input)))
    #+clisp (ext:run-shell-command str)
    #-clisp (trivial-shell:shell-command str)))

(defun command (str &rest args)
  "Run a formatted command."
  (command-str (apply #'format `(nil ,str ,@args))))
