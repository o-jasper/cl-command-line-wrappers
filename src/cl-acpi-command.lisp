;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-acpi-command
  (:use :common-lisp :alexandria :j-commandline :j-string-utils 
	:destructuring-regex)
  (:export acpi acpi-line)
  (:documentation "Wrapper around `acpi` program output.\
 (TODO: cffi libacpi, rename cl-top, cl-wmctrl, iw-scan?)"))

(in-package :cl-acpi-command)

(defun acpi-line (line)
  "Handle a single acpi output line."
  (declare (type string line))
  (destructuring-regex ;TODO this confuses the compiler or something?
      ((name :keyword) " " (nr :int)
       ": " (info "Full|Charging|Discharging|design capacity|on-line|ok|trip point|active|LCD|Processor|Fan") " |, " rest) line
    `(,name :nr ,nr
      ,@(let*((info   (intern (string-upcase info) :keyword)))
	  (case info
	    ((:full :charging :discharging)
	     (destructuring-regex ((val :uint*) saying) rest
	       `(:state ,info :fraction ,val :saying ,saying)))
	    (:|DESIGN CAPACITY|
	      (destructuring-regex 
		  ((capacity :uint*) "mAh, last full capacity"
		   (last-full :uint*) "mAh = " (p :uint*) "%") rest
		p ;Can't ignore yet. (a todo of destructuring-regex)
		`(:design-capacity ,capacity :last-full-capacity ,last-full)))
	    (:on-line
	     '(:on-line t))
	    ((:ok :active)
	     (destructuring-regex ((temp :num)
				   " +degrees +" (unit "C|F")) rest
	       (assert unit)
	       `(:state ,info :temp ,temp 
			:temp-unit ,(intern unit :keyword))))
	    (:|TRIP POINT|
	      (destructuring-regex
		  ((nr :uint*)
		   " +switches to mode +" (mode "passive|active|critical")
		   " +at temperature +" (threshhold :num)
		   " +degrees +" (unit "C|F")) rest
		`(:nr ,nr :to-mode ,(intern (rev-case mode) :keyword)
		  :temp ,threshhold :temp-unit ,(intern unit :keyword))))
	    ((:LCD :fan :processor)
	     (destructuring-regex ((n :int) " +of +" (m :int)) rest
	       `(:of ,info :cooling-cnt ,n :total-cnt ,m))))))))

(defun acpi
    (&optional (from/get :everything) (line-fun #'acpi-line))
  "Return the 'reworked' results from the acpi command.
If the argument is a:
stream/pathname:  read stream/filename as if acpi output.
string:           read _the string_ as acpi output(not the file!)
list:             Take the elements as arguments to the `acpi` command
null(empty list): `acpi --everything`
keyword           Single command."
  (flet ((prep-arg (el)
	   (string-downcase (princ-to-string el)))
	 (re (from/get) (acpi from/get line-fun)))
    (typecase from/get
      (stream  (line-by-line from/get line-fun))
      (string  (with-input-from-string (stream from/get)
		 (re stream)))
      (keyword (re (command-str "acpi --" (prep-arg from/get))))
      (list    (re (command-str
		      "acpi" (reduce (curry #'concat "--")
				     (mapcar #'prep-arg from/get)
				     :initial-value ""))))
      (t       (error "What sort of argument is ~a?" from/get)))))
