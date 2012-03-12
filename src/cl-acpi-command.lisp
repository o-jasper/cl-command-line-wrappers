;;
;;  Copyright (C) 10-03-2012 Jasper den Ouden.
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

(defun expect (cond) (assert cond))

(defun acpi-line (line)
  "Handle a single acpi output line."
  (declare (type string line))
  (destructuring-regex ;TODO this confuses the compiler or something?
      ((:keyword name) " " (:int nr)
       ": "("Full|Charging|Discharging|design capacity|on-line|\
ok|trip point|active|LCD|Processor|Fan" info) " |, " rest) line
    (let*((result (make-instance 'acpi:acpi :kind name :nr nr))
	  (info   (intern (string-upcase info) :keyword)))
      (case info
	((:full :charging :discharging)
	 (expect (eql name :battery))
	 (destructuring-regex ((:uint* val) saying) rest
	   (declare (ignore saying)) ;TODO interpret it.
	   (change-class result 'acpi:charge-state
	     :state info :fraction val)))
	(:|DESIGN CAPACITY|
	  (expect (eql name :battery))
	  (destructuring-regex 
	      ((:uint* capacity) " mAh, last full capacity "
	       (:uint* last-full) " mAh = " (:uint* p) "%") rest
	    (declare  (ignore p))
	    (change-class result 'acpi:charge-capacity
	      :design-capacity capacity :last-full-capacity last-full)))
	(:on-line
	 (expect (eql name :adapter))
	 (change-class result 'acpi:on-line))
	((:ok :active)
	 (expect (eql name :thermal))
	 (destructuring-regex ((:num temp)
			       " +degrees +" ("C|F" unit)) rest
	    (assert unit)
	    (change-class result 'acpi:thermal-state
	       :state info :temp temp
	       :temp-unit (intern unit :keyword))))
	(:|TRIP POINT|
	  (destructuring-regex
	      ((:uint* nr)
	       " +switches to mode +" ("passive|active|critical" mode)
	       " +at temperature +" (:num threshhold)
	       " +degrees +" ("C|F" unit)) rest
	    (change-class result 'acpi:thermal-trip
	      :nr nr :to-mode (intern (rev-case mode) :keyword)
	      :temp threshhold :temp-unit (intern unit :keyword))))
	((:LCD :fan :processor)
	 (destructuring-regex ((:int n) " +of +" (:int m)) rest
	   (change-class result 'acpi:cooling :of info
	     :cooling-cnt n :total-cnt m)))
	(t
	 result)))))

(defun acpi (&optional from/get (line-fun #'acpi-line))
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
      (null    (re :everything))
      (keyword (re (command-str "acpi --" (prep-arg from/get))))
      (list    (re (command-str
		      "acpi" (reduce (curry #'concat "--")
				     (mapcar #'prep-arg from/get)
				     :initial-value ""))))
      (t       (error "What sort of argument is ~a?" from/get)))))
