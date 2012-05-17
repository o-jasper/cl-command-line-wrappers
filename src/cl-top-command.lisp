;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-top-command
  (:use :common-lisp :j-commandline :j-general :j-string-utils :j-seq-utils)
  (:export top top-line)
  (:documentation "Translates command top output into something more lispy.
 (See cl-ps-command for the ps command.)"))

(in-package :cl-top-command)

(defun wh (ch) (case ch ((#\Newline #\Space) t)))

(defun parse-int (str) ;TODO parse-number. Parse time proper.
  (parse-integer str :junk-allowed t))
(defun parse-int-kmg (str)
  "Parse part with potential powers of ten."
  (multiple-value-bind (n index) (parse-int str)
    (if (= (length str) index)
      n (* n (expt 1024 (case (aref str index)
			  (#\k 1) (#\m 2) (#\g 3) (#\t 4) 
			  (t (warn "got ~a" str) 0)))))))

(defun top-line (line)
  "Handle a line with program info of `top` output, returns a plist with\
 values. (Compose this with your own if you want parsing done for you.)"
  (declare (type string line))
  (let ((tok (tokenize-str line "" #'wh 12)))
    (declare (type list tok))
    (assert (= (length tok) 12) nil
	    "Incorrect length from ~s" line)
    (destructuring-bind 
	  (pid user pr ni 
	       virt res shr s %cpu %mem time+ command) tok
      (append ;TODO `pr` an `ni` are always integers?
       (plist (parse-int pid) user pr ni
	      (parse-int-kmg virt) (parse-int-kmg res) 
	      (parse-int-kmg shr) s (parse-int %cpu)
	      (parse-int %mem) time+)
       (list :command (subseq-upto-not #\Space command :from-end t))))))

;(defun top-header () .. TODO

(defun top (&key (line-hook #'top-line) (user "") 
	         (upto-cnt most-positive-fixnum)
	         (top-command "top") (top-info 7))
  "Make top output lispy. TODO also eat the first bit of info."
  (declare (type (function (string) t) line-hook) 
	   (type string user) (type fixnum upto-cnt))
  (let ((i 0))
    (flet ((handle-line (line)
	     (declare (type string line))
	     (setq i (+ i 1))
	     (when (>= i (+ upto-cnt top-info))
	       (return-from handle-line (values nil t)))
	     (when (string= line "") ;TODO what to do with this?
	       (return-from handle-line)) 
	     (funcall line-hook line)))
      (with-input-from-string
	  (stream (command-str top-command " -b -n1"
		   (if (string= user "") "" (concat "-u" user))))
 ;TODO not reading the header at the moment.
	(dotimes (k top-info) (read-line stream))
	(line-by-line stream #'handle-line)))))
