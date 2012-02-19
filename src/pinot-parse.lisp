;;
;;  Copyright (C) 19-02-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-pinot-command
  (:use :common-lisp :alexandria :external-program :j-string-utils 
	:j-seq-utils)
  (:export pinot-stream parse-pinot-stream pinot-search
	   *search-engine* *search-db*)
  (:documentation "Functions to parse the pinot standard output.

Pinot is a search engine."))

(in-package :cl-pinot-command)

(defun from-homedir (str)
  (concat (namestring (user-homedir-pathname)) str))

(defvar *search-engine* :xapian "Default search engine.")
(defvar *search-db* (from-homedir ".pinot/") "Default db location.")

(defun pinot-stream (search-string &optional (search-engine *search-engine*) 
			    (search-db *search-db*))
  "Makes a raw stream outputting a search."
  (make-string-input-stream
   (with-output-to-string (stream) ;TODO call better.
     (run "pinot-search"
	  (list (typecase search-engine
		  (symbol (string-downcase (symbol-name search-engine)))
		  (string search-engine))
		search-db search-string)
	  :output stream))))

(defun parse-pinot-stream (pinot-stream &key close result)
  "Parses a stream which should be formatted like `pinot-search-stream`\
 result does."
  (flet ((rl ()  ;NOTE/TODO this thing is long...
	   (read-line pinot-stream nil :eof))
	 (dud-ch (ch)
	   (case ch ((#\Space #\Tab #\' #\") t)))
	 (add-result (of &rest add)
	   (if-let (a (assoc of result))
	     (setf (cdr a) (append (cdr a) add))
	     (push (cons of add) result))
	   (values)))
    (do ((line (rl) (rl))) ((eql line :eof) (values))
      (cond
	((start-str= line "Ran query \"")
	 (add-result :ran-query ;TODO need a better way to parse shit...
	   (subseq-upto #\" (subseq-upfrom #\" line))
	   (parse-integer 
	    (subseq-upfrom #\Space (subseq-upto #\Space line :from-end t) 
			   :from-end t))
	   (intern (subseq-upfrom #\Space line :from-end t) :keyword)))
	((start-str= line "Showing ")
	 (add-result :showing-of
	   (parse-integer (subseq-upto #\Space (subseq-upfrom #\Space line)))
	   (parse-integer (subseq-upfrom #\Space line :from-end t))))
	((not(find-if-not #'digit-char-p (subseq-upto #\Space line)))
	 (let*((i        (parse-integer (subseq-upto #\Space line)))
	       (el-str   (subseq-upto #\Space (subseq-upfrom #\Space line)))
	       (element  (intern (string-upcase el-str) :keyword))
	       (rest     (subseq (subseq-upfrom #\: line) 1)))
	   (add-result i element
	     (case element
	       (:location
		(subseq-upfrom #'dud-ch
		   (subseq-upto #'dud-ch rest :from-end t)))
	       (:date
		(list rest nil)) ;TODO read it to make it a unix time.
	       ((:size :score)
		(multiple-value-bind (val len) 
		    (parse-integer rest :junk-allowed t)
		  (list val (subseq rest len))))
	       (t
		rest)))))
	(t
	 (add-result :not-identified line)))))
  (when close (close pinot-stream))
  (reverse result))

(defun pinot-search (search-string &optional (search-engine *search-engine*) 
		     (search-db *search-db*))
  "Searches and parses, returning-the result."
  (parse-pinot-stream
   (pinot-stream search-string search-engine search-db) :close t))
