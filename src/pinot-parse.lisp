;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-pinot-command
  (:use :common-lisp :alexandria :external-program
	:destructuring-regex
	:j-string-utils :j-seq-utils)
  (:export pinot-stream parse-pinot-stream pinot-search
	   *search-engine* *search-db*)
  (:documentation "Functions to parse the pinot standard output.
 (dont expect it to be a good representation of pinot)

Pinot is a search engine. https://code.google.com/p/pinot-search/"))

(in-package :cl-pinot-command)

(defun from-homedir (str)
  (concat (namestring (user-homedir-pathname)) str))

(defvar *search-engine* :xapian
  "Default search engine, keywordized, initially :xapian.")
(defvar *search-db* (from-homedir ".pinot/main") 
  "Default db location, initially ~/.pinot/main")

(defun search-db (dot-pinot-db)
  (if dot-pinot-db 
    (from-homedir (concat ".pinot/" dot-pinot-db))
    *search-db*))

(defmacro with-bidirectional-stream (stream &body body)
  (with-gensyms (out in)
    `(with-output-to-string (,out) ;Sucks, i want to make it directly..
       (with-input-from-string (,in "")
	 (let ((,stream (make-two-way-stream ,in ,out)))
	   ,@body)))))

(defun scanned-p (file &key (search-engine *search-engine*)
		  dot-pinot-db (search-db (search-db dot-pinot-db)))
  (regex-case (with-output-to-string (out)
		(run "pinot-index" (list "-b" search-engine
					 "-c" "--db" search-db file)
		     :output out))
    ((".+: document ID " :int) t)
    (t                         nil)))

(defun pinot-index (file &key (search-engine *search-engine*)
		    raw ;Raw can be a real pain in the butt.
		    (update t)
		    dot-pinot-db (search-db (search-db dot-pinot-db)))
  "Indexes. 
`update` only indexes not-yet indexed files. (I dont see how to get the\
 index-timestamp so it can be compared with the file-timestamp.)
TODO niceness and _how the fuck_ do you just upgrade???"
  (labels ((ind (file &optional (with "-i"))
	     "Run pinot to index a given file."
	     (with-output-to-string (out)
	       (run "pinot-index" (list with "--db" search-db file)
		    :output out)))
	   (scanned-p* (file)
	     "Wrapper entering values to `scanned-p`"
	     (scanned-p file :search-engine search-engine
			:search-db search-db))
	   (handle (file)
	     "Handle a file, if it is a directory"
	     (if (string= (file-namestring file) "")
	       (mapcar #'handle (cl-fad:list-directory file))
	       (unless (scanned-p* file) ;Already scanned
		 (ind file)))))
    (cond (raw    (ind file))
	  (update (handle file)))))

(defun pinot-stream (search-string &key (search-engine *search-engine*) 
		     dot-pinot-db (search-db (search-db dot-pinot-db))
		     (max 10))
  "Makes a raw stream outputting a search.
use `dot-pinot-db` to indicate a database ~/.pinot/yourdb,\
`search-db` indicates arbitrary directories, defaulting to `*search-db*`"
  (declare (ignore max)) ;TODO 'wrong number of parameters'
  (make-string-input-stream
   (with-output-to-string (stream) ;TODO call better.
     (run "pinot-search"
	  (print(list 
		(typecase search-engine
		  (symbol (string-downcase (symbol-name search-engine)))
		  (string search-engine))
		search-db
		search-string))
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
      (regex-case line
	(("Ran query \".+" query "\" +" (nr :int) " +" (unit :keyword) stuff)
	 (add-result :ran-query query nr unit
		     (regex-case stuff
		       ((" +(N|n)o results.? +") :no-results)
		       (""                       :expected)
		       (t                        :other))
		     stuff))
	(("Showing +" (cnt :int) "~[0-9]" (total-cnt :int))
 ;" results of about "
	 (add-result :showing-of cnt total-cnt))
	(((i :int) " +" (what :keyword) " +: " rest)
	 (add-result i what
	     (case what
	       (:location      rest) ;TODO read it to make it a unix time.
	       (:date          (list rest nil))
	       ((:size :score) (multiple-value-bind (val len) 
				   (parse-integer rest :junk-allowed t)
				 (list val (subseq rest len))))
	       (t              rest))))
	("XapianDataBase::.+"
	 (add-result :some-error line))
	(t
	 (add-result :not-identified line)))))
  (when close (close pinot-stream))
  (reverse result))

(defun pinot-search (search-string &key (search-engine *search-engine*) 
		     dot-pinot-db (search-db (search-db dot-pinot-db))
		     (max 10))
  "Searches and parses, returning-the result."
  (parse-pinot-stream
   (pinot-stream search-string :search-engine search-engine 
		 :search-db search-db :max max) :close t))
