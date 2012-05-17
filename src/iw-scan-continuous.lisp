;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :iw-scan-continuous
  (:nicknames :iw-scan-cont) ;Hardly better..
  (:use :common-lisp :alexandria :iw-scan :j-string-utils)
  (:export *iw-look-hook* iw-scan-once iw-scan-stop iw-scan-continuous
	   iw-look-chain iw-look-print iw-store
	   *iw-sighted*  iw-account-for-stored-sightings
	   iw-look-sightings iw-store-sightings
	   iw-look-interests iw-store-interests)
  (:documentation "Continuous scanning with iw-scan. Provides a hook to look at\
 all the wonderful routers seen. The hooks are just functions with arguments\
 result, interface found on.
Nonexhaustive list of hooks.(Use appropos/the autodocs TODO those not actually made yet.)

iw-look-chain:      Convenience to use multiple hooks at the same time.
iw-store-sightings: Store first sightings in files.
iw-look-interests:  Allows you to say what you care about, takes as argument\
 another hook which takes the interface and those things as argument.
iw-store-interests: Uses the previous to store stuff you indicate. 
                    TODO memory versus disc storage, quick recall.

TODO: a library compresses slightly changing data with easy time/other based\
 access, and another library that makes this useful for iw-scan."))

(in-package :iw-scan-continuous)

(defun iw-look-chain (&rest look-hooks)
  "Chains wireless-look-hooks together"
  (lambda (result interface)
    (dolist (hook look-hooks)
      (funcall hook result interface))
    (values)))

(defun iw-look-print ()
  "Just prints the results.(Boring default)"
  (lambda (result interface) 
    (format t  "Interface ~a at ~a~%" interface (get-universal-time))
    (print result)
    (values)))

(defvar *iw-look-hook* (iw-look-print)
  "Hook that looks at the you ogle at the differ.")

(defun iw-scan-once (&key error-insolate (look-hook *iw-look-hook*))
  "Scan just once, extending the data."
  (labels ((handle-interface (interface)
	     (when (listp interface) ;TODO what else is it?
	       (mapcar (rcurry look-hook (cadar interface)) (cdr interface)))
	     (values))
	   (run ()
	     (mapcar #'handle-interface (iw-scan))
	     (values)))
    (if error-insolate (ignore-errors (run)) (run))))

(let (stop)
  (defun iw-scan-stop () 
    "Stops current continuous scan."
    (setq stop t))
  (defun iw-scan-continuous (&key error-insolate (look-hook *iw-look-hook*)
			     (between (constantly nil)))
    "Scans continously. `between` can be used for instance to wait a while.
Turn off error-insolate if something may be wrong to look at."
    (setq stop nil)
    (do () (stop (values))
      (iw-scan-once :error-insolate error-insolate :look-hook look-hook)
      (funcall between))
    (setq stop nil)))

;;Below the actual code 

;;TODO these guys are more general and could be applied to other things..
;;  .. separate library.

(defun iw-store (&key (path (from-homedir ".iw-scan/sightings/"))
		 (file #'concat) override-file)
  "Always store complete thing.(`iw-store-sighting stores only on sighting.)"
  (ensure-directories-exist path)
  (lambda (results interface)
    (let*((cell-str (concat-list (cdr (assoc :cell-code results))))
	  (file     (funcall file path cell-str)))
      (when (or override-file (not (probe-file file)))
	(with-open-file (stream file :direction :output
			 :if-exists (if override-file :supersede :error)
			 :if-does-not-exist :create)
	  (princ interface stream) (write-char #\Newline stream)
	  (mapcar (lambda (el) (format stream "~{~a~^~T~}~%" el)) results))))
    (values)))
;TODO read that store.

(defparameter *iw-sighted* nil "Wireless adresses seen.")

(defun iw-look-sightings (look)
  "Only run the hook given on first sighting."
  (lambda (results interface)
    (let ((cell-str (concat-list (cdr (assoc :cell-code results)))))
      (unless (find cell-str *iw-sighted* :test #'string=)
	(funcall look results interface)
	(push cell-str *iw-sighted*)))
    (values)))

(defun iw-account-for-stored-sightings
    (&key (path (from-homedir ".iw-scan/sightings/")))
  "Account for the sightings on disk."
  (dolist (file (fad:list-directory path) (values))
    (pushnew (file-namestring file) *iw-sighted* :test 'string=)))

(defun iw-store-sightings (&key (path (from-homedir ".iw-scan/sightings/"))
			  override-file)
  "Stores complete if first sighting. 
Trivial combination of iw-store and iw-look-sighting."
  (iw-look-sightings (iw-store :path path :override-file override-file)))

(defun iw-look-interests (interests then-what)
  "Finds the keywords in `interests` in the results, and feeds them into\
 `then-what` (Interface first argument, interests in-order the rest)
Probably you want at least :cell-code"
  (lambda (results interface)
    (apply then-what
	   (cons interface (mapcar (lambda (i) (cdr(assoc i results)))
				   interests)))
    (values)))

(defun iw-store-interest-write-immediately (file interface &rest values)
  "Default store interest writer writes it immediately."
  (with-open-file (stream file :direction :output
		    :if-exists :append :if-does-not-exist :create)
    (format stream "~a~T~a~{~T~a~}~%" (get-universal-time)
	    interface values)))

(defun iw-store-interests
    (interests &key (name "") 
     (path (from-homedir (concat ".iw-scan/interest/" name "/")))
     (format-in-each-file t)
     (writer #'iw-store-interest-write-immediately)
     (time-rel 0))
  "Continuously store elements you indicate you're interested in.
If path default, filling `name` recommended so you don't overwrite!"
  (ensure-directories-exist path)
  (let ((interests (cons :cell-code interests)))
    (labels ((file-here (str)
	       (concat path str))
	     (then-what (interface cell-code &rest values)
	       (unless (null (cdr cell-code))
		 (warn "Cell code not single string!"))
	       (let ((file (file-here (concat-list cell-code))))
		 (unless (probe-file file) ;Start each file with format.
		   (when format-in-each-file
		     (with-open-file (stream file :direction :output
					     :if-does-not-exist :create)
		       (format stream "#~{ ~a~}~%#Starting time ~a~%" 
			       interests (- (get-universal-time) time-rel)))))
		 (apply writer (cons file (cons interface values))))))
      (unless (probe-file (file-here "format"))
	(with-open-file (stream (file-here "format") :direction :output
				:if-does-not-exist :create)
	  (format stream "Format is~{ ~a~}~%Starting time ~a~%" 
		  interests (- (get-universal-time) time-rel))))
      (iw-look-interests interests #'then-what))))
