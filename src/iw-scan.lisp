;;
;;  Copyright (C) 19-02-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :iw-scan
  (:use :common-lisp :alexandria :j-general :j-seq-utils :j-string-utils
	:j-parse-number
	:j-commandline :read-tab-listing
	:regex :regex-sequence)
  (:export iw-stream iw-scan iw-press iw-press-cell press-address 
	   unpress-address iw-cur)
  (:documentation "Uses `iwlist wlan0 scanning` command to scan wireless\
 networks. The iw-scan-continuous package has facilities for continuous\
 scanning with a hook, and some hooks to start from.

NOTE: `sudo iwlist wlan0 scanning` it will only give a subset of the actual\
 result! For more add to file /etc/sudoers: `<username> /usr/sbin/iwlist\
 wlan0 scanning (TODO security implications?)"))

(in-package :iw-scan)
#| TODO provide the doc strings though..
 (defclass* iw-cell-short ()
  "Short information linked to a single measurment."
  (seen-time "Time at which seen(ms unix)" integer (get-universal-time))
  address channel
  frequency quality signal-level last-beacon)

 (defclass* iw-cell (iw-cell-short)
  "All sorts of info on a source."
  essid
  cell-nr frequency-unit max-quality signal-level-unit last-beacon-unit 
  last-beacon-from
  bit-rates encryption-key mode
  (other "Stuff i could not place." list nil))
|#

(defun cut-bit-rates (string)
  "Turn string of bit rates into parts."
  (declare (type string string))
  (destructuring-regex
      ("[:space:]*" ("[:digit:]*\\\.[:digit:]*|[:digit:]+" bit-rate)
       "[:space:]*" ("(M|G|T)?(b|B)/(s|S)" bit-rate-unit) ";" rest) string
    (when bit-rate
      (assert bit-rate-unit)
      (cons (list (parse-number bit-rate) (intern bit-rate-unit :keyword))
	    (when rest (cut-bit-rates rest))))))

(defun de-quote (string)
  "Takes out outer quotes."
  (subseq-upfrom #\" (subseq-upto #\" string :from-end t)))

(defun press-interface (interface-name)
  (intern (rev-case interface-name) :keyword))

(defun iw-press-cell-info (info &key long)
  "Press the Cell info in a format. TODO rather clumsy.."
  (if (listp info)
    (destructuring-regex (which "[:space:]*(:|=)[:space:]*" rest) (car info)
      (when long
	(if rest
	  (string-case (or which "fail")
	    ("Bit Rates" `((:bit-rates 
			    ,@(cut-bit-rates rest) 
			    ,@(mapcan #'cut-bit-rates (cdr info)))))
	    ("IE"        `((:ie-list rest ,@(cdr info))))
	    (t            (string-case (car info)
			    ("Preauthentication Supported"
			     `((:Preauthentication-Supported ,@(cdr info))))
			    (t (list info)))))
	  (list info))))
    (destructuring-regex (which ":|=" rest) info
      (assert rest nil "Which-scan failed on ~s" info)
      (string-case (or which "fail")
	("Channel"
	 (destructuring-regex
	     ("[:blank:]*" ("[:digit:]+" channel) "[:blank:]*") rest
	   `((:channel ,(parse-integer channel)))))
	("Frequency"
	 (destructuring-regex 
	     (("[:digit:]*\\\.[:digit:]*|[:digit:]+" frequency)
	      "[:space:]*" ("[:alnum:]+" frequency-unit) "[:space:]*") rest
	   (assert (and frequency rest))
	   `((:frequency ,(parse-number frequency) ,frequency-unit))))
	("Quality" ;TODO doesn't seem to get signal level unit.
	 (destructuring-regex 
	     (("[:digit:]+" quality) "/" ("[:digit:]+" max-quality) 
	      "[:space:]*Signal level[:space:]*" "=|:" "[:space:]*"
	      ("-?([:digit:]*\\\.[:digit:]*|[:digit:]+)" signal-level)
	      "[:space:]*" signal-level-unit "[:space:]*") rest
	   (assert (and quality max-quality signal-level signal-level-unit)
		   nil "failed on ~s" rest)
	   `((:quality ,(parse-integer quality))
	     (:signal-level ,(parse-number signal-level)
			    ,signal-level-unit)
	     (:max-quality ,(parse-integer max-quality)))))
	("Extra"
	 (when long
	   (destructuring-regex ("[:space:]*" which ":|=[:space:]*" rest) rest
	     (cond
	       ((string= which "Last beacon")
		(destructuring-regex
		    (("[:digit:]*\\\.[:digit:]*|[:digit:]+" last-beacon) 
		     last-beacon-unit "[:space:]" last-beacon-from) rest
		  (assert (and last-beacon last-beacon-unit))
		  `((:last-beacon ,(parse-number last-beacon)
				  ,(intern last-beacon-unit :keyword)
				  ,(intern last-beacon-from :keyword)))))
	       ((not long) nil)
	       ((string= which "tsf")
		`((:extra-tsf ,rest)))))))
	("ESSID"
	 `((:essid ,(de-quote rest))))
	(t
	 (when long
	   (string-case (or which "fail")
	     ("Bit Rates"
	      `((:bit-rates ,@(cut-bit-rates rest))))
	     ("fail"
	      `((,which ,@rest)))
	     (t
	      (assert which)
	      (list (cons (string-case which
			    ("Encryption key" :encryption) ("Mode" :mode) 
			    ("IE: Unknown" :ie-unknown) ("IE" :ie) (t which))
			  rest))))))))))

(defun press-address (adress)
  "Turns an addres of hexidecimal number separated with : into a list of
 integers."
  (destructuring-regex ((".." val) (":" between) rest) adress
    (when between
      (cons (parse-integer val :radix 16) (press-address rest)))))

(defun to-hex-min-len (i min-len)
  (let*((*print-radix* t) (*print-base* 16)
	(result (subseq (princ-to-string i) 2)))
    (if (< (length result) min-len)
      (concat (make-string (- min-len (length result)) :initial-element #\0)
	      result)
      result)))

(defun unpress-address (address &key (min-len 2)) ;TODO inverse-text
  "Inverse of `press-addres`, turns the numbers into a string with numbers in\
 hex."
  (reduce (lambda (have el)
	    (concat have ":" (to-hex-min-len el min-len)))
	  (cdr address) 
	  :initial-value (to-hex-min-len (car address) min-len)))

(defun iw-press-cell (cell &key (long t))
  (destructuring-regex
      ("Cell[:space:]*" ("[:digit:]+" cell-nr) "[:space:]*-?[:space:]*"
       "Address:[:space:]*" address) (car cell)
    (assert (and cell-nr address) nil
	    "destructuring-regex failed; ~s" (car cell))
    `((:cell-nr ,(parse-integer cell-nr))
      (:address ,@(press-address address))
      (:press-time ,(get-universal-time))
      ,@(mapcan (lambda (ci) (iw-press-cell-info ci :long long))
		(cdr cell)))))

(defun iw-press-top (top)
  (or (when (stringp top)
	(destructuring-regex 
	    (interface ("[:space:]*Interface doesn't support scanning." 
			found)) top
	  (when found (cons :interface-doesnt-support-scanning interface))))
      top))

(defun iw-press (result cell-hook)
  "Coerce what we understand in the scan result in a nicer form."
  (or (when (listp result) 
	(destructuring-regex
	    (interface ("[:space:]*Scan completed[:space:]*:?[:space:]*" 
			found)) (car result)
	  (when found 
	    (let ((interface (press-interface interface)))
	    (cons interface  ;Continue with cell.
		  (mapcar (rcurry cell-hook interface) (cdr result)))))))
      result))

(defun iw-stream (&key (sudo t))
  "Gives the raw output for the iwlist command. Defaultly uses `sudo`, see\
 documentation of package."
  (make-string-input-stream
   (command-str (if sudo "sudo " "") "iwlist wlan0 scanning |cat -v")))

(defun iw-scan 
    (&key (sudo t) (iw-stream (iw-stream :sudo sudo) iw-stream-manual-p)
     (m-to-s-expr t) (press t)
     (cell-hook (lambda (cell interface) (declare (ignore interface)) cell))
     (long t)
     (hook (rcurry #'iw-press 
		   (lambda (cell interface 
			    &key (pass (if press
					 (iw-press-cell cell :long long)
					 cell)))
		     (if cell-hook (funcall cell-hook pass interface) pass))))
     (return t))
  "Scan once, return info.
Works via `read-tab-listing`, note that some parts may be double. Also,\
 defaultly, `m-to-s-expr` is true, because the data makes more sense that\
 way, and it is also pressed into a more easily readable form.

TODO read the stream cell-by-cell?(NOTE: it is not yet an actual stream,\
 really)"
  (let ((result (read-tab-listing iw-stream :m-to-s-expr m-to-s-expr)))
    (prog1 (if return (mapcar hook result) 
	       (progn (mapc hook result) (values)))
      (unless iw-stream-manual-p (close iw-stream)))))

(defun iw-cur ()
  "If exists, gets the current connection.(Nill if none)
 TODO can multiple-exist? Would make sense, put a loop over them.."
  (destructuring-regex (interface "[:space:]+ESSID:[:space:]*" essid)
      (command-str "iwgetid | cat -v")
    (let ((address (command-str "iwgetid -r -a | cat -v")))
      (plist :keyword (press-interface interface) (de-quote essid) 
	              (press-address address)))))
