;;
;;  Copyright (C) 19-02-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-wmctrl
  (:use :common-lisp :j-commandline :j-string-utils)
  (:export wm-list +wmctrl-allowed+ do-wm-list wm-line-handler
	   wm-to-id wm-to-title command-of-pid)
  (:documentation "Thin layer around wmctrl command.
 (C-lib FFI might be better, but seems to work fine.)"))

(in-package :cl-wmctrl)

(defconstant +wmctrl-allowed+
  '(:id :pid :desktop :x-offset :y-offset :width :height :machine :title
    :command))

(defun command-of-pid (pid &key (remove-last #\Newline))
  "Get the command belonging to a pid.\
 (Actualy, perhaps better off in cl-top)"
  (let ((result (command-str "ps -p " pid " -o comm=")))
    (if (string= result "") ;Deal with all sorts of output of commands!
      result
      (let ((len-1 (- (length result) 1))) ;Bit ugly.
	(if (and remove-last (char= (aref result len-1) remove-last))
	  (subseq result 0 len-1) result)))))

(defun wh (ch)
  (case ch ((#\Space #\Tab) t)
	   (#\Newline (error "Newlines should be gone here."))))

(defun need-pids (want) ;See if want/need pid
  (or (find :pid want) (find :command want)))

(defun need-geom (want)
  (find-if (lambda (el) ;See if want geometry
	     (case el ((:x-offset :x-offset :width :height) t)))
	   want))

(defun wm-line-handler 
    (line want &key (pids (need-pids want)) (geom (need-geom want)))
  "Produces a function that can handle a single line"
  (let ((tok (tokenize-str line "" #'wh
			   (+ 3 (if pids 1 0) (if geom 4 0)))))
    (when (>= (length tok) 2)
      (destructuring-bind (id desktop &rest more) tok
	(destructuring-bind (pid x-offset y-offset width height
			     machine title &rest overflow)
	    (cond ;Arrange to get the variables right.
	      ((and pids geom) more)
	      (pids  	       (cons (car more)
				    (append (make-list 4) (cdr more))))
	      (geom 	       (cons nil more))
	      (t               (append (make-list 5) more)))
	  (assert (null overflow) nil
		  "Overflow ~s~%From window list entry ~s" overflow tok)
	  (flet ((get-wanted (wanted-name)
		   (case wanted-name ;;Hmm would be better if 
		     (:id       (parse-integer id :start 2 :radix 16))
		     (:pid      (parse-integer pid))
		     (:desktop  (parse-integer desktop))
		     (:x-offset (parse-integer x-offset))
		     (:y-offset (parse-integer y-offset))
		     (:width    (parse-integer width))
		     (:height   (parse-integer height))
		     (:machine  machine) 
		     (:title    title)
		     (:command  (command-of-pid (parse-integer pid)))
		     (t         (error "Not allowed to get ~a, see 
`+wmctrl-allowed+` for what is." wanted-name)))))
	    (mapcar #'get-wanted want)))))))

(defun wm-list (want &key (hook #'list) (prepare :full) (utf t))
  "List the windows. Want is one of 
:id :desktop :pid :x-offset :y-offset :width :height :machine :title

Note that with `utf` false, it will fail if there is no utf at the moment.."
  (declare (type function hook) (type list want))
  (let ((pids (need-pids want)) (geom (need-geom want)))
    (line-by-line 
     (command-str "wmctrl -l" (if utf " -u" "")
		  (if pids " -p" "") (if geom " -G" "") "|cat -v")
     (case prepare
       (:full
	(lambda (line)
	  (alexandria:when-let (handled (wm-line-handler line want
					      :pids pids :geom geom))
	    (apply hook handled))))
       ((:tok :tokenize)
	(lambda (line)
	  (apply hook (tokenize-str line))))
       (:line
	hook)))))

(defmacro do-wm-list (want &body body)
  "Macro for `wm-list`, for avoiding having to write what variables you want.\
 This is instead deduced by looking at the variable names."
  ;(assert (case prepare ((:full :tok :tokenize) t)))
  `(wm-list ',(mapcar (lambda (v) 
			(if (listp v)
			    (cadr v) (intern (symbol-name v) :keyword)))
		      want)
	    :hook (lambda ,(mapcar (lambda (v) (if (listp v) (car v) v)) want)
		    ,@body)
	    :prepare ,(case (car body) 
			((:full :tok :tokenize :line) (car body))
			(t :full))))

(defun wm-id-nr (id)
  "Turn integer id to string passable to command."
  (let ((str (let ((*print-base* 16)) (format nil "~a" id))))
    (concat "0x" (make-string (- 8 (length str)) :initial-element #\0) str)))

(defun wm-to-id (id)
  "Go to a window identity number"
  (command-str "wmctrl -i -a " (wm-id-nr id)))

(defun wm-to-title (title)
  "Go to window with that title. Note that it can be unreliable due to\
 identical/similar titles."
  (command-str "wmctrl -a " title))
