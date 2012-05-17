;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-ps-command
  (:use :common-lisp :alexandria :j-commandline :j-string-utils)
  (:export ps-command ps do-ps ps-line-handler +ps-allowed+
	   +ps-produce-list+)
  (:documentation "Pretty thin layer around the ps command.
 (C-lib FFI would be better)

Warning see `+ps-allowed+`"))

(in-package :cl-ps-command)

(defconstant +ps-allowed+
  '(:%cpu :%mem :args :blocked :bsdstart :bsdtime :c :caught :class 
    :cls :cmd :comm  :command :cp :cputime :egid :egroup :eip :esp 
    :etime :euid :euser 
    :f :fgid :fgroup :flag :flags :fname :fuid :fuser 
    :gid :group :ignored
    :label :lstart :lwp :ni :nice :nlwp :nwchan :pcpu :pending 
    :pgid :pgrp :pid :pmem :policy :ppid :psr
    :rgid :rgroup :rss :rssize :rsz :rtprio :ruid :ruser
    :s :sched :sess :sgi-p :sgid :sgroup :sid 
    :sig :sigcatch :sigignore :sigmask 
    :size :spid :stackp :start :start-time :stat :state
    :suid :suser :svgid :sz 
    :thcount :tid :sz
    :time :tname :tpgid :tt :tty :ucmd :ucomm :uid :uname :user :vsize
    :vsz :wchan)
  "Boring list of stuff you can ask for(atm)

WARNING: `:args` behaves a little nasty, it tokenizes it, but that is not\
 really desired.")

(defconstant +ps-produce-list+ 
  '(:cmd :command :args :lstart :label)
  "Stuff that produces lists because of how it is tokenized. 
It would be more hassle than it is worth to get round this. These can only go\
 at the end in `do-ps`
NOTE `(= (length +ps-allowed) 95)`, not sure if this is exhaustive!")

(defun key-str (sym)
  "Turn key into string.(only for here)"
  (assert (find sym +ps-allowed+) nil
	  "Cannot return ~a, see `+ps-allowed+` for elements that do exist.")
  (string-downcase (symbol-name sym)))

(defun ps-line-handler (line want)
  "Converts the line string into tokens and numbers.
TODO only tokenizes atm. The way to do it: pass a list of indices where the\
 different entries start! It is given as first line!"
  (declare (ignore want))
  (tokenize-str line))

(defun ps-command (want)
  (format nil "ps -o ~{~a~^,~}" (mapcar #'key-str want)))

(defun ps (want &key (hook #'list)
	   (prepare :full) (assert-in-list t))
  "Calls the `ps` command, runs over the lines collecting `hook` output.
`prepare` indicates whether to prepare throughput; :tok or :tokenize\
 for tokenizing and :full for tokenizing and converting strings into\
 numbers etc, otherwise, get 'raw' strings."
  (when assert-in-list
    (dolist (w want) 
      (assert (keywordp w) nil "~s not a keyword." w)
      (assert (find w +ps-allowed+) nil
	      "~s is not a thing that can be querried." w)))
  (with-input-from-string (stream (command-str (ps-command want)))
    (read-line stream nil)
    (line-by-line stream
       (case prepare
	 (:full (lambda (line) (apply hook (ps-line-handler line want))))
	 ((:tok :tokenize)
	  (lambda (line) (apply hook (tokenize-str line))))
	 (:line hook)
	 (t     (error "~a is not a way to prepare a command-`ps` output\
 line." prepare))))))

(defun to-local (symbol)
  (intern (symbol-name symbol) *package*))
(defun to-local-delist (val)
  (if (listp val) (car val) (to-local val)))

(defun to-key-delist (val)
  (if (listp val) (cadr val) (intern (symbol-name val) :keyword)))

(defmacro do-ps (want &body body)
  "Macro for `ps`, for avoiding having to write what variables you\
 want. This is instead deduced by looking at the variable names."
  (assert (not(find-if (rcurry #'find +ps-produce-list+) (butlast want))) nil
	  "The keywords in `+ps-produce-list+` may produce lists, and may only
 be at the end of the `want` argument.")
  `(ps ',(mapcar #'to-key-delist want)
       :hook (lambda (,@(mapcar #'to-local-delist (butlast want))
		      ,@(let*((v (to-local-delist (car (last want)))))
			  (if (find (to-key-delist (car(last want)))
				    +ps-produce-list+)
			    `(&rest ,v)
			    (list v))))
	       ,@body)
       :prepare ,(case (car body) 
		   ((:full :tok :tokenize :line) (car body))
		   (t :full))))
