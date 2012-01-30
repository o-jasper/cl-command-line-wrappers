;;
;;  Copyright (C) 30-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-ps-command
  (:use :common-lisp :j-commandline :j-string-utils)
  (:export ps-command ps do-ps ps-line-handler +ps-allowed+)
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

(defun key-str (sym)
  "Turn key into string.(only for here)"
  (assert (find sym +ps-allowed+) nil
	  "Cannot return ~a, see `+ps-allowed+` for elements that do exist.")
  (string-downcase (symbol-name sym)))

(defun ps-line-handler (line want)
  "Converts the line string into tokens and numbers.
TODO only tokenizes atm. Need to go through a lot of stuff.."
  (declare (ignore want))
  (tokenize-str line))

;TODO there is a fuckton of ways to select sublists..
(defun ps-command (want)
  (concat "ps -o"
	  (reduce (lambda (a b)
		    (concat a "," (key-str b)))
		  (cdr want) :initial-value (key-str (car want)))))

(defun ps (want &key (hook #'list)
	   (prepare :full) (assert-in-list t))
  "Calls the `ps` command, runs over the lines collecting `hook` output.
`prepare` indicates whether to prepare throughput; :tok or :tokenize\
 for tokenizing and :full for tokenizing and converting strings into\
 numbers etc, otherwise, get 'raw' strings."
  (when assert-in-list
    (dolist (w want) (assert (find w +ps-allowed+))))
  (with-input-from-string (stream (command-str (ps-command want)))
    (read-line stream)
    (line-by-line stream
       (case prepare
	 (:full (lambda (line) (apply hook (ps-line-handler line want))))
	 ((:tok :tokenize)
	  (lambda (line) (apply hook (tokenize-str line))))
	 (:line hook)
	 (t     (error "~a is not a way to prepare a command-`ps` output\
 line." prepare))))))

(defmacro do-ps (want &body body)
  "Macro for `ps`, for avoiding having to write what variables you\
 want. This is instead deduced by looking at the variable names."
  `(ps ',(mapcar (lambda (v)
		   (if (listp v)
		       (cadr v) (intern (symbol-name v) :keyword)))
		 want)
       :hook (lambda ,(mapcar (lambda (v) (if (listp v) (car v) v)) want)
	       ,@body)
       :prepare ,(case (car body) 
		   ((:full :tok :tokenize :line) (car body))
		   (t :full))))
  