;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

#|
=========================================================
Module: load-eliza.lisp: 
Description: a load for three eliza modules.
bugs to vladimir kulyukin in canvas
=========================================================
|#

;;; change this parameter as needed.
(defparameter *eliza-path* 
  "/home/chandler/Documents/cs5600/CS5600-finalproj/")

;;; the files that comprise ELIZA.
(defparameter *eliza-files* 
  '("auxfuns.lisp" "eliza.lisp")) 

(defun load-eliza-aux (path files)
  (mapc #'(lambda (file)
	    (load (concatenate 'string path file)))
	files))

;;; load ELIZA
(defun load-eliza ()
  (load-eliza-aux *eliza-path* *eliza-files*))

	
