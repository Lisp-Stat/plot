;;; -*- Mode: Lisp; Package: PLOT/TEXT; Syntax: ANSI-Common-Lisp; -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:plot/text)

(defun stem-and-leaf (x &key (stem-size 10) (leaf-size 1))
  "Plot X in stem & leaf format.
       X         - a vector of numbers or symbols which to plot
       STEM-SIZE - the optional number that each character of the stem represents
                   (default is 10)
       LEAF-SIZE - the optional number of characters in each leaf
                   (default is 1). Currently not implemented"
  (declare (ignore leaf-size))		;we'll use it later
  (check-type x vector)
  (let* ((all-stems  (efloor (e/ x stem-size)))
	 (all-leaves (emod x stem-size))
	 (stem-fmt (format nil "~~~AD |" (length (format nil "~A" (sequence-maximum all-stems))))))
    (loop for s from (sequence-minimum all-stems) to (sequence-maximum all-stems)
	  for stems  = (which all-stems :predicate #'(lambda (x) (= x s)))
	  for leaves = (if (= (length stems) 0)
			 nil
			 (coerce (sort (select all-leaves stems) #'<) 'list))
	  do (progn (format t stem-fmt s)
		    (format t "~{ ~D~}~%" leaves)))))
