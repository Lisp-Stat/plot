;;; -*- Mode: Lisp; Package: PLOT/TEXT; Syntax: ANSI-Common-Lisp; -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:plot/text)

(defun leaf-strings (all-stems all-leaves sort-order)
  "Returns a hashtable where the key represents the stem, and the value
   represents the leaves as a string."
  (let ((max-stem (sequence-maximum all-stems))
	(strings (make-hash-table :size 1000)))
    (loop for s from (sequence-minimum all-stems) to max-stem
	  for stems = (which all-stems :predicate #'(lambda (x) (= x s)))
	  for leaves = (if (= (length stems) 0)
			   nil
			   (coerce (sort (select all-leaves stems) sort-order) 'list))
	  do (setf (gethash s strings) (format nil "~{ ~A~}" leaves)))
    strings))

(defun stem-and-leaf (x &key (stem-size 10) (leaf-size 1) split-stems)
  "Plot X in stem & leaf format.
       X           - a vector of numbers or symbols which to plot
       STEM-SIZE   - the optional number that each character of the stem represents
                     (default is 10)
       LEAF-SIZE   - the optional number of characters in each leaf
                     (default is 1). Currently not implemented
       SPLIT-STEMS - if T the stems will be split. Currently not implemented."
  (declare (ignore leaf-size))		; we'll use it later

  (check-type x vector)
  (let* ((all-stems  (efloor (e/ x (if split-stems (/ stem-size 2) stem-size))))
	 (all-leaves (emod x stem-size))
	 (stem-fmt (format nil "~~~AD |" (length (format nil "~A" (sequence-maximum all-stems)))))
	 (leaf-strings-vector (leaf-strings all-stems all-leaves #'<)))
    (loop for s from (sequence-minimum all-stems) to (sequence-maximum all-stems)
	  do (progn (format t stem-fmt (if split-stems (floor (/ s 2)) s))
		    (format t "~A~%" (gethash s leaf-strings-vector))))))

(defun back-to-back-stem-and-leaf (x y &key (stem-size 10) (leaf-size 1) split-stems)
  "Plot X and Y in stem & leaf format.
       X           - a vector of numbers or symbols which to plot on the left
       Y           - a vector of numbers or symbols which to plot on the right
       STEM-SIZE   - the optional number that each character of the stem represents
                     (default is 10)
       LEAF-SIZE   - the optional number of character in each leaf.
                     (defaut is 1). Currently not implemented
       SPLIT-STEMS - if T the stems will be split. Currently not implemented."
  (declare (ignore leaf-size))   ; Not implemented. See STEM-AND-LEAF.
  (declare (ignore split-stems)) ; Not implemented. See github issue #2
  (check-type x vector)
  (check-type y vector)
  (let* ((left-stems  (efloor (e/ x stem-size)))
	 (right-stems (efloor (e/ y stem-size)))
	 (combined-stems (concatenate 'vector left-stems right-stems))
	 (left-leaves  (emod x stem-size))
	 (right-leaves (emod y stem-size))
	 (stem-fmt (format nil " | ~~~AD |" (length (format nil "~A" (sequence-maximum combined-stems)))))
	 (left-strings  (leaf-strings left-stems left-leaves #'>))
	 (right-strings (leaf-strings right-stems right-leaves #'<))
	 (left-min-col (sequence-maximum (loop for leaf being each hash-value of left-strings collect (length leaf)))))
    (loop for s from (sequence-minimum combined-stems) to (sequence-maximum combined-stems)
	  do (progn (format t "~v@A" left-min-col (gethash s left-strings ""))
		    (format t stem-fmt s)
		    (format t "~A~%" (gethash s right-strings ""))))))
