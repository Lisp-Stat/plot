;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VGLT -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vglt)

;;; JSON/Vega data manipulation

(defun sequence-to-alist (seq &optional (x "x"))
  "Map X to each value in SEQ as an ALIST

By default, Vega-Lite will accept a JSON array as data and map
it. The spec then refers to it as 'data', instead of X, Y, etc. If
for some reason you don't want to use that, this function will do
the same mapping using a key of your choice.

Input:
seq = #(1 2 3 4)
x = 'X'

Output: ((X 1) (X 2) (X 3) (X 4))

Remember that Vega-Lite is case sensitive"
  (map 'vector
       #'(lambda (elmt)
	   (list (cons x elmt)))
       seq))

