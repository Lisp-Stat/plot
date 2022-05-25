;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VGLT -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vglt)

;;; JSON/Vega encoding

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

(defun encode-symbol-list (keys)
  "Encode a list of symbols into a JSON vector.
Example: (encode-symbol-list '('mpg 'carb 'am)) => #('mpg' 'carb' 'am')"
  (map 'vector #'yason:encode-symbol-as-lowercase keys))

(defun flatten-data (data)
  "Return a plist specification for transforming data into Vega-Lite format"
  `#((:flatten ,(encode-symbol-list (plist-keys data)))))

(defmethod yason:encode ((p vglt-plot) &optional (stream *standard-output*))
  (let ((spec (plot-spec p))
	(yason:*list-encoder*       'yason:encode-plist)
	(yason:*symbol-encoder*     'yason:encode-symbol-as-lowercase)
	(yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase))

    (setf (getf spec :data) `(:url "hp-mpg-data.json"))
;;    (setf (getf spec :data) `(:values ,(plot-data p)))

    ;	  (encode-inline-data (plot-data p)) ;(:values #((:mpg #(18 ...
    (setf (getf spec :transform) (flatten-data (plot-data p)))

    (yason:with-output (stream :indent t)
      (yason:encode spec

		    ))))

