;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; Plotting utilities

;;; These are (mostly) generic so you may override their behaviour for
;;; use in more advanced plots than those provided by default
#|
(defgeneric variable-title (v default)
  ;; Is there a way to use a global default for x&y titles? E.g. vega:*default-x-title*, vega:*default-y-title*
  (:documentation
   "Give variable V a title by progressively searching meta data sources.

First we look for a DATA-FRAME column type designator and use that if present
If not, take the symbol name
Finally, in the case of a literal sequence, use DEFAULT")
  (:method ((v symbol) default)
    `(:title ,(format nil "~A" (if (get v :label)
				   (get v :label)
				   (symbol-name v)))))
  (:method ((v sequence) default)
    (format nil ":title ~A" default)))

;; According one of the Vega-Lite developers, Ordinal and nominal are
;; really just categorical data. Sorted categorical data is ordinal.
;; See: https://github.com/vega/vega-lite/issues/7654 and
;;      https://github.com/vega/vega-lite/issues/6633
;; When he says 'sorted', does he mean 'ordered' -- seems so
;; Integer vector then could be mapped to 'nominal'
(defun variable-type (v &optional (default "quantitative"))
  "Convert Lisp-Stat variable types to Vega-Lite types"
  (let ((type (get v :type)))
    `(:type ,(format nil "~A" (case type
				(string       "ordinal")
				(double-float "quantitative")
				(single-float "quantitative")
				(categorical  "ordinal")
				(temporal     "temporal")
				;;	    (integer "quantitative")	;this depends on proper variable meta-data. A integer in a column could be a FACTOR or quantitative variable
				(bit "ordinal")
				(null default)
				(t default))
		     ))))
|#

(defun title-description (title description)
  "Set title and description of plot"
  (let (td)
    (if title       (setf (getf td :title)       title))
    (if description (setf (getf td :description) description))
    td))

(defun height-width (h w)
  "Set height & width of plot"
  (let (hw)
    (if h (setf (getf hw :height) h))
    (if w (setf (getf hw :width) w))
    hw))

;; I would have named this 'encode', but don't want to clash with YASON:ENCODE
;; See: https://vega.github.io/vega-lite/docs/encoding.html for the gory complexity that is Vega-Lite encoding
;; TODO: account for legend in :color, e.g. :color (:field f :type "nominal" ,@(if legend `(:legend (:title ,legend))))
;; I think this can go away when we implement yason:encode for data variables, e.g mtcars:horsepower
#+nil
(defun aesthetics (plist)
  "Map variables to aesthetics
Shorthand encodings for Vega-Lite plot specifications. These short-hand versions handle most statistical use cases."
  (assert (typep plist 'list) () "Error ~A is not a plist" plist)
  (loop
    for (key value) on plist by #'cddr
    append (case key
		 ;; (:x `(:x (:field ,(string-downcase (symbol-name value)) ,@(variable-type value) ,@(variable-title value "x"))))
		 (:x `(:x (:field ,value ,@(variable-type value) ,@(variable-title value "x"))))
		 (:y `(:y (:field ,value ,@(variable-type value) ,@(variable-title value "y"))))
		 (:color `(:color (:field ,value))) ; ,@(variable-type value)))) ;See: https://vega.github.io/vega-lite/docs/scale.html#scheme
		 (:shape `(:shape `(:field ,value ,@(variable-type value))))
		  )
      into plot-encoding
    finally (return plot-encoding)
    )
  )

;;    (defmacro defprop (symbol value indicator)

;;;
;;; Helpers for specific plot types
;;;


(defun multi-line-plot-data (d)
  "Transform data, D a plist, into Vega-Lite format for line plotting.

D must have one :x value for the x axis, and one or more vectors of y values.  The symbols for the y values become line labels in the plot.  For example, given D: (:x #(1 2 3) :sin #(0.84147096 0.9092974 0.14112) :cos #(0.5403023 -0.41614684 -0.9899925))

Vega-Lite requires repeating x ranges in a 'long' format, so D needs to be transformed into a plist of 3 arrays:

'(:x #(1          2         3       1         2           3)          ;x values
  :y #(0.84147096 0.9092974 0.14112 0.5403023 -0.41614684 -0.9899925) ;f(x)
  :f #(sin        sin       sin     cos        cos         cos))      ;name of the function

We use a fill pointer, so the y values need not be of equal length."

  (let* ((x-values (getf d :x))		;TODO check that x-values aren't NIL
	 (len (* (length x-values) (1- (length d)))) ;length of x multiplied by the number of lines in the plot (i.e. 'long')
	 (x (make-array len :fill-pointer 0))  ;x values
	 (y (make-array len :fill-pointer 0))  ;y values
	 (f (make-array len :fill-pointer 0))) ;function
    (loop
      for (key value) on d by #'cddr
      unless (eq key :x) do
	(loop for i from 0 to (1- (length value)) do
	  (progn (vector-push (aref x-values i) x)
		 (vector-push (aref value i) y)
		 (vector-push (string-downcase (symbol-name key)) f))))
    `(:x ,x :y ,y :f ,f)))

;;; This is a slightly odd way to return the data. The reason is that, initially, the line plotting functions had a 2D array as input. That's a useful format to have this kind of data in, so we kept it as optional.
(defun plot-univariate-functions (x funs &key (as-array nil))
  "Apply the functions in FUN, a list, to X and return the results in an array suitable to pass to vega:multi-line-plot"
  (let+ (((&flet gen-vega-rows (fun)
	    (lambda (x)
	      `#(,x ,(funcall fun x) ,(string-downcase (symbol-name fun))))))
	 data)

    ;; set data to a 2d array of the values
    (setf data (apply #'aops:stack-rows
		      (loop for f in funs
			    collect (aops:combine (map 'vector (gen-vega-rows f) x)))))
    (if as-array
	data
	`(:x ,(select data t 0) :y ,(select data t 1) :f ,(select data t 2)))))




