;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:sctplt
    (:use #:cl #:plot #:alexandria #:vega #:let-plus
	  #:num-utils.arithmetic
	  #:num-utils.elementwise
	  #:num-utils.matrix-shorthand)
  (:import-from #:select #:select)
  (:import-from #:data-frame #:make-df)
  (:import-from #:statistics #:linear-regression)
  (:import-from #:smoothers #:lowess)
  (:documentation "Scatterplot functions")
  (:export #:fit-line
	   #:fit-lowess))
(in-package #:sctplt)


(defun fit-line (x y &key (color :red))
  "Return Vega-lite encoding to fit a line to scatterplot data."
  (let+ ((points (loop
                   for i across x
                   for j across y
                   collect (list i j)))
         ((&values a m) (linear-regression points))
	 (df (df:make-df '(x y) `(,x ,(map 'vector (lambda (x)
						     (+ (* m x) a))
					   x)))))
    `(:mark (:type :line :color ,color :interpolate :basis)
      :data (:values ,(df-to-vl-plist df))
      :encoding (:x (:field :x :type "quantitative")
		 :y (:field :y :type "quantitative")))))



(defun fit-lowess (x y &key
			 (color :red)
			 (span 2/3))
  "Return the Vega-lite encoding to fit a lowess curve to scatterplot data."
  (let ((df (df:make-df '(x y) `(,x ,(lowess x y :f span)))))
     `(:mark (:type :line :color ,color :interpolate :basis)
       :data (:values ,(df-to-vl-plist df))
       :encoding (:x (:field :x :type "quantitative")
		  :y (:field :y :type "quantitative")))))
