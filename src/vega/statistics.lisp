;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2023 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; Wrappers for some statistical plots

(defun qq-plot (sample dist)
  (let* ((eqp (empirical-quantile-probabilities (length sample)))
	 (q (map 'vector (lambda (x)
			    (quantile dist x))
		 eqp)))
    `(:mark :point
      :data ,(plist-df `(:x ,q
			 :y ,(ensure-sorted-vector sample)))
      :encoding (:x (:field :x :type :quantitative)
		 :y (:field :y :type :quantitative :scale (:domain #(,(emin sample)
								     ,(emax sample))))))))
