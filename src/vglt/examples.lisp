;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VGLT -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vglt)

;;; These started out as convenience wrappers for commonly used
;;; statistical plots.  After a while it became apparent this the
;;; Vega-Lite functions should be in their own layer, and a grammar of
;;; graphics layered on top of the various backends, Vega, Plotly,
;;; etc.

;;; In this new architecture, Vega-Lite won't rely on data-frame or
;;; try to infer variable types.  The gg package functions will accept
;;; data frames and do this work.


;; TODO account for legend in :color encoding field

(defun scatter-plot (data encoding &key (title nil) (description nil) (height nil) (width nil))
  "Return a scatter plot of X and Y"
  (assert (or (typep data 'df:data-frame)
	      (typep data 'plist)) () "Error: DATA must be either a PLIST or DATA-FRAME")
  (assert (plistp encoding) () "Error: ENCODING must be a PLIST")
  `(,@(title-description title description)
    ,@(height-width height width)
    :mark :point
    :data ,data
    :encoding ,(vglt:aesthetics encoding)))

(defun line-plot (data encoding &key (title nil) (description nil) (height nil) (width nil))
  "Line plot"
  `(,@(title-description title description)
    ,@(height-width height width)
    :mark :line
    :data ,data
    :encoding ,(vglt:aesthetics encoding)))

(defun bar-chart (data encoding &key (title nil) (description nil) (height nil) (width nil))
  "Return a Vega-Lite JSON specification for a bar chart"
  `(,@(title-description title description)
    ,@(height-width height width)
    :mark :bar
    :data ,data
    :encoding ,(vglt:aesthetics encoding)))


    ;; (setf spec (acons "encoding" `(("x" ("field" . ,x) ("type" . "nominal") ("axis" ("labelAngle" . 0)))
    ;; 				   ("y" ("field" . ,y) ("type" . "quantitative")))
    ;; 		      spec))

#+nil
(defun multi-line-plot (data &key (title nil) (description nil) (legend nil))
  "Multi-series colored line chart
Vega expects data in 'long', repeating format.  See function-plot-data and plot-univariate-functions for details."
;;  (let+ ((#(x y z) data))
    `(,@(title-description title description)
      :mark :line
      :data ,data
      :encoding (:x (:field x :type "quantitative")
		 :y (:field y :type "quantitative")
		 :color (:field f :type "nominal" ,@(if legend `(:legend (:title ,legend))))))
;;    )

  )
