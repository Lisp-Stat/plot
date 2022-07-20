;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; Wrappers for common statistical plots

(defun scatter-plot-matrix (data columns &key (title nil) (description nil) (height nil) (width nil))
  "Return a scatter plot matrix of ROWS and COLUMNS"
  (assert (or (typep data 'df:data-frame)
	      (typep data 'plist)) () "Error: DATA must be either a PLIST or DATA-FRAME")

    (when (typep data 'plist)
      (setf data (plist-df data)))

  `("$schema" "https://vega.github.io/schema/vega-lite/v5.json"
    :title ...
    :repeat (:row     #(,columns)
	     :columns #(,columns))	;reverse?
    :description ...
    :height ...
    :width ...
    :mark :point
    :data ,data
))

#| From the plotting examples
(defparameter vgcars-splom
 (vega::make-plot "vgcars-splom"
		  vgcars
		  `("$schema" "https://vega.github.io/schema/vega-lite/v5.json"
			:title "Scatterplot Matrix for Vega Cars"
			:repeat (:row    #(:horsepower :acceleration :miles-per-gallon)
			         :column #(:miles-per-gallon :acceleration :horsepower))
			:spec (:data (:url "/data/vgcars-splom-data.json")
			:mark :point
			:params #((:name "brush"
			:select (:type "interval"
			         :resolve "union"
					 :on "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!"
					 :translate "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!"
					 :zoom "wheel![event.shiftKey]"))
				    (:name "grid"
					 :select (:type "interval"
					 :resolve "global"
					 :translate "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!"
					 :zoom "wheel![!event.shiftKey]")
					 :bind :scales))
	        :encoding (:x (:field (:repeat "column") :type "quantitative")
			           :y (:field (:repeat "row") :type "quantitative" :axis ("minExtent" 30))
					   :color (:condition (:param "brush" :field :origin :type "nominal")
					           :value "grey"))))))
(plot:plot vgcars-splom)
|#
