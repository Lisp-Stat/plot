;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; Plotting examples

(defparameter vega-desktop '(:spec-loc #P"~/Desktop/plots/"
			     :data-loc #P"~/Desktop/plots/")
  "Data and specification go in the same directory")

;; Vega cars -- data frame
(load #P"LS:DATASETS;vlcars")
(vega:defplot hp-mpg `(,@(vega:scatter-plot vlcars '(:x vlcars:horsepower :y vlcars:miles-per-gallon) :title "my title")))
(plot:plot-from-file (vega:write-html hp-mpg))
(plot-to-device hp-mpg vega-desktop)

;; Vega cars - plist
(vega:defplot hp-mpg-2
  `(:mark :point
    :title "Vega Cars"
    :description "Variables describing the characterics of various cars"
    :data (:horsepower ,vlcars:horsepower
	   :mpg ,vlcars:miles-per-gallon)
    :encoding (:x (:field horsepower :type quantitative :title "Horsepower")
	       :y (:field mpg :type quantitative :title "Miles per gallon"))))

(plot:plot-from-file (vega:write-html hp-mpg-2))
(plot-to-device hp-mpg-2 vega-desktop)







