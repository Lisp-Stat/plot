;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:vega
  (:use #:lisp-stat #:let-plus #:plot)
  (:import-from #:parenscript #:symbol-to-js-string)
  (:import-from #:dfio #:with-input-stream #:data-column #:data-column-add #:data-column-vector)
  (:local-nicknames (:lt :local-time))
  (:documentation "A plotting backend for Plot based on Vega.  The specification DSL, in PLIST format, is that of Vega-Lite.  The data output representation is Vega.")

  (:export
   #:spec
   #:add

   #:plot
   #:defplot
   #:aesthetics
   #:save-plot
   #:make-plot
   #:show-plots
   #:write-html
   #:write-spec
   #:write-vega-data			;write data to a file
   #:show-plots				;move to PLOT?
   #:*all-plots*			;global list of plots
   #:plot-to-device

   ;; Data-frame I/O
   #:read-vega
   #:df-to-vl-plist

   ;; Convenience wrappers for commonly used plots
   #:bar-chart
   #:pie-chart
   #:scatter-plot
   #:line-plot
   #:histogram
   #:box-plot

   ;; Vega-Lite example data sets
   #:load-vega-examples

   #:annual-precip
   #:anscombe
   #:barley
   #:budget
   #:budgets
   #:burtin
   #:cars
   #:countries
   #:crimea
   #:driving
   #:earthquakes
   #:flare-dependencies
   #:flare
   #:flights-10k
   #:flights-200k
   #:flights-20k
   #:flights-2k
   #:flights-5k
   #:football
   #:gapminder
   #:income
   #:jobs
   #:londonBoroughs
   #:londonCentroids
   #:londonTubeLines
   #:miserables
   #:monarchs
   #:movies
   #:normal-2d
   #:obesity
   #:ohlc
   #:penguins
   #:points
   #:political-contributions
   #:population
   #:udistrict
   #:unemployment-across-industries
   #:uniform-2d
   #:us-10m
   #:us-state-capitals
   #:volcano
   #:weather
   #:wheat
   #:world-110m
   #:airports
   #:birdstrikes
   #:co2-concentration
   #:disasters
   #:flights-3m
   #:flights-airport
   #:gapminder-health-income
   #:github
   #:iowa-electricity
   #:la-riots
   #:lookup_groups
   #:lookup_people
   #:population_engineers_hurricanes
   #:seattle-weather-hourly-normals
   #:seattle-weather
   #:sp500-2000
   #:sp500
   #:stocks
   #:us-employment
   #:weather
   #:windvectors
   #:zipcodes
   #:unemployment
   #:flights-200k))


