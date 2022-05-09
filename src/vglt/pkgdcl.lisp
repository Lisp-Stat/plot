;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:vglt
    (:use #:cl
	  #:dfio
	  #:plot
	  #:select
	  #:let-plus
	  #:alexandria
	  #:alexandria+)
  (:import-from #:dfio #:df-to-alist)
  (:nicknames #:vl)
  (:export
   #:spec
   #:add

   #:plot
   #:defplot
   #:aesthetics
   #:save-plot
   #:show-plots
   #:write-html
   #:write-spec
   #:plot-to-device

   ;; Convenience wrappers for commonly used plots
   #:bar-chart
   #:pie-chart
   #:scatter-plot
   #:line-plot
   #:histogram
   #:box-plot

   ;; Vega-Lite example data sets
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


