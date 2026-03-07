;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VEGA-TESTS -*-
;;; CL forms for constructing test plots from cookbook fixtures.
;;; These forms construct plots using only literal inline data (no network).
;;; Each corresponds to a .json fixture file in this directory.
;;; SPDX-License-identifier: MS-PL

;;; simple-bar-chart.json
;;; Source: https://lisp-stat.dev/docs/cookbooks/plotting/#simple-bar-chart
;;; (vega:defplot simple-bar-chart
;;;   `(:mark :bar
;;;     :data (:values ,(plist-df '(:a #(A B C D E F G H I)
;;;                                :b #(28 55 43 91 81 53 19 87 52))))
;;;     :encoding (:x (:field :a :type :nominal :axis ("labelAngle" 0))
;;;                :y (:field :b :type :quantitative))))

;;; grouped-bar-chart.json
;;; Source: https://lisp-stat.dev/docs/cookbooks/plotting/#grouped-bar-chart
;;; (vega:defplot grouped-bar-chart
;;;   `(:mark :bar
;;;     :data (:values ,(plist-df '(:category #(A A A B B B C C C)
;;;                                :group    #(x y z x y z x y z)
;;;                                :value    #(0.1 0.6 0.9 0.7 0.2 1.1 0.6 0.1 0.2))))
;;;     :encoding (:x (:field :category)
;;;                :y (:field :value :type :quantitative)
;;;                :x-offset (:field :group)
;;;                :color    (:field group))))

;;; pie-chart.json
;;; Source: https://lisp-stat.dev/docs/cookbooks/plotting/#pie-chart
;;; (vega:defplot pie-chart
;;;   `(:data (:values ,(plist-df `(:category ,(aops:linspace 1 6 6)
;;;                                :value #(4 6 10 3 7 8))))
;;;     :mark :arc
;;;     :encoding (:theta (:field :value :type :quantitative)
;;;                :color (:field :category :type :nominal))))

;;; donut-chart.json
;;; Source: https://lisp-stat.dev/docs/cookbooks/plotting/#donut-chart
;;; (vega:defplot donut-chart
;;;   `(:data (:values ,(plist-df `(:category ,(aops:linspace 1 6 6)
;;;                                :value #(4 6 10 3 7 8))))
;;;     :mark (:type :arc :inner-radius 50)
;;;     :encoding (:theta (:field :value :type :quantitative)
;;;                :color (:field :category :type :nominal))))

;;; box-plot-summaries.json
;;; Source: https://lisp-stat.dev/docs/cookbooks/plotting/#summaries
;;; (vega:defplot box-plot-summaries
;;;   `(:title "Body Mass of Penguin Species (g)"
;;;     :data (:values ,(plist-df '(:species #("Adelie" "Chinstrap" "Gentoo")
;;;                                :lower #(2850 2700 3950)
;;;                                :q1 #(3350 3487.5 4700)
;;;                                :median #(3700 3700 5000)
;;;                                :q3 #(4000 3950 5500)
;;;                                :upper #(4775 4800 6300)
;;;                                :outliers #(#() #(2700 4800) #()))))
;;;     :encoding (:y (:field :species :type :nominal :title null))
;;;     :layer ...))

;;; url-data-spec.json
;;; Synthetic example: a spec using URL data reference instead of embedded values.
;;; Tests that write-spec correctly serializes (:data (:url "...")).
