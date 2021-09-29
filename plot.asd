;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "plot"
  :version     "1.0"
  :description "Plots for Common Lisp"
  :long-description "A plotting system for Common Lisp"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("lass"
	       "cl-who"
	       "alexandria")
  :serial t
  :components ((:file "pkgdcl")
	       (:file "init")
	       (:file "browser")
	       (:file "plot")))

(defsystem "plot/text"
  :version     "1.0"
  :description "Text based plotting"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on  ("select"
		"num-utils"
		"iterate"
		"cl-spark")
  :pathname    "src/text/"
  :components  ((:file "pkgdcl")
		(:file "histogram")
		(:file "stem-and-leaf")))

(defsystem "plot/vglt"
  :version     "1.0"
  :description "Plotting with vega lite"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("plot"
	       "yason"
	       "let-plus"
	       "dfio/json")
  :serial t
  :pathname    "src/vglt/"
  :components ((:file "pkgdcl")
	       (:file "data")
	       (:file "spec")
	       (:file "plots")))
  ;; :in-order-to ((test-op (test-op "plot/tests/vega")))) ;; TODO: Add tests for Vega/JSON functions

#+nil
(defsystem "plot/tests"
  :description "Unit tests for PLOT"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("plot"
               "parachute")
  :serial t
  :components ((:file "plot-tests")))
