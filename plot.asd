;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "plot"
  :version     "2.0"
  :description "Plots for Common Lisp"
  :long-description "A plotting system for Common Lisp"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("lass"
	       "cl-who"
	       "cl-ppcre"		;browser command line option parsing
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
  :version     "2.0"
  :description "Plotting with vega lite"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("plot"
	       "quri"
	       "yason"
	       "let-plus"
	       "dfio/json")
  :serial t
  :pathname    "src/vglt/"
  :components ((:file "pkgdcl")
	       (:file "init")
	       (:file "vega-data")
	       (:file "plot")
	       (:file "device")
	       (:file "encode")
	       (:file "utilities"))
  :in-order-to ((test-op (test-op "plot/vglt/tests"))))

(defsystem "plot/vglt/tests"
  :description "Unit tests for Vega Lite plotting"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("plot/vglt" "parachute")
  :serial t
  :pathname "tests/"
  :components ((:file "tstpkg")
	       (:file "vglt-tests"))
  :perform (test-op (o s)
  		    (symbol-call :vglt-tests :run-tests)))
