;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "plot"
  :version     "2.0"
  :description "Plots for Common Lisp"
  :long-description "A plotting system for Common Lisp"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :homepage    "https://lisp-stat.dev/docs/tasks/plotting/"
  :bug-tracker "https://github.com/Lisp-Stat/plot/issues"
  :source-control (:git "https://github.com/Lisp-Stat/plot.git")
  :licence     :MS-PL
  :depends-on ("cl-ppcre"		;browser command line option parsing
	       "alexandria"
	       "alexandria+"
	       "data-frame")
  :serial t
  :pathname "src/plot/"
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

(defsystem "plot/vega"
  :version     "2.0"
  :description "Plotting with Vega & Vega-Lite"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("plot"
	       "lass"
	       "cl-who"
	       "quri"
	       "yason"
	       "dfio"
	       "let-plus"
	       "local-time"
	       "duologue")
  :serial t
  :pathname    "src/vega/"
  :components ((:file "pkgdcl")
	       (:file "init")
	       (:file "data")
	       (:file "plot")
	       (:file "device")
	       (:file "encode")
	       (:file "utilities")
	       (:file "statistics")
	       (:file "vega-datasets"))
  :in-order-to ((test-op (test-op "plot/vega/tests"))))

(defsystem "plot/vega/tests"
  :description "Unit tests for Vega plotting"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("plot/vega" "parachute")
  :serial t
  :pathname "tests/"
  :components ((:file "tstpkg")
	       (:file "vega-tests"))
  :perform (test-op (o s)
  		    (symbol-call :vega-tests :run-tests)))
