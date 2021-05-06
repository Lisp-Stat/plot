;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem :plot
  :version     (:read-file-form "version.sexp")
  :description "Plots for Common Lisp"
  :long-description "A plotting system for Common Lisp"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on (#:lass
	       #:cl-who
	       #:alexandria)
  :serial t
  :components ((:file "pkgdcl")
	       (:file "init")
	       (:file "browser")
	       (:file "plot")))

(asdf:defsystem :plot/text
  :version     "0.1"
  :description "Text based plotting"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on  (#:select		; https://github.com/Lisp-Stat/select
		#:iterate	        ; https://gitlab.common-lisp.net/iterate/iterate
		#:cl-spark)		; https://github.com/tkych/cl-spark
  :pathname    "src/text/"
  :components  ((:file "pkgdcl")
		(:file #:histogram)))

(asdf:defsystem :plot/vglt
  :version     "0.1"
  :description "Plotting with vega lite"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on (#:plot
	       #:yason
	       #:let-plus
	       #:dfio)
  :serial t
  :pathname    "src/vglt/"
  :components ((:file "pkgdcl")
	       (:file "data")
	       (:file "spec")
	       (:file "plots")))
  ;; :in-order-to ((test-op (test-op "plot/tests/vega")))) ;; TODO: Add tests for Vega/JSON functions

#+nil
(asdf:defsystem :plot/tests
  :description "Unit tests for PLOT."
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on (#:plot
               #:parachute)
  :serial t
  :components ((:file "plot-tests")))
