;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package :cl-user)

(uiop:define-package :vega-jupyter-tests
  (:documentation "Tests for the optional Vega Jupyter adapter")
  (:use :common-lisp
	:vega
	:clunit)
  (:import-from :plot
		#:mime-representation)
  (:import-from :jupyter
		#:mime-bundle-data)
  (:export :run-tests))
