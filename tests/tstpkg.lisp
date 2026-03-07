;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022, 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package :cl-user)

(uiop:define-package :vega-tests
  (:documentation "Tests for Vega-Lite plotting")
  (:use :common-lisp
	:vega
	:clunit)
  ;; plot-name, plot-data, plot-spec are exported from :plot but not
  ;; re-exported by :vega, so import them explicitly.
  (:import-from :plot
		#:plot-name
		#:plot-data
		#:plot-spec)
  (:export :run-tests))
