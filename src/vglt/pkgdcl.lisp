;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:vglt
    (:use #:cl
	  #:dfio
	  #:select
	  #:let-plus)
  (:import-from #:dfio #:df-to-alist)
  (:export
   #:spec
   #:add

   #:plot
   #:save-plot

   ;; Convenience wrappers for commonly used plots
   #:bar-chart
   #:pie-chart
   #:scatter-plot
   #:histogram
   #:box-plot))


