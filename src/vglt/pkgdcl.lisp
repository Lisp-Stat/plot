;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package :vglt
    (:use #:cl
	  #:dfio
	  #:select
	  #:let-plus
	  #:dfio.data-column)
  (:export
   #:spec
   #:json-to-data-frame			; Read vega-lite data into df
   #:data-frame-to-json			; Write df data into vega-lite array
   #:df-to-alist                        ; Write df data to alist for further encoding via JSON library

   #:plot
   #:save-plot

   ;; Convenience wrappers for commonly used plots
   #:bar-chart
   #:pie-chart
   #:scatter-plot
   ))


