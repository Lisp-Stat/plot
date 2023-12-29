;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:plot
  (:use #:cl #:alexandria #:alexandria+)
  (:documentation "Functionality common to all plotting.  The plot class is defined here, with each backend subclassing it.  Functions for interacting with display systems, like the browser or Electron/Webview are also defined here")
  (:export
   #:plot
   #:plot-data
   #:plot-spec
   #:plot-name
   #:plot-from-file
   #:*temp*
   #:*data*
   #:*cache*
   #:*config*
   #:*browser-commands*
   #:*default-browser-command*
   #:*default-browser-options*))


