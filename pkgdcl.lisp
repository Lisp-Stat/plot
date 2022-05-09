;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:plot
  (:use #:cl)
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


