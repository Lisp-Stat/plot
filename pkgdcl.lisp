;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:plot
  (:use #:cl)
  (:export
   #:plot-from-file
   #:*browser-commands*
   #:*default-browser-command*
   #:*default-browser-options*))


