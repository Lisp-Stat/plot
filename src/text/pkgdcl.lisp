;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package :plot/text
  (:use #:cl
	#:iterate
	#:select)
  (:export
   #:hist
   #:hist-record))


