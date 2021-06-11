;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:plot/text
  (:use #:cl
	#:iterate
	#:select)
  (:import-from :num-utils :e/ :efloor :emod :sequence-minimum :sequence-maximum)
  (:export
   #:hist
   #:hist-record
   #:stem-and-leaf))


