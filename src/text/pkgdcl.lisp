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
   #:stem-and-leaf
   #:back-to-back-stem-and-leaf)
  (:documentation "Text based plotting utilties.  These are intended to show summaries, for example stem plots or frequency tables, using text and rendered in the emacs REPL buffer."))


