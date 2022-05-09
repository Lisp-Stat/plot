;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: PLOT -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:plot)

;; Normalise *features* across implementations
#+(and (not win32) (or win32 mswindows)) (pushnew :win32 *features*)


;;; Logical pathname setup
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (logical-pathname-translations "plot")
	`(("CACHE;**;*.*.*"    ,(merge-pathnames "plot/**/*.*" (uiop:xdg-cache-home)))
	  ("DATA;**;*.*.*"     ,(merge-pathnames "plot/**/*.*" (uiop:xdg-data-home)))
	  ("TEMP;**;*.*.*"     ,(merge-pathnames "plot/**/*.*" (uiop:default-temporary-directory)))
	  ("CONFIG;**;*.*.*"   ,(merge-pathnames "plot/**/*.*" (uiop:xdg-config-home))))))

;;; From version 2.0, use regular pathnames
(defparameter *cache*  (uiop:xdg-cache-home))
(defparameter *data*   (uiop:xdg-data-home))
(defparameter *temp*   (uiop:default-temporary-directory))
(defparameter *config* (uiop:xdg-config-home))
