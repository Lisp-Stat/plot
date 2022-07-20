;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.

(in-package :cl-user)

(uiop:define-package :vglt-tests
  (:documentation "Tests for Vega-Lite plotting")
  (:use :common-lisp
	:vglt
	:parachute))
