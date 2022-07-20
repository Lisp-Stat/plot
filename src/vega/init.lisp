;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VEGA -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; Default to using plists for plot encoding. You can overide this on
;;; an individual basis by rebinding before calling a write or
;;; encoding function, but you probably don't want to when using this
;;; package.
(setf yason:*list-encoder* 'yason:encode-plist)
