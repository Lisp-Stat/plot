;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2022, 2026 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

(defun merge-plists (&rest plists)
  (let ((result '()))
    (dolist (plist plists result)
      (loop for (key val) on plist by #'cddr do
        (let ((existing (getf result key)))
          (setf (getf result key)
                (cond
                  ((and (eq key :layer)
                        (vectorp existing)
                        (vectorp val))
                   (concatenate 'vector existing val))
                  ;; existing behaviour: deep-merge nested plists
                  ((and (listp existing) (listp val))
                   (apply #'merge-plists (list existing val)))
                  ;; default: last writer wins
                  (t val))))))))
