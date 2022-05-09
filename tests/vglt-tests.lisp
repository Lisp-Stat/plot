;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: VGLT-TESTS -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.

(in-package #:vglt-tests)

(defun run-tests (&key (report 'parachute:plain) (test 'vglt))
    (let ((*package* #.*package*))
      (parachute:test (etypecase test
                        (symbol test)
                        (string (find-symbol test *package*)))
                      :report report)))

(define-test vglt)

(define-test write-spec
  :parent vglt)

(define-test embed-spec
  :parent write-spec
  (is = 5 5)
  (is = 5.5 3))
