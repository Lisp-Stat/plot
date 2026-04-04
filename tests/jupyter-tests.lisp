;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VEGA-JUPYTER-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package #:vega-jupyter-tests)

(defun run-tests (&optional (report-progress t))
  "Run all optional Jupyter adapter test suites."
  (let ((*print-pretty* t)
        (clunit:*test-output-stream* *standard-output*))
    (run-suite 'vega-jupyter :report-progress report-progress)))

(defsuite vega-jupyter ())

(deftest jupyter-adapter-forwards-plot-owned-mime-data (vega-jupyter)
  "The optional adapter forwards plot-owned MIME data without rebuilding it."
  (let* ((spec '(:mark :bar
                 :description "Simple bar chart"
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (vega::%defplot 'test-jupyter-mime spec)))
    (assert-equalp (mime-representation plot)
                   (mime-bundle-data plot))))
