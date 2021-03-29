;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VGLT -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package :vglt)

;;; Functions for working with Vega-Lite plot specifications

(defun spec (schema) ; currently hard-coded to v5
  "Returns an empty Vega-Lite spec with the given schema version"
  `(("$schema" . "https://vega.github.io/schema/vega-lite/v5.json")))

(defun add (spec key value)
  "Adds the given KEY / VALUE pair to the Vega-Lite specification"
  (pushnew (cons key value) spec))



;;; Convenience wrappers for common specifications

(defun bar-chart (data x y &key (title nil) (description nil))
  "Return a Vega-Lite JSON specification for a bar chart"
  (let ((spec '(("$schema" . "https://vega.github.io/schema/vega-lite/v5.json"))))
    (when title (setf spec (acons "title" title spec)))
    (when description (setf spec (acons "description" description spec)))
    (if (typep data 'df:data-frame)
	(setf spec (acons "data" `(("values" . ,(df-to-alist data))) spec))
	(setf spec (acons "data" data spec)))
    (setf spec (acons "mark" "bar" spec))
    (setf spec (acons "encoding" `(("x" ("field" . ,x) ("type" . "nominal") ("axis" ("labelAngle" . 0)))
				   ("y" ("field" . ,y) ("type" . "quantitative")))
		      spec))
    (reverse spec)))

(defun pie-chart (data &key (title nil) (description nil))
  "Return a Vega-Lite JSON specification for a bar chart"
  nil)
;; https://vega.github.io/vega-lite/examples/arc_pie.html
;; https://vega.github.io/vega/examples/pie-chart/
;; https://vega.github.io/vega-lite/examples/layer_arc_label.html


#+nil
(defun publish (spec)
  "Write SPEC to a github gist and return the URL for editing"
  nil)
