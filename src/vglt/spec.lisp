;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VGLT -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vglt)

;;; Functions for working with Vega-Lite plot specifications

(defun spec (&optional (schema "https://vega.github.io/schema/vega-lite/v5.json"))
  "Returns an empty Vega-Lite spec with the given schema version"
  `(("$schema" . ,schema)))

(defun add (spec key value)
  "Adds the given KEY / VALUE pair to the Vega-Lite specification"
  (pushnew (cons key value) spec))


;;;
;;; Convenience wrappers for common specifications
;;;

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


(defun pie-chart (data category count &key (title nil) (description nil))
  "Return a Vega-Lite JSON specification for a pie chart"
  (let ((spec '(("$schema" . "https://vega.github.io/schema/vega-lite/v5.json"))))
    (when title (setf spec (acons "title" title spec)))
    (when description (setf spec (acons "description" description spec)))
    (if (typep data 'df:data-frame)
	(setf spec (acons "data" `(("values" . ,(df-to-alist data))) spec))
	(setf spec (acons "data" data spec)))
    (setf spec (acons "mark" "arc" spec))
    (setf spec (acons "encoding" `(("theta" ("field" . ,count) ("type" . "quantitative"))
				   ("color" ("field" . ,category) ("type" . "nominal")))
		      spec))
    (setf spec (acons "view" `(("stroke" "")) spec))
    (reverse spec)))


(defun scatter-plot (data x y &key (title nil) (description nil))
  "Return a Vega-Lite JSON specification for a scatter plot"
  (let ((spec '(("$schema" . "https://vega.github.io/schema/vega-lite/v5.json"))))
    (when title (setf spec (acons "title" title spec)))
    (when description (setf spec (acons "description" description spec)))
    (if (typep data 'df:data-frame)
	(setf spec (acons "data" `(("values" . ,(df-to-alist data))) spec))
	(setf spec (acons "data" data spec)))
    (setf spec (acons "mark" "point" spec))
    (setf spec (acons "encoding" `(("x" ("field" . ,x) ("type" . "quantitative"))
				   ("y" ("field" . ,y) ("type" . "quantitative")))
		      spec))
    (reverse spec)))

;;; This was written as part of the basic tutorial to illustrate
;;; plotting data from a lisp sequence.  In hindsight, this is not a
;;; good pattern because if any other layers are added, the data isn't
;;; present.  By passing the entire data frame to the plotting
;;; function, we can build up layers as we like.  The original
;;; function signature is below, commented out.  The final form of the
;;; function is intended to be used with data frames.  The original
;;; code is left as an example.
;;(defun histogram (data &key (column nil) (title nil) (description nil))
(defun histogram (data column &key (title nil) (description nil))
  "Return a Vega-Lite JSON specification for a histogram plot"
  (assert (or (and (typep data 'df:data-frame) column)
	      (typep data 'sequence))
	  ()
	  "If using a DATA-FRAME, a column must be given")
  (let ((spec '(("$schema" . "https://vega.github.io/schema/vega-lite/v5.json"))))
    (when title (setf spec (acons "title" title spec)))
    (when description (setf spec (acons "description" description spec)))
    (typecase data
      (df:data-frame (setf spec (acons "data" `(("values" . ,(df-to-alist data))) spec)))
      (sequence   (setf spec (acons "data" `(("values" . ,(sequence-to-alist data))) spec)))
      (t (setf spec (acons "data" data spec))))
    (setf spec (acons "mark" "bar" spec))
    (etypecase data
      (df:data-frame (setf spec (acons "encoding" `(("x" ("field" . ,column) ("bin" . t))
						    ("y" ("aggregate" . "count")))
				       spec)))
      (sequence (setf spec (acons "encoding" `(("x" ("field" . "X") ("bin" . t))
					       ("y" ("aggregate" . "count")))
				       spec))))
    (reverse spec)))

(defun box-plot (data x y &key (title nil) (description nil))
  "Return a Vega-Lite JSON specification for a box plot using min-max extent"
  (let ((spec '(("$schema" . "https://vega.github.io/schema/vega-lite/v5.json"))))
    (when title (setf spec (acons "title" title spec)))
    (when description (setf spec (acons "description" description spec)))
    (if (typep data 'df:data-frame)
	(setf spec (acons "data" `(("values" . ,(df-to-alist data))) spec))
	(setf spec (acons "data" data spec))) ;data is a URL
    (setf spec (acons "mark" `(("type" . "boxplot") ("extent" . "min-max")) spec))
    (setf spec (acons "encoding" `(("x" ("field" . ,x) ("type" . "nominal"))
				   ("color" ("field" . ,x) ("type" . "nominal") ("legend" . nil))
				   ("y" ("field" . ,y) ("type" . "quantitative") ("scale" ("zero" . yason:false))))
		      spec))
    (reverse spec)))

#+nil
(defun publish (spec)
  "Write SPEC to a github gist and return the URL for editing"
  nil)
