;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2022-2023 Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:vega)

(defparameter *chart-types* '((:point . "Scatter plot")(:bar . "Bar chart")(:line . "Line plot")(:area . "Area chart")
			      (:rect . "Table plot / heatmap")(:arc . "Pie/donut chart")(:boxplot . "Boxplot"))
  "Map Vega-Lite mark types to plot types")

(defparameter *all-plots* (make-hash-table :test 'equal)
  "Global table of plots")

(defun show-plots ()
  "Show all plots in the current environment"
  (loop for i = 0 then (1+ i)
	for value being the hash-values of *all-plots*
	do  (format t "~%~A: ~A~%" i value)))


(defclass vega-plot (plot:plot) ())
(defgeneric write-html (plot &optional html-loc spec-loc))
(defgeneric write-spec (plot &key spec-loc data-url data-loc))

(defun make-plot (name &optional
			 data
			 (spec '("$schema" "https://vega.github.io/schema/vega-lite/v5.json")))
  "Plot constructor"
  (make-instance 'vega-plot :name name
			    :data data
			    :spec spec))

(defmethod print-object ((p vega-plot) stream)
  (let ((plot-type (assoc-value *chart-types* (getf (plot-spec p) :mark)))
	(desc (getf (plot-spec p) :description))
	(name (plot-name p)))
    (print-unreadable-object (p stream)
      (format stream
	      "PLOT ~A: ~A~%~A"
	      (if name name "Unnamed ")
	      plot-type
	      desc))))

(defmethod describe-object ((p vega-plot) stream)
  (let ((plot-type (assoc-value *chart-types* (getf (plot-spec p) :mark)))
	(desc (getf (plot-spec p) :description))
	(data (plot-data p))
	(name (plot-name p)))
    (format stream "~A~%" (if name name "Unnamed "))
    (format stream "  ~A of ~A~%" plot-type (name data))
    (format stream "  ~A~%" desc)))

;;;
;;; These work with most specifications.  Those that have multiple
;;; 'data' properties, or a 'data' property at other than the top
;;; level must be encoded with yason:encode
;;;

(defun %defplot (name spec &optional (schema "https://vega.github.io/schema/vega-lite/v5.json"))
  "A PLOT constructor that moves :data from the spec to the PLOT object.
By putting :data onto the plot object we can write it to various locations and add the neccessary transformations to the spec."
  (let ((data (getf spec :data))
	(given-schema (getf spec "$schema")))

    (assert (plistp spec) () "Error spec is not a PLIST")
    (assert (or (plistp data)
		(typep data 'quri.uri:uri)
		(typep data 'df:data-frame))
	    () "Error data must be a PLIST, URI or DATA-FRAME, not a ~A" (type-of data))

    ;; If DATA is a DATA-FRAME, and it has a name, use it as the title if one wasn't provided
    #+nil
    (if (and (typep data 'df:data-frame) ;we may not want a title
	     (slot-boundp data 'name)
	     (not (getf spec :title)))
	(setf (getf spec :title) (name data)))

    (unless given-schema
      (setf (getf spec "$schema") schema))
    (make-plot (symbol-name name) data spec))) ;TODO update plot:plot class and remove DATA slot

(defmacro defplot (name &body spec)
  "Define a plot NAME. Returns an object of PLOT class bound to a symbol NAME.  Adds symbol to *all-plots*."
  `(progn
     (defparameter ,name (%defplot ',name ,@spec))
     (setf (gethash (plot-name ,name) *all-plots*) ,name)
     ,name))				;Return the plot instead of the list


(defmethod write-spec ((p vega-plot) &key
				       spec-loc
				       data-url
				       data-loc
				       (data (plot-data p)))
  "Write PLOT components to source locations and update spec's data url."
  (let ((spec (plot-spec p))
	(yason:*symbol-encoder*     'encode-symbol-as-metadata) ;not just meta-data, to JavaScript as well
	(yason:*symbol-key-encoder* 'encode-symbol-as-metadata))

    (when (not (or spec-loc data-url data-loc)) ;no locations set, return a string
      (return-from write-spec
	(with-output-to-string (s)
	  (yason:encode spec s))))

    (typecase spec-loc
      (pathname
       (ensure-directories-exist spec-loc)
       (with-open-file (s spec-loc :direction :output
				   :if-exists :supersede
				   :if-does-not-exist :create)
	 (yason:encode spec s)))
      (stream (yason:encode spec spec-loc))
      ;; (url ...
      )
    (typecase data-loc
      (pathname
       (ensure-directories-exist data-loc)
       (with-open-file (s data-loc :direction :output
				   :if-exists :supersede
				   :if-does-not-exist :create)
	 (yason:encode data s)))
      (stream (yason:encode data data-loc))
      ;; (url ... ; url should be the first item in the type case so we don't write to disk if both remote url and data-loc is specified
      )
    (values spec-loc data-url data-loc)))


(defmethod write-html ((p vega-plot) &optional html-loc spec-url)
  "Write HTML to render a plot. HTML-LOCATION can be a FILESPEC, quri URI or cl-gist GIST.
Note: Only FILESPEC is implemented."
  (setf (cl-who:html-mode) :html5)
  (let ((plot-pathname (cond ((uiop:file-pathname-p html-loc) html-loc)
			     ((uiop:directory-pathname-p html-loc) (uiop:merge-pathnames*
								    (uiop:pathname-directory-pathname html-loc)
								    (make-pathname :name (string-downcase (plot-name p))
										   :type "html")))
			     (t (uiop:merge-pathnames* (uiop:physicalize-pathname #P"PLOT:TEMP;")
					;(uiop:pathname-directory-pathname plot:*temp*) ;unwind move to physical paths
						       (make-pathname :name (string-downcase (plot-name p))
								      :type "html")))))
	(style (lass:compile-and-write '(html :height 100%
					 (body :height 100%
					       :display flex
					       :justify-content center
					       :align-items center)))))
    (ensure-directories-exist plot-pathname)
    (with-open-file (f plot-pathname :direction :output :if-exists :supersede)
      (who:with-html-output (f)
	(:html
	 (:head
	  (:style (who:str style))
	  (:script :type "text/javascript" :src "https://cdn.jsdelivr.net/npm/vega@5")
	  (:script :type "text/javascript" :src "https://cdn.jsdelivr.net/npm/vega-lite@5")
	  (:script :type "text/javascript" :src "https://cdn.jsdelivr.net/npm/vega-embed@6"))
	 (:body
	  (:div :id "vis")
	  (:script
	   "var spec = "
	   (if spec-url
	       spec-url
	       (write-spec p :spec-loc f))
	   "; vegaEmbed(\"#vis\", spec).then(result => console.log(result)).catch(console.warn);")))))
    plot-pathname))


;; Note dependency on vega:write-html.  write-html is generic, but defined in the vega package.
(defun plot:plot (spec)
  "Render a Vega-Lite specification, SPEC, after saving it to a file"
  (plot-from-file (write-html spec)))

