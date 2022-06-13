;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

(defparameter *chart-types* '((:point . "Scatter plot")(:bar . "Bar chart")(:line . "Line plot"))
  "Map Vega-Lite mark types to plot types")

(defparameter *all-plots* (make-hash-table)
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
  (let ((plot-type (cdr-assoc (getf (plot-spec p) :mark) *chart-types*))
	(desc (getf (plot-spec p) :description))
	(name (plot-name p)))
    (print-unreadable-object (p stream)
      (format stream
	      "PLOT ~A: ~A~%~A"
	      (if name name "Unnamed ")
	      plot-type
	      desc))))

(defun %defplot (name spec &optional (schema "https://vega.github.io/schema/vega-lite/v5.json"))
  "A PLOT constructor that moves :data from the spec to the PLOT object.
By putting :data onto the plot object we can write it to various locations and add the neccessary transformations to the spec."
  (let ((data (getf spec :data))
	(given-schema (getf spec "$schema")))

    (assert (plistp spec) () "Error spec is not a PLIST")
    (assert (or (plistp data)
		(typep data 'df:data-frame)) () "Error data must be a PLIST or DATA-FRAME, not a ~A" (type-of data))

    ;; If DATA is a DATA-FRAME, and it has a name, use it as the title if one wasn't provided
    (if (and (typep data 'df:data-frame)
	     (slot-boundp data 'name)
	     (not (getf spec :title)))
	(setf (getf spec :title) (name data)))

    ;; If the DATA-FRAME has a doc string, use it as the description if one wasn't provided
    (if (and (typep data 'df:data-frame)
	     (slot-boundp data 'doc-string)
	     (not (getf spec :description)))
	(setf (getf spec :description) (doc-string data)))

    (unless given-schema
      (setf (getf spec "$schema") schema))
    (remf spec :data)
    (make-plot (symbol-name name) data spec)))

(defmacro defplot (name &body spec)
  "Define a plot NAME. Returns an object of PLOT class bound to a symbol NAME.  Adds symbol to *all-plots*."
  `(progn
     (defparameter ,name (%defplot ',name ,@spec))
     (setf (gethash (plot-name ,name) *all-plots*) ,name)
     ,name))				;Return the plots instead of the list

(defmethod write-spec ((p vega-plot) &key
				       spec-loc
				       data-url
				       data-loc)
  "Write PLOT components to source locations and update spec's data url."
  (let ((spec (plot-spec p))
	(data (plot-data p))
	(yason:*symbol-encoder*     'encode-symbol-as-metadata)
	(yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase))

    (when (typep data 'plist)
      (setf data (plist-df data)))

    (etypecase data-url
      (string   (setf (getf spec :data) `(:url ,data-url)))
      (quri:uri (setf (getf spec :data) `(:url data-url)))
      (pathname (setf (getf spec :data) `(:url data-url)))

      (null			     ;embed the spec
       (let ((df:*large-data* 50000)	;larger than this can cause browser performance problems
	     (data-size (apply #'* (aops:dims data))))
	 (handler-bind ((df:large-data
			  #'(lambda (c)
			      (invoke-debugger c))))
	   (if (< data-size *large-data*)
	       (setf (getf spec :data) `(:values ,data))
	       (restart-case (signal 'df:large-data :data-size data-size)
		 (take-all ()
		   :report "Take all data"
		   (format t "Taking ~D data points" data-size)
		   (setf (getf spec :data) `(:values ,data)))
		 (take-n (n)
		   :report "Take the first N data points"
		   :interactive (lambda ()
				  (let ((n (duologue:prompt "Number of data points to take: " :parser #'parse-integer :type '(integer 1 *))))
				    (list n)))
		   (let* ((nrows (floor (/ n (aops:ncol data))))
			  (data-slice (select data (range 0 nrows) t)))
		     (format t "Taking ~D rows (~D data points)" nrows n)
		     (setf (getf spec :data) `(:values ,data-slice))))
		 (take-max ()
		   :report "Take up to maximum recommended number"
		   (let* ((nrows (floor (/ df:*large-data* (aops:ncol data))))
			  (data-slice (select data (range 0 nrows) t)))
		     (format t "Taking ~D rows (~D data points)" nrows df:*large-data*)
		     (setf (getf spec :data) `(:values ,data-slice))))))))))

    (typecase spec-loc
      (pathname
       (ensure-directories-exist spec-loc)
       (with-open-file (s spec-loc :direction :output
				   :if-exists :supersede
				   :if-does-not-exist :create)
	 (yason:encode spec s)))
      (stream (yason:encode spec spec-loc))
      ;; (gist ...
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
      ;; (gist ...
      ;; (url ... ; url should be the first item in the type case so we don't write to disk if both remote url and data-loc is specified
      )))

(defmethod write-html ((p vega-plot) &optional html-loc spec-url)
  "Write HTML to render a plot. HTML-LOCATION can be a FILESPEC, quri URI or cl-gist GIST.
Note: Only FILESPEC is implemented."

  #+nil
  (cond ((uiop:directory-pathname-p html-loc) (format t "Directory given~%"))
	((uiop:file-pathname-p html-loc) (format t "Filename given~%"))
	(t (format t "Use default plot directory~%")))

  (setf (cl-who:html-mode) :html5)
  (let ((plot-pathname (cond ((uiop:file-pathname-p html-loc) html-loc)
			     ((uiop:directory-pathname-p html-loc) (uiop:merge-pathnames*
								    (uiop:pathname-directory-pathname html-loc)
								    (make-pathname :name (string-downcase (plot-name p))
										   :type "html")))
			     (t (uiop:merge-pathnames* (uiop:pathname-directory-pathname plot:*temp*)
						       (make-pathname :name (string-downcase (plot-name p))
								      :type "html")))))
	(style (lass:compile-and-write '(html :height 100%
					 (body :height 100%
					       :display flex
					       :justify-content center
					       :align-items center)))))
	 ;; (yason:*symbol-encoder*     'yason:encode-symbol-as-lowercase)
	 ;; (yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase))

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
