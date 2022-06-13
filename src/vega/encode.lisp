;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; JSON/Vega-lite serialisation

(defmethod yason:encode ((ts lt:timestamp) &optional (stream *standard-output*))
  "Encode a timestamp into a Vega dateTime object"
  (lt:with-decoded-timestamp (:year year :month month :day day :hour hour :minute minute :sec second) ts
    (yason:with-output (stream)
      (yason:with-object ()
	(yason:encode-object-element "year"   year)
	(yason:encode-object-element "month"  month)
	(yason:encode-object-element "day"    day)
	(yason:encode-object-element "hour"   hour)
	(yason:encode-object-element "minute" minute)
	(yason:encode-object-element "second" second)))))

(defmethod yason:encode ((df data-frame) &optional (stream *standard-output*))
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
	(size (apply #'* (aops:dims df))))
    (when (>= size *large-data*) (signal 'large-data :data-size size))
    (yason:encode (df-to-vl-plist df) stream)))

(defmethod yason:encode ((uri quri:uri) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (quri:render-uri uri stream)))

(defmethod yason:encode ((p pathname) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (format stream "~A" (namestring p))))

(defun encode-symbol-as-metadata (sym)
  "Encode a data frame variable name, SYM, with the meta data about that variable
DATA-FRAME variable names store meta data as symbol properties.
Note: We should have a sentinel property value to indicate a data symbol; we now just check for a property list."
  (let+ ((name (symbol-name sym))
	 ((&plist-r/o (type :type) (unit :unit) (label :label)) (symbol-plist sym)))
    (if (or type unit label)
	(yason::make-raw-json-output
	 (yason:with-output-to-string* ()
	     (yason:with-object ()
	       (yason:encode-object-element "field" (string-downcase name))
	       (when type
		 (check-type type data-type "a valid data variable type")
		 (yason:encode-object-element "type" (case type
						       (:string       "ordinal")
						       (:double-float "quantitative")
						       (:single-float "quantitative")
						       (:categorical  "ordinal") ;TODO if multi-level, use 'nominal' & 'sort'
						       (:temporal     "temporal")
						       (:integer      "quantitative")
						       (:bit          "ordinal")
						       (:null         "ordinal")
						       (t             "ordinal")))) ;this shouldn't happen
	       (when label
		 (yason:encode-object-element "title" label))
	       (when (and (not label) unit) ;if no label, use unit if it exists
		 (yason:encode-object-element "title" unit)))))
	(progn
	  (assert (notany #'lower-case-p name))
	   (string-downcase name)))))

#+nil
(defun encode-symbol-as-metadata (sym)
  "Encode a data frame variable name, SYM, with the meta data about that variable
DATA-FRAME variable names store meta data as symbol properties.
Note: We should have a sentinal property value to indicate a data symbol; we now just check for a property list."
  (let+ (meta-data
	 (name (symbol-name sym))
	 ((&plist-r/o (type :type) (unit :unit) (label :label)) (symbol-plist sym)))
    (if (or type unit label)
	(progn
	  (push "field" meta-data)
	  (push (string-downcase name) meta-data)
	  (when type
	    (check-type type data-type "a valid data variable type")
	    (push "type"  meta-data)
	    (push (case type
		    (:string       "ordinal")
		    (:double-float "quantitative")
		    (:single-float "quantitative")
		    (:categorical  "ordinal") ;TODO if multi-level, use 'nominal' & 'sort'
		    (:temporal     "temporal")
		    (:integer      "quantitative")
		    (:bit          "ordinal")
		    (null         "ordinal"))
		  meta-data))
	  (when label ;TODO if no label, use unit if it exists
	    (push "title"  meta-data)
	    (push label meta-data))
	(reverse meta-data))

	(progn
	  (assert (notany #'lower-case-p name))
	  (string-downcase name)))))
#+nil
(defun encode-symbol-as-metadata (sym)
  "Encode a data frame variable name, SYM, with the meta data about that variable
DATA-FRAME variable names store meta data as symbol properties.
Note: We should have a sentinal property value to indicate a data symbol; we now just check for a property list."
  (let+ ((name (symbol-name sym))
	 (property-list (symbol-plist sym))
	 ((&plist-r/o (type :type) (unit :unit) (label :label)) property-list))
    (declare (ignore unit))
;;    (format t "Label: ~A" label)
    (if property-list
	(yason:with-output (stream)
	  (yason:with-object ()
	    (yason:encode-object-element "field" (string-downcase name))
	    (when type
	      (yason:encode-object-element "type" (case type
						    (string       "ordinal")
						    (double-float "quantitative")
						    (single-float "quantitative")
						    (categorical  "ordinal") ;TODO if multi-level, use 'nominal' & 'sort'
						    (temporal     "temporal")
						    (integer      "quantitative")
						    (bit          "ordinal")
						    (null         "ordinal")))) ;this shouldn't happen
	    (when label ;TODO if no label, use unit if it exists
	      (yason:encode-object-element "title" label))))

	)
    (progn
      (assert (notany #'lower-case-p name))
      (string-downcase name))
    ))
