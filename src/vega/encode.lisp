;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; JSON/Vega-lite serialisation

(defmethod yason:encode ((ts timestamp) &optional (stream *standard-output*))
  "Encode a timestamp into an ISO-8601 string"
  (yason:encode (format-timestring nil ts) stream))

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
	 (yason:with-output-to-string* (:stream-symbol s)
	   (yason:encode (ps:symbol-to-js-string sym))
	   (write-char #\, s)
	   (when type
	     (check-type type data-type "a valid data variable type")
	     (yason::encode-key/value "type"
			       (case type
				 (:string       "ordinal")
				 (:double-float "quantitative")
				 (:single-float "quantitative")
				 (:categorical  "nominal") ;TODO if sorted, use 'ordinal' once multi-level is in data-frame
				 (:temporal     "temporal")
				 (:integer      "quantitative")
				 (:bit          "nominal")
				 (:null         "nominal")
				 (t             "ordinal")) ;this shouldn't happen
			       s))
	   (write-char #\, s)
	   (when label
	     (yason::encode-key/value "title" label s))
	   (when (and (not label) unit) ;if no label, use unit if it exists
	     (yason::encode-key/value "title" unit s))))
	(progn
	  (assert (notany #'lower-case-p name))
	  (yason::make-raw-json-output
	   (yason:with-output-to-string* ()
	     (cond ((string= name "NA")
		    (yason:encode nil))
		   ((string= name "FALSE")
		    (yason:encode 'yason::false))
		   (t
		    (yason:encode (ps:symbol-to-js-string sym))))))))))
