;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2021-2022 Symbolics Pte. Ltd. All rights reserved.
(in-package #:vega)

;;; JSON/Vega-lite data manipulation

(defun df-to-vl-plist (df)
  "Convert DF, a data-frame, to a Vega style plist
This is useful when working with a JSON encoder that will take a plist and output a Vega-Lite plot specification."
  (loop for i below (aops:nrow df)
	with lst = nil
	do (loop for k across (keys df)
		 collecting (ps:symbol-to-js-string k) into row
		 collecting (select df i k) into row
		 finally (push row lst))
	finally (return (coerce lst 'vector))))

(defun json-to-data-columns  (source &key map-alist)
  "Read a JSON array and accumulate the values in DATA-COLUMNs, return a list of columns.  Rows are checked to have the same number of elements.  The second value is a list of column names."
  (let (data-columns column-keys)
    (with-input-stream (s source)
      (loop for row in (yason:parse s)
	  do (if data-columns
		 (assert (alexandria:length= data-columns (alexandria:hash-table-values row)))
		 (setf data-columns (loop repeat (length (alexandria:hash-table-keys row)) collect (data-column :map-alist map-alist))))
	  do (if (not column-keys)
		 (setf column-keys (alexandria:hash-table-keys row)))
	  do (mapc #'data-column-add
		   data-columns
		   (mapcar #'princ-to-string
			   (alexandria:hash-table-values row))))) ;Sigh JSON->number->string to reuse data-column-add
    (values data-columns (map 'list #'string-to-symbol column-keys))))

(defun read-vega (source &key (map-alist '((""    . :na)
					   ("NIL" . :na))))
  "Read a stream of Vega-Lite data into a DATA-FRAME
Useful when working with Vega-Lite data sets from external sources."
  (let+ (((&values data-columns column-keys) (json-to-data-columns source :map-alist map-alist))
	 (df (data-frame:alist-df
	      (mapcar (lambda (column-key data-column)
			(cons column-key (data-column-vector data-column)))
		      column-keys data-columns))))
    df))


(defun write-vega-data (data pathspec)
  "Write DATA to PATHSPEC in vega JSON format"
    (let ((yason:*symbol-encoder*     'encode-symbol-as-metadata) ;not just meta-data, to JavaScript as well
	  (yason:*symbol-key-encoder* 'encode-symbol-as-metadata))
      (with-open-file (stream pathspec :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
	(yason:encode (df-to-vl-plist data) stream)))
  (values))				;return nothing. Emacs buffers choke on large data.
