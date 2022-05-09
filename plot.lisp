;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: PLOT -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:plot)

;;; Definitions that can be used in plots of any type.

(defclass plot ()
  ((name :initarg :name :initform nil :accessor plot-name)
   (data :initarg :data :initform nil :accessor plot-data)	;alist of key/column pairs
   (spec :initarg :spec :initform nil :accessor plot-spec))	;plot specification, lisp format
   (:documentation "Base class for plots"))

(defun make-plot (name &optional data spec)
  "Plot constructor"
  (make-instance 'plot :name name
		       :data data
		       :spec spec))

;;; More specific plots, like Vega-lite, have definitions of print-object. Do we need one at this level?
#+nil
(defmethod print-object ((p plot) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a" (symbol-name p))))

(defgeneric write-plot (plot device)
  (:documentation "Write PLOT to DEVICE
May not be immediately visible to the user, depending on the device.
A device could be a webserver, where a PUT operation would write the plot, locations on disk, a github gist, etc."))

;; General function to open a browser to display a file
(defun plot-from-file (filespec &key (browser *default-browser-command*) (browser-options *default-browser-options*))
  "Open FILESPEC with browser. FILESPEC must be displayable by the browser, e.g. HTML."
  (let ((plot-file (namestring (truename filespec))))
    #+win32 (setf plot-file (concatenate 'string "file:///" plot-file))
    #+(or macos darwin linux) (setf plot-file (concatenate 'string "file://" plot-file))
    (uiop:launch-program  (append (list (alexandria:assoc-value plot:*browser-commands* browser))
				  (cl-ppcre:split "\\s+" (case browser
							   (:chrome (if (assoc "app" browser-options :test 'string=)
									(setf (cdr (assoc "app" browser-options :test 'string=)) plot-file))
							    (encode-chrome-options browser-options))
							   (:default plot-file))))
			 :ignore-error-status t)))

