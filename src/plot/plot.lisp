;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: PLOT -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:plot)

;;; Definitions that can be used in plots of any type.

(defclass plot ()
  ((name :initarg :name :initform nil :accessor plot-name)
   (data :initarg :data :initform nil :accessor plot-data
	 :documentation "A DATA-FRAME OR PLIST of key/column pairs.  Internally, a PLIST will will be converted to a DATA-FRAME by the rendering functions.  A PLIST can sometimes be more convenient for ad-hoc plotting.")
   (spec :initarg :spec :initform nil :accessor plot-spec
	 :documentation "The plot specification in PLIST format.  The PLIST is passed to YASON for encoding to the backend specific JSON.  See the file encode.lisp in the Vega backend for examples of how this is done."))
   (:documentation "Base class for plots"))

(defun make-plot (name &optional data spec)
  "Plot constructor"
  (make-instance 'plot :name name
		       :data data
		       :spec spec))

;;; More specific plots, like Vega-lite, have definitions of
;;; print-object. If the plot:plot class is going to be abstract, do
;;; we need one at this level?
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
  (declare (type browser-specifier browser))
  (let ((plot-file (namestring (truename filespec))))
    #+win32 (setf plot-file (concatenate 'string "file:///" plot-file))
    #+(or macos darwin linux) (setf plot-file (concatenate 'string "file://" plot-file))
    (uiop:launch-program  (append (list (alexandria:assoc-value plot:*browser-commands* (if (eq browser :chrome-app-mode) :chrome browser)))
				  (cl-ppcre:split "\\s+" (if (eq browser :chrome-app-mode)
							     (progn (if (assoc "app" browser-options :test 'string=)
									(setf (cdr (assoc "app" browser-options :test 'string=)) plot-file))
								    (encode-chrome-options browser-options))
							     plot-file)))
			  :ignore-error-status t)))
