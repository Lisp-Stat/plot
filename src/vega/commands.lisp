;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VEGA -*-
;;; Copyright (c) 2021, 2023 Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:vega)

;;; REPL functions/commands for working with vega visualisations

(defun editor-url (gist)
  "Return the URL for the vega editor for the supplied gist"
  (let+ ((base-url "https://vega.github.io/editor/#/gist/")
	 ((&structure-r/o gist- id history files) gist)
	 ((&structure-r/o file- name) (first files))
	 ((&flet print-version (version)
	    (when version
	      (format nil "~A/" version))))
	 version)

    (when history
      (setf version (history-version (first history))))
    (concatenate 'string base-url id "/" (print-version version) name)))

(defun publish (plot &optional device)
  "Publish a plot via a gist.  Returns a URL to the plot"
  (declare (ignore device))		;ignore for now, to be merged with plot-to-device
  (let* ((gist (cl-gists:create-gist
		(cl-gists:make-gist :description (getf (plot-spec plot) :description)
				    :public t
				    :files `((:name ,(concatenate 'string (plot-name plot) ".json")
					      :content ,(write-spec plot)))))))
    (values (editor-url gist) gist)))

(defun edit (plot &key (browser *default-browser-command*) (browser-options *default-browser-options*))
  "Open vega editor and display PLOT"
  (declare (type plot::browser-specifier browser))
  (let+ (((&values url gist) (publish plot)))
     (uiop:launch-program
      (append (list (alexandria:assoc-value plot:*browser-commands* (if (eq browser :chrome-app-mode) :chrome browser)))
	      (cl-ppcre:split "\\s+" (case browser
				       (:chrome-app-mode (plot::encode-chrome-app-options browser-options url))
				       (:chrome (plot::encode-chrome-options browser-options url))
				       (:firefox (plot::encode-firefox-options browser-options url))
				       (t url))))
      :ignore-error-status t)
    (cl-gists:delete-gist gist)))

;; (defun view
;;  "View on the most capable display, local webview, heavy broswer, REPL (via spark)
;; Implement this once Plotview is working.  View a data-frame or plot.

;; (defun export
;;   "Export a filespec, vega-data, Lisp-Stat date, HTML, PNG, SVG, etc. from a plot
;;   Perhaps we need an 'as-vega' function for a data-frame, like we have an 'as-plist'.  Should probably also implement 'as-csv'.  Convenience wrappers over 'encode' and 'decode'

;; (defun import
;;   Do we need an import function?


