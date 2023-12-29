;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VEGA -*-
;;; Copyright (c) 2022-2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:vega)

;;; A plot DEVICE is a proxy for the streams that have to be
;;; written/read to interact with a plot.

;;; Initially DEVICE was envisioned as a CLOS object, but during
;;; development it turned out to not require anything more than lists.

;;; A DEVICE is a plist specifying locations for the Vega
;;; specification file, the data and the URL. Here are two examples:

;; TODO merge this with the 'publish' function for a single interface for publishing: web, gists, etc.
(defun plot-to-device (device plot)
  "Wrapper over WRITE-SPEC to make saving plots a bit more convenient."
  (let* ((name (plot-name plot))
	 (spec-loc (getf device :spec-loc)) ;TODO add alist destructuring to let+
	 (data-url (getf device :data-url))
	 (data-loc (getf device :data-loc)))

    ;; TODO Sanity check the combinations of keyword parameters

    ;; Some heuristics for the data location filename
    (unless (eql data-url :ignore)
      (setf data-url (cond (;; Generate a URL for the data file relative to the spec file location
			    (and (not data-url) data-loc) (concatenate 'string
								       (enough-namestring data-loc spec-loc)
								       (string-downcase name)
								       "-data.json"))

			   (;; Neither data location nor URL given, embed data in the spec
			    (not (and data-url data-loc)) nil)

			   (;; Location provided. Generate URL relative to spec location.
			    (and data-url data-loc) (typecase data-url
						      (string   (concatenate 'string data-url (string-downcase name) "-data.json"))
						      (quri:uri (quri:render-uri data-url))
						      (pathname (if (uiop:file-pathname-p data-url)
								    (enough-namestring data-url spec-loc)
								    (concatenate 'string
										 (enough-namestring data-url spec-loc)
										 (string-downcase name)
										 "-data.json"))))))))
    (if (uiop:directory-pathname-p spec-loc)
	(setf spec-loc (make-pathname :directory (pathname-directory spec-loc)
				      :type "vl.json"
				      :name (string-downcase name))))
    (if (uiop:directory-pathname-p data-loc)
	(setf data-loc (make-pathname :directory (pathname-directory data-loc)
				      :type "json"
				      :name (string-downcase (concatenate 'string name "-data")))))

    (vega:write-spec plot :spec-loc spec-loc :data-url data-url :data-loc data-loc)))



#|
;;;
;;; Demonstrate various data/url/spec combinations. These aren't
;;; exhaustive, but cover all the common use cases I've encountered
;;;

#+nil
(defparameter vega-desktop '(:spec-loc #P"~/Desktop/plots/"
			     :data-loc #P"~/Desktop/plots/")
  "Data and specification go in the same directory")

#+nil
(defparameter vdsk1 '(:spec-loc #P"~/Desktop/plots/"
		      :data-loc #P"~/Desktop/plots/data/")
  "Put data into a data/ subdirectory")

#+nil
(defparameter vdsk2 '(:spec-loc #P"~/Desktop/plots/"
		      :data-loc #P"~/Desktop/plots/data/"
		      :data-url :ignore)
  "Write specs with data property not at top level")

#+nil
(defparameter hugo-url '(:spec-loc #P"s:/src/documentation/static/plots/"
			 :data-loc #P"s:/src/documentation/static/data/"
			 :data-url "/data/")
  "Device for most plots deployed to the Lisp-Stat website")

#+nil
(defparameter hugo-url-2 '(:spec-loc #P"s:/src/documentation/static/plots/"
			 :data-loc #P"s:/src/documentation/static/data/"
			 :data-url :ignore)
  "Device for plots with a data property not at the top level")

;;; In practice, I define a few of these directories in my lisp init
;;; file. See the end of this file for some tests/examples.

;(and data-url data-loc)
(plot-to-device hp-mpg '(;specify the name of the data file
		  :spec-loc #P"~/Desktop/plots/"
		  :data-url "hp-mpg-data.json"
		  :data-loc #P"~/Desktop/plots/"))

(plot-to-device hp-mpg '(;data in a directory beneath the spec
		  :spec-loc #P"~/Desktop/plots/"
		  :data-url #P"~/Desktop/plots/data/hp-mpg-data.json"
		  :data-loc #P"~/Desktop/plots/data/"))

(plot-to-device hp-mpg `(;data obtained from a remote server
		  :spec-loc #P"~/Desktop/plots/"
		  :data-url ,(quri:uri "https://lisp-stat.dev/")
		  :data-loc #P"~/Desktop/plots/data/")) ; This won't make a difference if the server is remote.

(plot-to-device hp-mpg '(;give spec a name other than the name of the plot
		  :spec-loc #P"~/Desktop/plots/foo.vl.json"
		  :data-url #P"~/Desktop/plots/data/"
		  :data-loc #P"~/Desktop/plots/data/"))


;(and (not data-url) data-loc)
(plot-to-device hp-mpg '(;generate a data url for external data
		  :spec-loc #P"~/Desktop/plots/"
		  :data-loc #P"~/Desktop/plots/"))
(plot-to-device '(; use different data directory
		  :spec-loc #P"~/Desktop/plots/"
		  :data-loc #P"~/Desktop/plots/data/")
hp-mpg)


;(not (and data-url data-loc))
(plot-to-device '(:spec-loc #P"~/Desktop/plots/") hp-mpg) ;embed data into a spec of the same name as the plot
(plot-to-device '(:spec-loc #P"~/Desktop/plots/foo.vl.json") hp-mpg) ;embed the data into a spec of the given name

;; Use pre-defined locations
(plot-to-device vega-desktop hp-mpg)
(plot-to-device vdsk1 hp-mpg)
|#
