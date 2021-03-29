;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: VGLT -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package :vglt)

(defun save-plot (spec &key (embed-spec nil) (filespec nil)) ; TODO add filename
  "Saves SPEC as JavaScript and HTML suitable for viewing with a browser into file specified by FILESPEC
SPEC is a SYMBOL who's value is the lisp specification for the plot. If FILESPEC is not given, SYMBOL-NAME is used for the HTML filename and it is written to the PLOT:CACHE; directory. EMBED-SPEC indicates whether or not to embed the specification into the JavaScript, currently this option is not honored; it will be used when the websockets REPL is implemented.
Returns the pathname to the file."

  (check-type spec (and symbol (not null)) "a name (SYMBOL) for a Vega-Lite specification")
  (let ((plot-pathname (make-pathname :host "plot"
				      :directory "cache"
				      :name (symbol-name spec)
				      :type "html"))
	(style (lass:compile-and-write '(html :height 100%
					 (body :height 100%
					       :display flex
					       :justify-content center
					   :align-items center))))
	(yason:*list-encoder* 'yason:encode-alist))

    (setf embed-spec t)			; temporary see docstring
    (if filespec			; probably not worth importing when/if-let
	(setf plot-pathname filespec))

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
	   "const spec = "
	   (when embed-spec (yason:encode (symbol-value spec) f))
	   "; vegaEmbed(\"#vis\", spec).then(result => console.log(result)).catch(console.warn);")))))
    plot-pathname))

#+nil
(defun plot (spec)
  "Open an interactive plot from the REPL."
  nil)
