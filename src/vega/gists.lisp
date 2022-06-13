;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-GIST -*-
;;; Copyright (c) 2021 Symbolics Pte. Ltd. All rights reserved.
(in-package #:cl-gists)

;; Need to define class and methods

(defclass github-credentials ()
  ((username :initarg :username)
   (password :initarg :password)
   (oauth-token :initarg :oauth-token)))

;; username and password, sufficient to list public gists

(defmethod username ((obj github-credentials))
  (slot-value obj 'username))

(defmethod password ((obj github-credentials))
  (slot-value obj 'password))

(setf *credentials* (make-instance 'github-credentials
                                   :username "Symbolics"
                                   :password "8*6kdr7Gdo"))


;; oauth-token - required to create gists

(defmethod oauth-token ((obj github-credentials))
  (slot-value obj 'oauth-token))

(setq *credentials* (make-instance 'github-credentials
                                   :oauth-token "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"))

(list-gists :username "Symbolics")	;docs wrongly suggest you can use (list-gists), without :username

;; Create a gist
(let ((gist (make-gist :description "sample" ;does not seem to appear in the github web ui
                       :public t			   ;should this be nil by default?
                       :files `((:name "gist-test.vl.json" ;becomes the end of the gist URL
				 :content ,(alexandria:read-file-into-string "s:/src/plot/src/vglt/gist-test.vl.json"))))))
  (create-gist gist))

;; See https://github.com/metasoarous/oz/blob/04b8d9b21994768b07220ab19e5afd13b0343ab9/src/clj/oz/core.clj#L1260

;;https://gist.github.com/Symbolics/d467377062e4a11693fbf0c34ff223aa
;;https://vega.github.io/editor/#/gist/d467377062e4a11693fbf0c34ff223aa/bar-chart.vl.json

;;                                              <-- gist URL -------------------------------> <----------- revision (optional) ------> <- filename ->
;;https://vega.github.io/editor/#/gist/vega-lite/metasoarous/87a5621b0dbec648b2b54f68b3354c3a/e1d471b5a5619a1f6f94e38b2673feff15056146/vega-viz.json


