;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: PLOT -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package :plot)

(defun plot-from-file (filespec &key (browser *default-browser-command*) (browser-options *default-browser-options*))
  "Open plot specification FILESPEC"
  (let ((plot-file (namestring (truename filespec))))
    #+windows (setf plot-file (concatenate 'string "file:///" plot-file))
    (uiop:launch-program (format nil "~A ~A"
                                 (alexandria:assoc-value plot:*browser-commands* browser)
			         (case browser
			           (:chrome (if (assoc "app" browser-options :test 'string=)
					        (setf (cdr (assoc "app" browser-options :test 'string=)) plot-file))
			            (encode-chrome-options browser-options))
			           (:default plot-file)))
			 :ignore-error-status t)))

