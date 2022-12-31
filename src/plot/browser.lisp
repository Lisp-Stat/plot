;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: PLOT -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:plot)

;;; Helper functions to launch browsers

;;; Note: This experiment has shown that there is too much variability
;;; in browser behaviour to use the command line switches to control
;;; behaviour.  The best solution is to control as much of the browser
;;; behaviour in JavaScript, where the behaviour should be similar
;;; across platforms.

;;; Some things, like launching in app mode, must be done at the
;;; command line and for that you can use what's in this file.  It is
;;; for the above reasons that there is an argument encoding function
;;; specific to each browser that is selected in the arguments to
;;; uiop:launch-program.

(deftype browser-specifier () '(member :chrome-app-mode :chrome :firefox :default))
(declaim (type browser-specifier *default-browser-command*))

;;;
;;; Chrome on Linux
;;;

(defun executable-present-p (potential-executable)
  "Return T if POTENTIAL-EXECUTABLE responds to --version argument"
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program (list potential-executable "--version"))))))

(defun find-chrome-executable-linux ()
  "Find Chrome's executable for Linux distributions"
  ;; Linux distributions unfortunately do not all use the same name for chrome,
  ;; or there may only be chromium installed.
  ;; Partial list of executables: https://unix.stackexchange.com/questions/436835/universal-path-for-chrome-on-nix-systems
  (find-if #'executable-present-p
	   (list "google-chrome" "chrome" "google-chrome-stable" "chromium" "chromium-browser")))

;;;
;;; Functions and data for all browsers
;;;

(defparameter *browser-commands*
  (list (cons :chrome #+win32 "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
		      #+(or macos darwin) "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
		      #+linux (find-chrome-executable-linux))
	(cons :firefox #+win32 "C:\\Program Files (x86)\\Mozilla Firefox\\firefox.exe"
		       #+(or macos darwin) "/Applications/Firefox.app/Contents/MacOS/firefox"
		       #+linux "firefox")
	(cons :default #+win32 "explorer"
		       #+(or macos darwin) "open"
		       #-(or macos darwin win32) "xdg-open")
  "Maps browser names to system paths"))

(defparameter *default-browser-options* nil)

(defun %print-alist (stream data &rest args)
  (declare (ignore args)
	   (special arg-format))
  (destructuring-bind (head . tail) data
    (format stream arg-format head (if (consp tail) (first tail) tail))))

(defun encode-application-options (options arg-format)
  "Turns OPTIONS, an alist, into a command line argument list according to ARG-FORMAT"
  ;; Chrome arg-format: "--~A=~A"
  ;; Firefox: "-~A ~A"
  (declare (special arg-format))
  (format nil "~{~/plot::%print-alist/~^ ~}" options))


;;;
;;; Chrome
;;;

(defun encode-chrome-options (options)
  "Encode command line options for Chrome"
  ;; We want to add a --user-data-directory so that --windows-size is honoured
  ;; --app, if present, will have been set by the caller
  ;; We also want --no-default-browser-check and --no-first-run to suppress
  ;; a dialog box shown on startup when an empty data directory is picked.
  ;; See note at top of file about proper way to set window attributesc
  (let ((chrome-options (append (list (cons "user-data-dir"
					    (merge-pathnames "chrome-data"
							     (translate-logical-pathname #P"PLOT:TEMP;")))
				      (cons "no-default-browser-check" 1)
				      (cons "no-first-run" 1))
			     options)))
    (encode-application-options chrome-options "--~A=~A")))

(defun set-chrome-size (size)
  "Set the windows size in *default-browser-options*"
  (setf (cdr (assoc "window-size" *default-browser-options* :test 'string=)) size))

(defparameter *default-chrome-app-options*
  (list (cons "window-size" "800,600")	; This should probably be set in the plot JavaScript
	(cons "app" "foo")))            ; Run without tabs, menus, etc. "foo" ignored



;;;
;;; Set global defaults.
;;;

;;; If passing custom options frequently, set these
(defparameter *default-browser-command* (if (executable-present-p (alexandria:assoc-value *browser-commands* :chrome))
					    :chrome-app-mode
					    :default))
(defparameter *default-browser-options* (when (eq *default-browser-command* :chrome-app-mode) *default-chrome-app-options*))
