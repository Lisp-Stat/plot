;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:sctplt
    (:use #:cl #:plot #:alexandria #:vega #:let-plus
	  #:num-utils.arithmetic
	  #:num-utils.elementwise
	  #:num-utils.matrix-shorthand)
  (:import-from #:select #:select)
  (:import-from #:data-frame #:make-df)
  (:import-from #:statistics-1 #:linear-regression)
  (:documentation "Scatterplot functions")
  (:export #:fit-line
	   #:fit-lowess))
(in-package #:sctplt)


(defun fit-line (x y &key (color :red))
  "Return Vega-lite encoding to fit a line to scatterplot data."
  (let+ ((points (loop
                   for i across x
                   for j across y
                   collect (list i j)))
         ((&values a m) (linear-regression points))
	 (df (df:make-df '(x y) `(,x ,(map 'vector (lambda (x)
						     (+ (* m x) a))
					   x)))))
    `(:mark (:type :line :color ,color :interpolate :basis)
      :data (:values ,(df-to-vl-plist df))
      :encoding (:x (:field :x :type "quantitative")
		 :y (:field :y :type "quantitative")))))



;;;
;;; Lowess
;;;

(defun eliminate-matrix (matrix rows cols)
  "Gaussian elimination with partial pivoting.  "
  ;; Evaluated for side effect.  A return value of :singular indicates the
  ;; matrix is singular (an error).
  (let ((max 0))
    (loop for i below rows
     do (setf max i)
     do (loop for j from (1+ i) below rows
         do (when (> (abs (aref matrix j i))
                     (abs (aref matrix max i)))
              (setf max j)))
     do (when (zerop (aref matrix max i))
          (return-from eliminate-matrix :singular)) ; error "Singular matrix"
     do (loop for k from i below cols   ; Exchange rows
         do (rotatef (aref matrix i k) (aref matrix max k)))
     do (loop for j from (1+ i) below rows
         do (loop for k from (1- cols) downto i
             do (setf (aref matrix j k)
                      (- (aref matrix j k)
                         (* (aref matrix i k)
                            (/ (aref matrix j i)
                               (aref matrix i i))))))))
    matrix))

(defun substitute-matrix (matrix rows cols)
  (let ((temp 0.0)
        (x (make-array rows :initial-element 0)))
    (loop for j from (1- rows) downto 0
     do (setf temp 0.0)
     do (loop for k from (1+ j) below rows
         do (incf temp (* (aref matrix j k) (aref x k))))
     do (setf (aref x j) (/ (- (aref matrix j (1- cols)) temp)
                            (aref matrix j j))))
    x))

(defun solve-matrix (matrix &optional (destructive T) print-soln)
  "Solve a matrix using Gaussian elimination
   Matrix must be N by N+1
   Assume solution is stored as the N+1st column of the matrix"
  (let ((rows (aops:nrow matrix))
        (cols (aops:ncol matrix))
        (result (if destructive matrix (copy-array matrix))))
    (unless (= (1+ rows) cols)
      (error "Ill formed matrix"))      ; Cryptic error message
    (cond ((eq :singular (eliminate-matrix result rows cols)))
          (T (let ((soln (substitute-matrix result rows cols)))
               (when print-soln
                 (loop for i below rows
                  do (format t "~% X~A = ~A" i (aref soln i))))
               soln)))))

(defun clip (a b c)
  (aops:map-array a (lambda (x) (cond ((< x b) b)
				 ((> x c) c)
				 (t x)))))

;;; See https://gist.github.com/agramfort/850437
(defun lowess (x y &key
		     (f 2/3)
		     (iter 3)
		     (wt-fun (lambda (x)
			       (nu:cube (- 1 (nu:cube x))))))
  "Lowess smoother (robust locally weighted regression).

    Fits a nonparametric regression curve to a scatterplot.
    Parameters
    ----------
    y, x : vector
        The arrays x and y contain an equal number of elements; each pair (x[i], y[i]) defines a data point in the scatterplot.
    f : real
        The smoothing span. A larger value will result in a smoother curve.
    iter : integer
        The number of robustifying iterations. The function will run faster with a smaller number of iterations.
    Returns
    -------
    yest : vector
        The estimated (smooth) values of y."
  (let* ((n (length x))
	 (r (ceiling (* f n)))
	 (y-est (aops:zeros n))
	 (delta (aops:ones n))
	 h w)

    ;; Determine weights
    (setf h (loop
	      with vec = (make-array n :element-type 'double-float :fill-pointer 0)
	      for i from 0 below n
	      do (vector-push (aref (sort (nu:eabs (e- x (aref x i))) #'<) r) vec)
	      finally (return vec))
	  w (aops:map-array
	     (clip (eabs (e/ (e- (aops:recycle x :outer n) (aops:recycle x :inner n))
			     (aops:recycle h :outer n)))
		   0d0 1d0)
	     wt-fun))

    (loop
      for i below iter
      with residuals and s
      do (loop
	   for k below n
	   with weights and b and A and beta
	   do (setf weights (e2* delta (select w t k))
		    b (vec 'double-float (sum (e2* weights y)) (sum (e* weights x y)))
		    A (mx 'double-float
			((nu:sum weights) (nu:sum (e* weights x)))
			((nu:sum (e* weights x)) (nu:sum (e* weights x x))))
		    beta (solve-matrix (aops:stack-cols A b))
		    (aref y-est k) (+ (aref beta 0) (* (aref beta 1) (aref x k)))))
	 (setf residuals (e- y y-est)
	       s (alexandria:median (eabs residuals))
	       delta (clip (e/ residuals (* 6 s)) -1 1)
	       delta (esquare (e- 1 (esquare delta))))
      finally (return y-est))))

(defun fit-lowess (x y &key (color :red))
  "Return the Vega-lite encoding to fit a lowess curve to scatterplot data."
  (let ((df (df:make-df '(x y) `(,x ,(lowess x y)))))
     `(:mark (:type :line :color ,color :interpolate :basis)
       :data (:values ,(df-to-vl-plist df))
       :encoding (:x (:field :x :type "quantitative")
		  :y (:field :y :type "quantitative")))))
