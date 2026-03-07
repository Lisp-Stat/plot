;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: VEGA-TESTS -*-
;;; Copyright (c) 2022, 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package #:vega-tests)

;;; Test runner
(defun run-tests (&optional (report-progress t))
  "Run all vega test suites. Returns the clunit-report object.

Binds *test-output-stream* to the current *standard-output* because
clunit2 initialises *test-output-stream* at load time; if the system
was loaded with (ql:quickload :silent t) that stream is a null broadcast
stream that discards all output.  Also binds *print-pretty* so the
report formats with line-breaks instead of printing on a single line."
  (let ((*print-pretty* t)
        (clunit:*test-output-stream* *standard-output*))
    (run-suite 'vega :report-progress report-progress)))

;;; Root suite
(defsuite vega ())

;;; Child suites
(defsuite write-spec-suite (vega))
(defsuite merge-plists-suite (vega))
(defsuite data-conversion-suite (vega))
(defsuite encoding-suite (vega))
(defsuite commands-suite (vega))

;;; Utility: parse JSON string to hash-table for order-independent comparison
(defun parse-json (json-string)
  "Parse a JSON string into a nested hash-table structure using Yason."
  (yason:parse json-string))

;;; Utility: load a fixture file and return its contents as a string
(defun load-fixture (fixture-name)
  "Load a JSON fixture file from tests/fixtures/ and return as a string."
  (let ((path (asdf:system-relative-pathname "plot/vega/tests"
                                             (format nil "tests/fixtures/~A" fixture-name))))
    (uiop:read-file-string path)))

;;; Utility: string-key plist accessor (getf uses EQ, fails for strings)
(defun getf-string (plist key)
  "Like GETF but uses STRING= for key comparison."
  (loop for (k v) on plist by #'cddr
        when (and (stringp k) (string= k key))
          return v))

;;; Utility: encode a symbol via the Vega symbol encoder to get JSON
(defun encode-symbol-via-plist (sym)
  "Encode SYM through Yason with the Vega symbol encoders.
Returns a JSON string.  Useful for testing encode-symbol-as-metadata
without calling write-spec."
  (let ((yason:*symbol-encoder*     #'vega::encode-symbol-as-metadata)
        (yason:*symbol-key-encoder* #'vega::encode-symbol-as-metadata))
    (with-output-to-string (s)
      (yason:encode (list :test sym) s))))

;;; Utility: construct a mock gist for editor-url tests.
;;; NOTE: Uses internal constructors cl-gists.gist::%make-gist and
;;; cl-gists.history::%make-history.  If cl-gists renames or removes
;;; these, this helper is the single place to update.
(defun make-test-gist (&key id filename (version nil version-supplied-p))
  "Construct a mock cl-gists gist struct for testing.
When VERSION is supplied the gist includes a history entry."
  (let ((file (cl-gists:make-file :name filename)))
    (if version-supplied-p
        (cl-gists.gist::%make-gist :id id
				   :files (list file)
				   :history (list (cl-gists.history::%make-history :version version)))
        (cl-gists.gist::%make-gist :id id
				   :files (list file)))))

;;; Seed test — validates the harness works
(deftest harness-sanity (vega)
  "Validates that the test harness loads and runs."
  (assert-true t))


;;;
;;; merge-plists tests
;;;

(deftest merge-plists-simple (merge-plists-suite)
  "Simple merge: later values override earlier ones."
  (let ((result (vega::merge-plists '(:a 1 :b 2) '(:b 3 :c 4))))
    (assert-eql 1 (getf result :a))
    (assert-eql 3 (getf result :b))
    (assert-eql 4 (getf result :c))))

(deftest merge-plists-recursive (merge-plists-suite)
  "Recursive merge: nested plists are merged recursively."
  (let* ((result (vega::merge-plists '(:a (:x 1 :y 2))
                                     '(:a (:y 3 :z 4))))
         (nested (getf result :a)))
    (assert-eql 1 (getf nested :x))
    (assert-eql 3 (getf nested :y))
    (assert-eql 4 (getf nested :z))))

(deftest merge-plists-empty-override (merge-plists-suite)
  "Empty override: base is returned unchanged."
  (let ((result (vega::merge-plists '(:a 1 :b 2))))
    (assert-eql 1 (getf result :a))
    (assert-eql 2 (getf result :b))))

(deftest merge-plists-multiple-overrides (merge-plists-suite)
  "Multiple overrides: each successive override takes precedence."
  (let ((result (vega::merge-plists '(:a 1 :b 2)
                                    '(:b 10)
                                    '(:b 20 :c 30))))
    (assert-eql 1  (getf result :a))
    (assert-eql 20 (getf result :b))
    (assert-eql 30 (getf result :c))))

(deftest merge-plists-non-plist-override (merge-plists-suite)
  "Non-plist value replaces plist value entirely."
  (let ((result (vega::merge-plists '(:a (:x 1)) '(:a "string"))))
    (assert-equal "string" (getf result :a))))


;;;
;;; write-spec tests
;;;

(deftest write-spec-simple-bar-chart (write-spec-suite)
  "write-spec produces correct JSON for a simple bar chart."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 28)
                                  (:a "B" :b 55)
                                  (:a "C" :b 43)
                                  (:a "D" :b 91)
                                  (:a "E" :b 81)
                                  (:a "F" :b 53)
                                  (:a "G" :b 19)
                                  (:a "H" :b 87)
                                  (:a "I" :b 52)))
                 :encoding (:x (:field :a :type :nominal
                                :axis (:label-angle 0))
                            :y (:field :b :type :quantitative))))
         (plot (vega::%defplot 'test-bar spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "simple-bar-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-grouped-bar-chart (write-spec-suite)
  "write-spec produces correct JSON for a grouped bar chart."
  (let* ((spec '(:mark :bar
                 :data (:values #((:category "A" :group "x" :value 0.1d0)
                                  (:category "A" :group "y" :value 0.6d0)
                                  (:category "A" :group "z" :value 0.9d0)
                                  (:category "B" :group "x" :value 0.7d0)
                                  (:category "B" :group "y" :value 0.2d0)
                                  (:category "B" :group "z" :value 1.1d0)
                                  (:category "C" :group "x" :value 0.6d0)
                                  (:category "C" :group "y" :value 0.1d0)
                                  (:category "C" :group "z" :value 0.2d0)))
                 :encoding (:x (:field :category)
                            :y (:field :value :type :quantitative)
                            :x-offset (:field :group)
                            :color (:field :group))))
         (plot (vega::%defplot 'test-grouped spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "grouped-bar-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-pie-chart (write-spec-suite)
  "write-spec produces correct JSON for a pie chart."
  (let* ((spec '(:data (:values #((:category 1.0d0 :value 4)
                                  (:category 2.0d0 :value 6)
                                  (:category 3.0d0 :value 10)
                                  (:category 4.0d0 :value 3)
                                  (:category 5.0d0 :value 7)
                                  (:category 6.0d0 :value 8)))
                 :mark :arc
                 :encoding (:theta (:field :value :type :quantitative)
                            :color (:field :category :type :nominal))))
         (plot (vega::%defplot 'test-pie spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "pie-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-donut-chart (write-spec-suite)
  "write-spec produces correct JSON for a donut chart."
  (let* ((spec '(:data (:values #((:category 1.0d0 :value 4)
                                  (:category 2.0d0 :value 6)
                                  (:category 3.0d0 :value 10)
                                  (:category 4.0d0 :value 3)
                                  (:category 5.0d0 :value 7)
                                  (:category 6.0d0 :value 8)))
                 :mark (:type :arc :inner-radius 50)
                 :encoding (:theta (:field :value :type :quantitative)
                            :color (:field :category :type :nominal))))
         (plot (vega::%defplot 'test-donut spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "donut-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-url-data (write-spec-suite)
  "write-spec produces correct JSON for a spec with URL data."
  (let* ((spec `(:data (:url ,(quri:uri "https://raw.githubusercontent.com/vega/vega-datasets/master/data/cars.json"))
                 :mark :point
                 :encoding (:x (:field "horsepower" :type :quantitative)
                            :y (:field "miles-per-gallon" :type :quantitative))))
         (plot (vega::%defplot 'test-url spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "url-data-spec.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-returns-string (write-spec-suite)
  "write-spec with no keyword args returns a string."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (vega::%defplot 'test-string spec))
         (result (vega::write-spec plot)))
    (assert-true (stringp result))))

(deftest write-spec-has-schema (write-spec-suite)
  "write-spec output includes $schema with v6 URL."
  (let* ((spec '(:mark :bar
                 :data (:values #((:a "A" :b 1)))
                 :encoding (:x (:field :a) :y (:field :b))))
         (plot (vega::%defplot 'test-schema spec))
         (parsed (parse-json (vega::write-spec plot))))
    (assert-equal "https://vega.github.io/schema/vega-lite/v6.json"
                         (values (gethash "$schema" parsed)))))


;;;
;;; editor-url tests
;;;

(deftest editor-url-with-history (commands-suite)
  "editor-url constructs correct URL with history version."
  (let ((gist (make-test-gist :id "abc123" :filename "plot.json" :version "v1")))
    (assert-equal "https://vega.github.io/editor/#/gist/abc123/v1/plot.json"
                         (vega::editor-url gist))))

(deftest editor-url-without-history (commands-suite)
  "editor-url constructs correct URL without history."
  (let ((gist (make-test-gist :id "def456" :filename "chart.json")))
    (assert-equal "https://vega.github.io/editor/#/gist/def456/chart.json"
                         (vega::editor-url gist))))


;;;
;;; encoding-suite tests — encode-symbol-as-metadata
;;;

(deftest encode-normal-keyword (encoding-suite)
  "Normal keywords encode as lowercase strings."
  (let ((json (encode-symbol-via-plist :bar)))
    (assert-true (search "\"bar\"" json))))

(deftest encode-camel-case (encoding-suite)
  "Hyphenated keywords encode as camelCase."
  (let ((json (encode-symbol-via-plist :inner-radius)))
    (assert-true (search "\"innerRadius\"" json))))

(deftest encode-camel-case-x-offset (encoding-suite)
  "x-offset encodes as xOffset via camelCase."
  (let ((json (encode-symbol-via-plist :x-offset)))
    (assert-true (search "\"xOffset\"" json))))

(deftest encode-na-as-null (encoding-suite)
  "The symbol NA encodes as JSON null."
  (let ((json (encode-symbol-via-plist 'na)))
    (assert-true (search "null" json))))

(deftest encode-false-as-false (encoding-suite)
  "The symbol FALSE encodes as JSON false."
  (let ((json (encode-symbol-via-plist 'false)))
    (assert-true (search "false" json))))

(deftest encode-symbol-with-type-metadata (encoding-suite)
  "A symbol with :type property emits type metadata in JSON."
  (let ((sym (make-symbol "TEMPERATURE")))
    (setf (get sym :type) :double-float)
    (unwind-protect
         (let ((json (encode-symbol-via-plist sym)))
           (assert-true (search "\"quantitative\"" json))
           (assert-true (search "\"temperature\"" json)))
      (remprop sym :type))))

(deftest encode-symbol-with-label-metadata (encoding-suite)
  "A symbol with :type and :label properties emits both in JSON."
  (let ((sym (make-symbol "SPEED")))
    (setf (get sym :type) :double-float)
    (setf (get sym :label) "Speed (km/h)")
    (unwind-protect
         (let ((json (encode-symbol-via-plist sym)))
           (assert-true (search "\"quantitative\"" json))
           (assert-true (search "\"Speed (km/h)\"" json)))
      (remprop sym :type)
      (remprop sym :label))))

(deftest encode-symbol-with-unit-no-label (encoding-suite)
  "A symbol with :unit but no :label uses unit as title."
  (let ((sym (make-symbol "WEIGHT")))
    (setf (get sym :unit) "kg")
    (unwind-protect
         (let ((json (encode-symbol-via-plist sym)))
           (assert-true (search "\"kg\"" json)))
      (remprop sym :unit))))


;;;
;;; data-conversion-suite tests — df-to-vl-plist
;;;

(deftest df-to-vl-plist-basic (data-conversion-suite)
  "df-to-vl-plist converts a data-frame to a vector of row plists."
  (let* ((df (df:make-df '(:a :b) (list #("X" "Y") #(1 2))))
         (result (df-to-vl-plist df)))
    (assert-true (vectorp result))
    (assert-eql 2 (length result))))

(deftest df-to-vl-plist-row-content (data-conversion-suite)
  "df-to-vl-plist row plists contain correct key-value pairs."
  (let* ((df (df:make-df '(:col-a :col-b) (list #("hello") #(42))))
         (result (df-to-vl-plist df))
         (row (aref result 0)))
    ;; ps:symbol-to-js-string converts :col-a → "colA", :col-b → "colB"
    ;; Row plists have string keys, so use getf-string
    (assert-equal "hello" (getf-string row "colA"))
    (assert-eql 42 (getf-string row "colB"))))

(deftest df-to-vl-plist-preserves-row-count (data-conversion-suite)
  "df-to-vl-plist produces one plist per data-frame row."
  (let* ((df (df:make-df '(:x) (list #(10 20 30 40 50))))
         (result (df-to-vl-plist df)))
    (assert-eql 5 (length result))))


;;;
;;; commands-suite — make-plot constructor
;;;

(deftest make-plot-defaults (commands-suite)
  "make-plot with only a name has nil data and v6 schema in spec."
  (let ((p (make-plot "test-default")))
    (assert-equal "test-default" (plot-name p))
    (assert-false (plot-data p))
    ;; Default spec has string key "$schema"; use getf-string
    (assert-equal "https://vega.github.io/schema/vega-lite/v6.json"
                         (getf-string (plot-spec p) "$schema"))))

(deftest make-plot-with-data-and-spec (commands-suite)
  "make-plot with data and spec stores both correctly."
  (let* ((data '(:values #((:a 1))))
         (spec '(:mark :bar))
         (p (make-plot "test-full" data spec)))
    (assert-equal "test-full" (plot-name p))
    (assert-equalp data (plot-data p))
    (assert-equalp spec (plot-spec p))))


;;;
;;; write-spec-suite — additional plot types (Wave 2)
;;;

(deftest write-spec-line-chart (write-spec-suite)
  "write-spec produces correct JSON for a line chart."
  (let* ((spec '(:mark :line
                 :data (:values #((:x 1 :y 10)
                                  (:x 2 :y 20)
                                  (:x 3 :y 15)))
                 :encoding (:x (:field :x :type :quantitative)
                            :y (:field :y :type :quantitative))))
         (plot (vega::%defplot 'test-line spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "line-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-scatter-chart (write-spec-suite)
  "write-spec produces correct JSON for a scatter plot with inline data."
  (let* ((spec '(:mark :point
                 :data (:values #((:x 1 :y 5)
                                  (:x 2 :y 3)
                                  (:x 3 :y 7)))
                 :encoding (:x (:field :x :type :quantitative)
                            :y (:field :y :type :quantitative))))
         (plot (vega::%defplot 'test-scatter spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "scatter-chart.json"))))
    (assert-equalp expected actual)))

(deftest write-spec-area-chart (write-spec-suite)
  "write-spec produces correct JSON for an area chart."
  (let* ((spec '(:mark :area
                 :data (:values #((:x 1 :y 10)
                                  (:x 2 :y 20)
                                  (:x 3 :y 15)))
                 :encoding (:x (:field :x :type :quantitative)
                            :y (:field :y :type :quantitative))))
         (plot (vega::%defplot 'test-area spec))
         (actual (parse-json (vega::write-spec plot)))
         (expected (parse-json (load-fixture "area-chart.json"))))
    (assert-equalp expected actual)))

