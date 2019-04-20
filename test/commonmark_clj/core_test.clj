(ns commonmark-clj.core-test
  (:require [clojure.test :refer :all]
            [commonmark-clj.core :refer :all]))

(deftest test-parsing-empty-document
  (is (= (parse "") {:type :document, :parent nil, :children nil, :string-value nil})))

(deftest test-parsing-single-paragraph
  (is (= (parse "hello\nworld") {:type :paragraph})))
