(ns squares-squared.core-test
  (:require [clojure.test :refer :all]
            [squares-squared.core :refer :all]))

(deftest test-2-2
  (testing "2 and 2"
    (= (squares-squared.core/squares-squared 2 2) ["2"])))

(deftest test-2-4
  (testing "2 and 4"
    (= (squares-squared.core/squares-squared 2 4) [" 2 "
                          "* 4"
                          " * "])))

(deftest test-3-81
  (testing "3 and 81"
    (= (squares-squared.core/squares-squared 3 81) [" 3 "
                           "1 9"
                           " 8 "])))

(deftest test-4-20
  (testing "4 and 20"
    (= (squares-squared.core/squares-squared 4 20) [" 4 "
                           "* 1"
                           " 6 "])))

(deftest test-2-256
  (testing "2 and 256"
    (= (squares-squared.core/squares-squared 2 256) ["  6  "
                            " 5 * "
                            "2 2 *"
                            " 6 4 "
                            "  1  "])))

(deftest test-10-10000
  (testing "10 and 10000"
    (= (squares-squared.core/squares-squared 10 10000) ["   0   "
                               "  1 0  "
                               " 0 1 0 "
                               "* 0 0 0"
                               " * 1 * "
                               "  * *  "
                               "   *   "])))
