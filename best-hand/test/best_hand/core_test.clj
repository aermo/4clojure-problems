(ns best-hand.core-test
  (:require [clojure.test :refer :all]
            [best-hand.core :refer :all]))

(deftest high-card-test
  (testing "High card"
    (is (= :high-card (best-hand.core/best-hand ["HA" "D2" "H3" "C9" "DJ"])))))

(deftest pair-test
  (testing "Pair"
    (is (= :pair (best-hand.core/best-hand ["HA" "HQ" "SJ" "DA" "HT"])))))

(deftest two-pairs-test
  (testing "Two pairs"
    (is (= :two-pair (best-hand.core/best-hand ["HA" "DA" "HQ" "SQ" "HT"])))))

(deftest threeof-a-kind-test
  (testing "Three of a kind"
    (is (= :three-of-a-kind (best-hand.core/best-hand  ["HA" "DA" "CA" "HJ" "HT"])))))

(deftest straight-five-high-test
  (testing "Straight, five high"
    (is (= :straight (best-hand.core/best-hand ["HA" "DK" "HQ" "HJ" "HT"])))))

(deftest straight-five-low-test
  (testing "Straight, five low"
    (is (= :straight (best-hand.core/best-hand ["HA" "H2" "S3" "D4" "C5"])))))

(deftest full-house-test
  (testing "Full house"
    (is (= :full-house (best-hand.core/best-hand ["HA" "DA" "CA" "HJ" "DJ"])))))

(deftest four-of-a-kind-test
  (testing "Four of a kind"
    (is (= :four-of-a-kind (best-hand.core/best-hand ["HA" "DA" "CA" "SA" "DJ"])))))

(deftest straight-flush-test
  (testing "Straight Flush"
    (is (= :straight-flush (best-hand.core/best-hand ["HA" "HK" "HQ" "HJ" "HT"])))))
