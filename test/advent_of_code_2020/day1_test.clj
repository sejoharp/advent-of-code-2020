(ns advent-of-code-2020.day1-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn read-expenses
  [file-path]
  (->> file-path
       slurp
       str/split-lines
       (map #(Integer/parseInt %))
       ))

(defn find-buddy
  [summand1 expenses]
  (cond
    (= (+ summand1 summand1) 2020) [summand1 summand1]
    (empty? expenses) nil
    (= (+ summand1 (first expenses)) 2020) [summand1 (first expenses)]
    :else (find-buddy summand1 (rest expenses))
    )
  )

(defn find-summands
  [expenses]
  (let [summand1 (first expenses)
        tail (rest expenses)
        summands (find-buddy summand1 tail)]
    (if (= (count summands) 2)
      summands
      (find-summands tail)
      )
    )
  )

(defn multiply-2-summands
  [summands]
  (* (first summands) (second summands)))

(defn find-three-summands
  [expenses]
  (-> (for [s1 expenses
            s2 expenses
            s3 expenses
            :let [summary (+ s1 s2 s3)]
            :when (= summary 2020)]
        [s1 s2 s3])
      first
      ))

(defn multiply-3-summands
  [summands]
  (* (first summands) (second summands) (nth summands 2)))

(deftest day1-task1
  (testing "reads input"
    (let [expenses (read-expenses "resources/input-day1")]
      (is (= (count expenses) 200))
      (is (int? (first expenses)))))
  (testing "finds numbers that sums up to 2020"
    (let [expenses [20]
          summands (find-buddy 2000 expenses)]
      (is (= (count summands) 2))
      (is (= (+ (first summands) (second summands)) 2020))))
  (testing "finds numbers that sums up to 2020"
    (let [expenses [1 2000]
          summands (find-buddy 20 expenses)]
      (is (= (count summands) 2))
      (is (= summands [20 2000]))))
  (testing "does not find numbers that sums up to 2020"
    (let [expenses [1 2]
          summands (find-buddy 20 expenses)]
      (is (nil? summands))))
  (testing "finds same number that sums up to 2020"
    (let [expenses []
          summands (find-buddy 1010 expenses)]
      (is (= summands [1010 1010]))))
  (testing "finds number that sums up to 2020"
    (let [expenses [1 1010 2]
          summands (find-summands expenses)]
      (is (= summands [1010 1010]))))
  (testing "finds number that sums up to 2020"
    (let [expenses (read-expenses "resources/input-day1")
          summands (find-summands expenses)]
      (is (= summands [1999 21])))))
(deftest day1-task2
  (testing "multiplies summands"
    (let [expenses (read-expenses "resources/input-day1")
          summands (find-summands expenses)
          product (multiply-2-summands summands)]
      (is (= product 41979))))
  (testing "finds 3 numbers that sums up to 2020"
    (let [expenses (read-expenses "resources/input-day1")
          summands (find-three-summands expenses)]
      (is (= summands [624 277 1119]))))
  (testing "multiplies summands"
    (let [expenses (read-expenses "resources/input-day1")
          summands (find-three-summands expenses)
          product (multiply-3-summands summands)]
      (is (= product 193416912)))))