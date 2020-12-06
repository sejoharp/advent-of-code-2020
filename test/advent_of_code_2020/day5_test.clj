(ns advent-of-code-2020.day5-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))


(defn to-binary [code zero one]
  (-> code
      (str/replace zero "0")
      (str/replace one "1")
      (Integer/parseInt 2))
  )


(defn to-row [code]
  (to-binary code "F" "B"))


(defn to-column [code]
  (to-binary code "L" "R"))

(defn to-seat-id [row column]
  (+ column (* row 8))
  )

(defn position [code]
  (let [row-code (subs code 0 7)
        row (to-row row-code)
        column-code (subs code 7 10)
        column (to-column column-code)]
    {:row row :column column :seat-id (to-seat-id row column)}
    ))



(defn highest-seat [seats]
  (->> seats
       (map :seat-id)
       (sort >)
       first
       ))

(defn get-highest-seat-id-from-file [file]
  (->> file
       slurp
       str/split-lines
       (map position)
       highest-seat)
  )
(deftest day5-task1
  (testing "transforms F and B into binay number"
    (is (= (to-row "FBFBBFF") 44)))
  (testing "transforms R and L into binay number"
    (is (= (to-column "RLR") 5)))
  (testing "computes seat id"
    (is (= (to-seat-id 44 5) 357)))
  (testing "transforms code to row and column and seat-id"
    (is (= (position "FBFBBFFRLR") {:row 44 :column 5 :seat-id 357})))
  (testing "get highest seat id"
    (let [seats [{:row 44 :column 4 :seat-id 357}
                 {:row 44 :column 5 :seat-id 358}]]
      (is (= (highest-seat seats) 358))))
  (testing "get highest seat id for task 1"
    (let [file "resources/input-day5"]
      (is (= (get-highest-seat-id-from-file file) 955)))))

