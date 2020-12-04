(ns advent-of-code-2020.day3-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

; parse every line as an array
; the input is then a array of array
; loop over each row - thats down 1
; look at the position plus 3
; do a modulo for the position to handle positions that are out of the map
; count each tree and each square
; do looping as recursive function - head is the current line - tail the next input
; do that as long as the tail has elements
; remove the first line because we start there. We travel directly to the next line and start counting there.

(defn parse-input
  [file]
  (->> file
       slurp
       str/split-lines
       (map #(map str %))
       (map vec)
       )
  )

(defn calculate-position
  [row position]
  (let [row-length (count row)]
    (mod position row-length)
    ))

(defn tree?
  [row position]
  (let [path (get row (calculate-position row position))]
    (= path "#")))

(defn tree?-to-number
  [row position]
  (if (tree? row position)
    1
    0)
  )

(defn traverse-path
  [world tree-count position right-offset down-offset]
  (if (< (count (drop down-offset world)) down-offset)
    tree-count
    ;TODO: what happens when I use a let block to calculate new tree and square count? Does tail recursion still work?
    (let [current-position (+ position right-offset)
          current-world (drop down-offset world)
          new-tree-count (+ tree-count (tree?-to-number (first current-world) current-position))]
      (traverse-path
        current-world
        new-tree-count
        current-position
        right-offset
        down-offset))
    )
  )

(deftest day3-task1
  (testing "parse-input"
    (let [input "resources/input-day3-sample"
          expected [["." "." "#"]
                    ["." "#" "#"]]
          actual (parse-input input)]
      (is (= actual expected))
      (is (vector? (first actual)))))
  (testing "detect tree"
    (let [input ["." "#" "#"]
          expected true
          actual (tree? input 1)]
      (is (= actual expected))))
  (testing "calculate position on overlapping row"
    (let [row ["." "#" "."]
          position 3
          expected 0
          actual (calculate-position row position)]
      (is (= actual expected))))
  (testing "calculate position on overlapping row"
    (let [row ["." "#" "."]
          position 4
          expected 1
          actual (calculate-position row position)]
      (is (= actual expected))))
  (testing "detect tree on expanded row"
    (let [input ["." "#" "."]
          expected true
          actual (tree? input 4)]
      (is (= actual expected))))
  (testing "detect empty map"
    (let [world []
          start 3
          expected 0
          actual (traverse-path world 0 start 3 1)]
      (is (= actual expected))))
  (testing "find tree in map"
    (let [world [["." "." "#"]
                 ["#" "." "#"]]
          expected 1
          start 0
          actual (traverse-path world 0 start 2 1)]
      (is (= actual expected))))
  (testing "find trees"
    (let [world [["#" "." "#"]
                 ["." "#" "#"]
                 ["." "." "."]]
          start 1
          expected 0
          actual (traverse-path world 0 start 2 1)]
      (is (= actual expected))))
  (testing "find trees and squares"
    (let [map (parse-input "resources/input-day3-example")
          start 0
          expected 7
          actual (traverse-path map 0 start 3 1)]
      (is (= actual expected))))
  (testing "find trees for task 1"
    (let [map (parse-input "resources/input-day3")
          start 0
          expected 286
          actual (traverse-path map 0 start 3 1)]
      (is (= actual expected))))
  )

(deftest day3-task2
  (testing "detects finish when we skip rows"
    (let [world [["#" "." "#"]
                 ["." "#" "#"]]
          start 0
          right-offset 1
          down-offset 2
          expected 0
          actual (traverse-path world 0 start right-offset down-offset)]
      (is (= (:trees actual) (:trees expected)))))
  (testing "find trees"
    (let [world [["#" "." "#"]
                 ["." "." "#"]
                 ["." "." "#"]]
          start 0
          right-offset 1
          down-offset 1
          expected 1
          actual (traverse-path world 0 start right-offset down-offset)]
      (is (= actual expected))))
  (testing "find trees"
    (let [world (parse-input "resources/input-day3-example")
          start 0
          right-offset 1
          down-offset 1
          expected 2
          actual (traverse-path world 0 start right-offset down-offset)]
      (is (= (:trees actual) (:trees expected)))))
  (testing "find trees"
    (let [world (parse-input "resources/input-day3-example")
          start 0
          right-offset 5
          down-offset 1
          expected 3
          actual (traverse-path world 0 start right-offset down-offset)]
      (is (= (:trees actual) (:trees expected)))))
  (testing "find trees"
    (let [world (parse-input "resources/input-day3-example")
          start 0
          right-offset 1
          down-offset 2
          expected 2
          actual (traverse-path world 0 start right-offset down-offset)]
      (is (= (:trees actual) (:trees expected)))))
  (testing "multiply all trees for task 2 example"
    (let [world (parse-input "resources/input-day3-example")
          start 0
          paths [
                 {:right 1 :down 1}
                 {:right 3 :down 1}
                 {:right 5 :down 1}
                 {:right 7 :down 1}
                 {:right 1 :down 2}
                 ]
          expected 336
          actual (->> paths
                      (map #(traverse-path world 0 start (:right %) (:down %)))
                      (reduce *)
                      )]
      (is (= actual expected))))
  (testing "multiply all trees for task 2"
    (let [world (parse-input "resources/input-day3")
          start 0
          paths [{:right 1 :down 1}
                 {:right 3 :down 1}
                 {:right 5 :down 1}
                 {:right 7 :down 1}
                 {:right 1 :down 2}]
          expected 3638606400
          actual (->> paths
                      (map #(traverse-path world 0 start (:right %) (:down %)))
                      (reduce *)
                      )]
      (is (= actual expected))))
  )
