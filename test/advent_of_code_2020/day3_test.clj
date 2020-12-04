(ns advent-of-code-2020.day3-test
  (:require [clojure.test :refer :all]))

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

(defn square?
  [row position]
  (let [path (get row (calculate-position row position))]
    (= path ".")))

(defn bool-to-count
  [detect-function row position]
  (if (detect-function row position)
    1
    0)
  )

(defn traverse-path
  [map tree-count square-count position]
  (if (empty? map)
    {:trees tree-count :squares square-count}
    ;TODO: what happens when I use a let block to calculate new tree and square count? Does tail recursion still work?
    (traverse-path
      (rest map)
      (+ tree-count (bool-to-count tree? (first map) (+ position 3)))
      (+ square-count (bool-to-count square? (first map) (+ position 3)))
      (+ position 3))
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
  (testing "detect square"
    (let [input ["." "#" "."]
          expected true
          actual (square? input 2)]
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
  (testing "detect square on expanded row"
    (let [input ["." "#" "."]
          expected true
          actual (square? input 3)]
      (is (= actual expected))))
  (testing "detect tree on expanded row"
    (let [input ["." "#" "."]
          expected true
          actual (tree? input 4)]
      (is (= actual expected))))
  (testing "detect empty map"
    (let [map []
          expected {:trees 0 :squares 0}
          actual (traverse-path map 0 0 3)]
      (is (= actual expected))))
  (testing "find tree in map"
    (let [map [["." "." "#"]]
          expected {:trees 1 :squares 0}
          position 2
          actual (traverse-path map 0 0 position)]
      (is (= actual expected))))
  (testing "find square in map"
    (let [map [["." "." "#"]]
          position 1
          expected {:trees 0 :squares 1}
          actual (traverse-path map 0 0 position)]
      (is (= actual expected))))
  (testing "find trees and squares"
    (let [map [["#" "." "#"]
               ["." "#" "#"]
               ["." "." "#"]]
          position 0
          expected {:trees 1 :squares 2}
          actual (traverse-path map 0 0 position)]
      (is (= actual expected))))
  (testing "find trees and squares"
    (let [map (parse-input "resources/input-day3-example")
          position 0
          expected {:trees 7 :squares 3}
          actual (traverse-path (rest map) 0 0 position)]
      (is (= actual expected))))
  (testing "find trees for task 1"
    (let [map (parse-input "resources/input-day3")
          position 0
          expected {:squares 36 :trees 286}
          actual (traverse-path (rest map) 0 0 position)]
      (is (= actual expected))))
  )
