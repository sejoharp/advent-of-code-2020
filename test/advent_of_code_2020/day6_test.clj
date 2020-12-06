(ns advent-of-code-2020.day6-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]))

(defn group-answers [input]
  (-> input
      (string/replace "\n\n" "|")
      (string/replace "\n" "")
      (string/split #"\|")))

(defn count-answers-per-group [grouped-answers]
  (->> grouped-answers
       (map set)
       (map count)
       ))

(defn count-all-answers [file]
  (->> file
       slurp
       group-answers
       count-answers-per-group
       (reduce +)))

(deftest day6-task1
  (testing "group answers"
    (let [input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
          expected ["abc" "abc" "abac" "aaaa" "b"]]
      (is (= (group-answers input) expected))))
  (testing "count group answers"
    (let [input ["abc" "abc" "abac" "aaaa" "b"]
          expected [3 3 3 1 1]]
      (is (= (count-answers-per-group input) expected))))
  (testing "count all answers"
    (let [input "resources/input-day6"]
      (is (= (count-all-answers input) 6947))))
  )

(defn group-answers-task2 [input]
  (-> input
      (string/replace "\n\n" "|")
      (string/split #"\|")
      (#(map string/split-lines %))
      )
  )

(defn contains-first-element-in-every-group [answers answers-group]
  (every? #(string/includes? % (str (first answers))) answers-group))

(defn count-yes-answers-recursive [answers-group answers yes]
  (cond
    (empty? answers) yes
    (contains-first-element-in-every-group answers answers-group) (count-yes-answers-recursive answers-group (rest answers) (+ yes 1))
    :else (count-yes-answers-recursive answers-group (rest answers) yes)))

(defn count-yes-answers [answer-group]
  (count-yes-answers-recursive answer-group (first answer-group) 0))

(defn count-all-yes-answers-per-group [grouped-answers]
  (map count-yes-answers grouped-answers))

(defn count-all-answers-task2 [file]
  (->> file
       slurp
       group-answers-task2
       count-all-yes-answers-per-group
       (reduce +)))

(deftest day6-task2
  (testing "group answers"
    (let [input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
          expected [["abc"] ["a" "b" "c"] ["ab" "ac"] ["a" "a" "a" "a"] ["b"]]]
      (is (= (group-answers-task2 input) expected))))
  (testing "count yes answers from group"
    (is (= (count-yes-answers ["abc"]) 3))
    (is (= (count-yes-answers ["ab" "ac"]) 1))
    (is (= (count-yes-answers ["a" "b" "c"]) 0)))
  (testing "count group answers"
    (let [input [["abc"] ["a" "b" "c"] ["ab" "ac"] ["a" "a" "a" "a"] ["b"]]
          expected [3 0 1 1 1]]
      (is (= (count-all-yes-answers-per-group input) expected))))
  (testing "count all answers"
    (let [input "resources/input-day6"]
      (is (= (count-all-answers-task2 input) 3398)))))