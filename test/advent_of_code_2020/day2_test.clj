(ns advent-of-code-2020.day2-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day2 :refer :all]
            [clojure.string :as str]))

;https://clojuredocs.org/clojure.core/partition-by
(comment
  (group-by identity "ABBA")
  ;=> {\A [\A \A], \B [\B \B]}
  )

(defn parse-requirements
  [input]
  (let [parts (str/split input #" ")
        boundaries (str/split (first parts) #"-")
        min (Integer/parseInt (first boundaries))
        max (Integer/parseInt (second boundaries))]
    {:min min :max max :character (second parts)})
  )

(defn parse-input-row
  [row]
  (let [parts (str/split row #": ")]
    {:requirements (parse-requirements (first parts)) :password (second parts)})
  )

(deftest day2-task1
  (testing "reads requirements"
    (let [input "1-3 b"
          expected {:min 1 :max 3 :character "b"}
          actual (parse-requirements input)]
      (is (= expected actual))))
  (testing "reads input"
    (let [input "1-3 b: cdefg"
          expected {:requirements {:min 1 :max 3 :character "b"} :password "cdefg"}
          actual (parse-input-row input)]
      (is (= expected actual))))
  )