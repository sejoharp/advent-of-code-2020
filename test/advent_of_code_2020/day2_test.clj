(ns advent-of-code-2020.day2-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day2 :refer :all]
            [clojure.string :as str]))

;https://clojuredocs.org/clojure.core/partition-by
(comment
  (group-by identity "ABBA")
  ;=> {\A [\A \A], \B [\B \B]}
  )

(defn count-char
  [char-map]
  (let [key (first char-map)
        values (second char-map)]
    [(str key) (count values)]))

(defn group-by-char
  [input]
  (->> input
       (group-by identity)
       (map count-char)
       (into {})
       )
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
  (let [parts (str/split row #": ")
        password (second parts)]
    {:requirements (parse-requirements (first parts)) :password password :tokens (group-by-char password)})
  )

(defn parse-input
  [file]
  (->> file
       slurp
       str/split-lines
       (map #(parse-input-row %))
       )
  )
(defn password-valid?
  [input]
  (let [tokens (:tokens input)
        required-char (get-in input [:requirements :character])
        required-char-count (get tokens required-char 0)
        min (get-in input [:requirements :min])
        max (get-in input [:requirements :max])]
    (<= min required-char-count max)))

(defn char-present
  [password char position]
  (let [array-position (- position 1)]
    (if (= (str (get password array-position)) char)
      1
      0)))

(defn password-matches-positions?
  [input]
  (let [required-char (get-in input [:requirements :character])
        password (:password input)
        min (get-in input [:requirements :min])
        max (get-in input [:requirements :max])
        min-present (char-present password required-char min)
        max-present (char-present password required-char max)
        matched-rules-count (+ min-present max-present)]
    (= matched-rules-count 1)))

(defn valid-password-count
  [passwords]
  (->> passwords
       (filter password-valid?)
       count)
  )

(defn valid-password-matches-positions-count
  [passwords]
  (->> passwords
       (filter password-matches-positions?)
       count)
  )

(deftest day2-task1
  (testing "reads requirements"
    (let [input "1-3 b"
          expected {:min 1 :max 3 :character "b"}
          actual (parse-requirements input)]
      (is (= expected actual))))
  (testing "reads input line"
    (let [input "1-3 b: abbac"
          expected {:requirements {:min 1 :max 3 :character "b"} :password "abbac" :tokens {"a" 2 "b" 2 "c" 1}}
          actual (parse-input-row input)]
      (is (= actual expected))))
  (testing "group passwords by charactor"
    (let [input "abbac"
          expected {"a" 2 "b" 2 "c" 1}
          actual (group-by-char input)]
      (is (= expected actual))))
  (testing "reads inputs from file"
    (let [input "resources/input-day2"
          expected-size 1000
          expected-row {:requirements {:min 3 :max 4 :character "l"} :password "vdcv" :tokens {"v" 2 "d" 1 "c" 1}}
          actual (parse-input input)]
      (is (= (count actual) expected-size))
      (is (= (first actual) expected-row))))
  (testing "password is valid"
    (let [input {:requirements {:min 1 :max 2 :character "v"} :password "vdcv" :tokens {"v" 2 "d" 1 "c" 1}}
          expected true
          actual (password-valid? input)]
      (is (= actual expected))))
  (testing "counts valid passwords"
    (let [passwords (parse-input "resources/input-day2")
          expected 447
          actual (valid-password-count passwords)]
      (is (= actual expected))))
  )

(deftest day2-task2
  (testing "password is valid"
    (let [input {:requirements {:min 1 :max 3 :character "a"} :password "abcde" :tokens {}}
          expected true
          actual (password-matches-positions? input)]
      (is (= actual expected))))
  (testing "password is valid"
    (let [input {:requirements {:min 1 :max 3 :character "b"} :password "cdefg" :tokens {}}
          expected false
          actual (password-matches-positions? input)]
      (is (= actual expected))))
  (testing "password is valid"
    (let [input {:requirements {:min 2 :max 9 :character "c"} :password "ccccccccc" :tokens {}}
          expected false
          actual (password-matches-positions? input)]
      (is (= actual expected))))
  (testing "counts valid passwords"
    (let [passwords (parse-input "resources/input-day2")
          expected 249
          actual (valid-password-matches-positions-count passwords)]
      (is (= actual expected))))
  )