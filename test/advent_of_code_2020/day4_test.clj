(ns advent-of-code-2020.day4-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(def required-fields [:byr :iyr :eyr :hgt :hcl :ecl :pid])

(comment
  (-> (slurp "resources/input-day4-example")
      (str/replace #"\n\n" "|")
      (str/replace #"\n" " ")
      (str/split #"\|")
      ))

(defn parse-input
  [fields-string]
  (-> fields-string
      (str/replace #"\n\n" "|")
      (str/replace #"\n" " ")
      (str/split #"\|"))
  )

(defn field-to-map
  [field]
  (->> field
       ((fn [fields] (str/split fields #" ")))
       (map #(str/split % #":"))
       (map (fn [field] [(keyword (first field)) (second field)]))
       (into {})
       ))

(defn fields-to-map
  [fields]
  (map field-to-map fields)

  )

(defn passport-valid?
  [passport]
  (every? passport required-fields)
  )

(defn count-valid-passwords
  [input]
  (->> input
       parse-input
       fields-to-map
       (filter passport-valid?)
       count
       ))

(deftest day4-task1
  (testing "parse password fields string"
    (let [fields "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
          expected ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
                    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
                    "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
                    "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"]
          actual (parse-input fields)]
      (is (= actual expected))))
  (testing "transform field into map"
    (let [field "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
          expected {:ecl "gry" :pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"}
          actual (field-to-map field)]
      (is (= actual expected))))
  (testing "transform fields into map"
    (let [fields ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"]
          expected [{:ecl "gry" :pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"}]
          actual (fields-to-map fields)]
      (is (= actual expected))))
  (testing "detects valid passport"
    (let [passport {:ecl "gry" :pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"}
          expected true
          actual (passport-valid? passport)]
      (is (= actual expected))))
  (testing "detects invalid passport"
    (let [passport {:pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"}
          expected false
          actual (passport-valid? passport)]
      (is (= actual expected))))
  (testing "counts valid passworts in example"
    (let [input (slurp "resources/input-day4-example")
          expected 2
          actual (count-valid-passwords input)]
      (is (= actual expected))))
  (testing "counts valid passworts in example"
    (let [input (slurp "resources/input-day4")
          expected 228
          actual (count-valid-passwords input)]
      (is (= actual expected))))
  )