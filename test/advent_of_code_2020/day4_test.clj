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

(defn valid-year?
  [field key min max]
  (let [value (key field)]
    (<= min (Integer/parseInt value) max)))

(defn valid-byr?
  [field]
  (valid-year? field :byr 1920 2002))

(defn valid-iyr?
  [field]
  (valid-year? field :iyr 2010 2020))

(defn valid-eyr?
  [field]
  (valid-year? field :eyr 2020 2030))

(defn valid-hgt-unit?
  [value unit min max]
  (-> value
      (str/replace unit "")
      (Integer/parseInt)
      (#(<= min % max))))

(defn valid-hgt?
  [field]
  (let [value (:hgt field)]
    (cond
      (str/ends-with? value "in") (valid-hgt-unit? value "in" 59 76)
      (str/ends-with? value "cm") (valid-hgt-unit? value "cm" 150 193)
      :else false
      )))

(defn valid-hcl?
  [field]
  (let [value (:hcl field)
        pattern #"(^[#])([0-9]|[a-f]){6}"]
    (not (nil? (re-matches pattern value)))))

(defn valid-ecl?
  [field]
  (let [value (:ecl field)
        valid-colors (set ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])]
    (true? (some #(= value %) valid-colors))))

(defn valid-pid?
  [field]
  (let [value (:pid field)
        pattern #"[0-9]{9}"]
    (not (nil? (re-matches pattern value)))))

(defn passport-rule-compliant?
  [passport]
  (and (passport-valid? passport)
       (valid-byr? passport)
       (valid-iyr? passport)
       (valid-eyr? passport)
       (valid-hgt? passport)
       (valid-hcl? passport)
       (valid-ecl? passport)
       (valid-pid? passport)
       (valid-byr? passport)
       ))

(defn count-rule-compliant-passports
  [input]
  (->> input
       parse-input
       fields-to-map
       (filter passport-rule-compliant?)
       count))

(deftest day4-task2
  (testing "validates byr field"
    (is (= (valid-byr? {:byr "2002"}) true))
    (is (= (valid-byr? {:byr "2003"}) false)))
  (testing "detects valid hgt field"
    (is (= (valid-hgt? {:hgt "60in"}) true))
    (is (= (valid-hgt? {:hgt "193cm"}) true)))
  (testing "validates hgt field"
    (is (= (valid-hgt? {:hgt "190in"}) false))
    (is (= (valid-hgt? {:hgt "190"}) false)))
  (testing "validates hcl field"
    (is (= (valid-hcl? {:hcl "#123abc"}) true))
    (is (= (valid-hcl? {:hcl "#123abz"}) false))
    (is (= (valid-hcl? {:hcl "#123abcc"}) false))
    (is (= (valid-hcl? {:hcl "123abc"}) false)))
  (testing "validates ecl field"
    (is (= (valid-ecl? {:ecl "brn"}) true))
    (is (= (valid-ecl? {:ecl "wat"}) false)))
  (testing "validates pid field"
    (is (= (valid-pid? {:pid "000000001"}) true))
    (is (= (valid-pid? {:pid "0123456789"}) false)))
  (testing "validates passport"
    (let [invalid-input {:eyr "1972" :cid "100" :hcl "#18171d" :ecl "amb" :hgt "170" :pid "186cm" :iyr "2018" :byr "1926"}
          valid-input {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2012" :eyr "2030" :byr "1980" :hcl "#623a2f"}]
      (is (= (passport-rule-compliant? invalid-input) false))
      (is (= (passport-rule-compliant? valid-input) true))))
  (testing "counts valid passworts in example"
    (let [input (slurp "resources/input-day4-example")
          expected 2
          actual (count-rule-compliant-passports input)]
      (is (= actual expected))))
  (testing "counts valid passworts for task 2"
    (let [input (slurp "resources/input-day4")
          expected 175
          actual (count-rule-compliant-passports input)]
      (is (= actual expected))))
  )
