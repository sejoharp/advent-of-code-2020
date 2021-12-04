(ns advent-of-code-2020.day7-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]))

(def rules-sample "light red bags contain 1 bright white bag, 2 muted yellow bags.\n
dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n
bright white bags contain 1 shiny gold bag.\n
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n
dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n
faded blue bags contain no other bags.\n
dotted black bags contain no other bags.")

(defn color-body [input]
  (->> input
       rest
       (string/join "-")
       keyword))

(defn colors-in-bag [input]
  (let [parts (-> input
                  string/trim
                  (string/replace #"bags" "")
                  (string/replace #"bag" "")
                  (string/split #" "))
        ]
    (color-body parts)
    ))

(defn parse-content [body]
  (-> body
      (string/split #",")
      (#(map colors-in-bag %))
      set)
  )

(defn parse-rule [rule]
  (let [key (-> rule
                (string/split #" bags contain ")
                first
                (string/replace #" " "-")
                keyword)
        body (second (string/split rule #" bags contain "))]
    {key (if (= body "no other bags")
           #{}
           (parse-content body))}))

(defn split-rules [rule-strings]
  (-> rule-strings
      (string/replace #"\n" "")
      (string/split #"\."))
  )

(defn parse-rules [rule-strings]
  (->> rule-strings
       split-rules
       (map parse-rule)
       (into {})))

(defn to-set [s]
  (if (set? s) s #{s}))
(defn set-union [s1 s2]
  (clojure.set/union (to-set s1) (to-set s2)))
(defn mergeMatches [propertyMapList]
  (reduce #(merge-with set-union %1 %2) {} propertyMapList))


(defn content-to-bag [rules]
  (let [rule-list (for [[bag contents] rules]
                    (for [content (seq contents)]
                      {content bag}))]
    (-> rule-list
        flatten
        mergeMatches
        )
    )
  )

(defn find-bag-color [color rules]
  (filter (fn [[color-key content]] (contains? content color)) rules)
  )

(deftest day7-task1
  (testing "parse-rule-body"
    (let [input "1 bright white bag"
          expected :bright-white]
      (is (= (colors-in-bag input) expected))))
  (testing "parse rule body"
    (let [input "light red bags contain 1 bright white bag, 2 muted yellow bags"
          expected {:light-red #{:bright-white
                                 :muted-yellow}}]
      (is (= (parse-rule input) expected))))
  (testing "parse empty bag rule"
    (let [input "faded blue bags contain no other bags"
          expected {:faded-blue #{}}]
      (is (= (parse-rule input) expected))))
  (testing "parse example rules"
    (let [expected {:bright-white #{:shiny-gold}
                    :dark-olive   #{:dotted-black
                                    :faded-blue}
                    :dark-orange  #{:bright-white
                                    :muted-yellow}
                    :dotted-black #{}
                    :faded-blue   #{}
                    :light-red    #{:bright-white
                                    :muted-yellow}
                    :muted-yellow #{:faded-blue
                                    :shiny-gold}
                    :shiny-gold   #{:dark-olive
                                    :vibrant-plum}
                    :vibrant-plum #{:dotted-black
                                    :faded-blue}}]
      (is (= (parse-rules rules-sample) expected))))
  (testing "reverse map to content -> bag"
    (let [input {:bright-white #{:shiny-gold}
                 :dark-olive   #{:dotted-black
                                 :faded-blue}
                 :dark-orange  #{:bright-white
                                 :muted-yellow}
                 :dotted-black #{}
                 :faded-blue   #{}
                 :light-red    #{:bright-white
                                 :muted-yellow}
                 :muted-yellow #{:faded-blue
                                 :shiny-gold}
                 :shiny-gold   #{:dark-olive
                                 :vibrant-plum}
                 :vibrant-plum #{:dotted-black
                                 :faded-blue}}
          expected {:faded-blue   #{:vibrant-plum :muted-yellow :dark-olive},
                    :dotted-black #{:vibrant-plum :dark-olive},
                    :bright-white #{:light-red :dark-orange},
                    :muted-yellow #{:light-red :dark-orange},
                    :vibrant-plum :shiny-gold,
                    :dark-olive   :shiny-gold,
                    :shiny-gold   #{:bright-white :muted-yellow}}]
      (is (= (content-to-bag input) expected))))
  ;(testing "find gold bags"
  ;  (let [input (parse-rules-without-amounts rules-sample)
  ;        expected {:bright-white #{:shiny-gold}
  ;                  :muted-yellow #{:faded-blue
  ;                                  :shiny-gold}}]
  ;    (is (= (find-bag-color :shiny-gold input) expected))))
  )

