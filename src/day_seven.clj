(ns day-seven
 (:require [clojure.string :as s]
           [clojure.pprint :refer [pprint]]
           [clojure.set :refer [union]]))

(def input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def sample (first (s/split-lines input)))

(def clean-str
  (comp
    (map #(s/replace % "bags" "bag"))
    (map #(s/replace % "." ""))))

(defn try-parse-int
  [x]
  (try
    (Integer/parseInt x)
    (catch Exception _ false)))

(defn str->bag-amount-map
  [s]
  (let [num (-> s
                (s/split #" ")
                first
                try-parse-int)
        name (as-> s x
                 (s/split x #" ")
                 (rest x)
                 (s/join " " x))]
    {name num}))

(defn arrs->bag-map
  [s]
  (apply
    merge
    (map
      (fn [[k vs]]
        {k (->> vs
                (map str->bag-amount-map)
                (filter #(nil? (get % "other bag")))
                (into []))}) s)))

(defn can-hold
  [xs bag]
  (->> xs
       (map (fn [[container bags]]
              (if (some #{bag} (flatten (map keys bags)))
                container)))
       (filter some?)
       set))

(can-hold x "dotted black bag")

(defn puzzle-one
  [input]
  (let [bag-map (->> input
                        s/split-lines
                        (sequence clean-str)
                        (map #(s/split % #" contain "))
                        (map (fn [[k vs]] [k (vec (s/split vs #", "))]))
                        arrs->bag-map)]
    (loop [bags (can-hold bag-map "shiny gold bag")]
      (let [new-bags (apply union (cons bags (map #(can-hold bag-map %) bags)))]
        (if (= bags new-bags)
          bags
          (recur new-bags))))))

(comment
  (puzzle-one input)
  (count (puzzle-one (slurp "src/day_seven.txt")))
  ; Find which containers can hold a specific bag
  (map (fn [[container bags]]
         (if (some #{"dotted black bag"} (flatten (map keys bags)))
           container))
       x)
  (puzzle-one input)
  (def x (->> input
           s/split-lines
           (sequence clean-str)
           (map #(s/split % #" contain "))
           (map (fn [[k vs]] [k (vec (s/split vs #", "))]))
           arrs->bag-map)))
(comment)
