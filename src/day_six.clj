(ns day-six
 (:require [clojure.string :as s]
           [clojure.pprint :refer [pprint]]
           [clojure.set :refer [union]]))

(def input "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn- yeses-per-group [input]
  (let [f (comp
           (map set)
           (map #(disj % \newline))
           (map count))]
    (into [] f (s/split input #"\n\n"))))

(defn- yeses-per-person [input]
  (let [f (comp)]

    (into [] f (s/split input #"\n\n"))))

(defn count-group-answers [input]
  (reduce + (yeses-per-group input)))

(defn count-all-yeses [input]
  (count (filter #(= % 26) (yeses-per-group input))))

(defn- strs-to-set
  [strs]
  (set (s/join strs)))

(defn- all-questions
  [xs]
  (reduce
   (fn [acc as] (union acc (strs-to-set as))) #{} xs))

(defn count-by
  [pred coll]
  (count (filter pred coll)))

(defn f
  [set-questions group]
  (->> (for [q set-questions]
        [q (every? #(s/includes? % (str q)) group)])
       (map (fn [[_ b]] b))
       (count-by true?)))

(comment
  (strs-to-set ["ab" "ac"])
  (def input (slurp "src/day_six.txt"))
  (def answers (->> (s/split input #"\n\n")
                    (map #(s/split % #"\n"))))

  (reduce (fn [acc as] (union acc (strs-to-set as))) #{} answers)


  ; Answer to part 2
  (let [input (slurp "src/day_six.txt")
        answers (->> (s/split input #"\n\n")
                    (map #(s/split % #"\n")))
        qs (all-questions answers)]
    (->> (for [a answers]
           (f qs a))
         (reduce +)))

  (map (fn [x] (every? #(= (count %) 3) x)) answers)

  (count-group-answers input)
  (count-all-yeses input)
  (->> (s/split input #"\n\n")
       (map (fn [x] (partition-by #(= \newline %) x)))
       (map (fn [xss] (filter (fn [xs] #(= (first %) \newline) xs) xss))))
  (comment))
