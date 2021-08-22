(ns day-six
  (:require [clojure.string :as s]))

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

(defn count-group-answers [input]
  (let [f (comp
           (map set)
           (map #(disj % \newline))
           (map count))]
   (reduce + (into [] f (s/split input #"\n\n")))))

(comment
  (def input (slurp "src/day_six.txt"))
  (count-group-answers input))
