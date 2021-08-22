(ns day-five
  (:require [clojure.string :refer [split-lines]]
            [clojure.pprint :refer [pprint]]))

(def sample "FBFBBFFRLR")
(def sample2 "BFFFBBFRRR")
(def sample3 "FFFBBBFRRR")

(defn ceil
  [x]
  (int (Math/ceil x)))

(defn plane-row
  [code]
  (reduce
    (fn [{mn :min mx :max} char]
      (if (= char \F)
        {:min mn :max (+ (int (/ (- mx mn) 2)) mn)}
        {:min (+ (ceil (/ (- mx mn) 2)) mn) :max mx}))
    {:min 0 :max 127}
    (take 7 code)))

(defn plane-col
  [code]
  (reduce
    (fn [{mn :min mx :max} char]
      (if (= char \L)
        {:min mn :max (+ (int (/ (- mx mn) 2)) mn)}
        {:min (+ (ceil (/ (- mx mn) 2)) mn) :max mx}))
    {:min 0 :max 7}
    (reverse (take 3 (reverse code)))))

(plane-col "BBFFBBFRLL")

(defn plane-id
  [row col]
  (+ (* row 8) col))

(defn plane-info
  [code]
  (let [row (:min (plane-row code))
        col (:min (plane-col code))]
    {:row row :col col :id (plane-id row col)}))

(defn puzzle-one
  [input]
  (apply max (plane-ids input)))

(defn plane-ids
  [input]
  (let [f (comp
            (map plane-info)
            (map :id))]
    (into [] f input)))

(defn find-missing
  [ids]
  (->> ids
       sort
       (partition 2)
       (filter (fn [[mn mx]] (= 2 (- mx mn))))
       first
       first
       inc))



(comment
  (def input (-> "src/day_five.txt" slurp split-lines))
  (apply max (map plane-info input))
  (partition 2 (sort (plane-ids input)))
  (partition 2 (plane-ids input))
  (find-missing (plane-ids input))
  (puzzle-one input)
  (plane-info sample2)
  (plane-info sample3)
  (plane-info "BBFFBBFRLL")
  (plane-col sample)
  (plane-col sample2)
  (plane-col sample3)
  (plane-col sample3)
  (plane-col "BBFFBBFRLL")
  (plane-row sample)
  (plane-row sample2)
  (plane-row sample3))
