(ns day-eight
  (:require [clojure.string :as s]))

(defn try-parse-int
  [x]
  (try
    (Integer/parseInt x)
    (catch Exception _ nil)))

(def sample "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn perform-instruction
  [[op x] current-line-num acc]
  (let [value (Integer/parseInt x)]
    (case op
      "nop" [(inc current-line-num) acc]
      "acc" [(inc current-line-num) (+ acc value)]
      "jmp" [(+ current-line-num value) acc])))

(defn puzzle-one
  [instructions current-line-num acc line-nums-run]
  (if (or (some #{current-line-num} line-nums-run) (nil? (get instructions current-line-num)))
    acc
    (let [[next-line-num next-acc] (perform-instruction (get instructions current-line-num) current-line-num acc)]
      (recur instructions next-line-num next-acc (conj line-nums-run current-line-num)))))

(defn puzzle-with-set
  [instructions current-line-num acc line-nums-run]
  (println (sort (vec line-nums-run)))
  (if (or (some #{current-line-num} line-nums-run) (nil? (get instructions current-line-num)))
    [acc (nil? (get instructions current-line-num))]
    (let [[next-line-num next-acc] (perform-instruction (get instructions current-line-num) current-line-num acc)]
      (recur instructions next-line-num next-acc (conj line-nums-run current-line-num)))))

(puzzle-one
 (->> (s/split-lines sample)
      (map #(s/split % #" "))
      (into []))
 0
 0
 #{})

(puzzle-one
 (->> (s/split-lines (slurp "src/day_eight.txt"))
      (map #(s/split % #" "))
      (into []))
 0
 0
 #{})

(let [instructions (->> (s/split-lines (slurp "src/day_eight.txt"))
                        (map #(s/split % #" "))
                        (into []))
      cnt (count instructions)
      vals-with-set (map-indexed (fn [i [op val]]
                                   (case op
                                     "nop" (puzzle-with-set (assoc instructions i ["jmp" val]) 0 0 #{})
                                     "jmp" (puzzle-with-set (assoc instructions i ["nop" val]) 0 0 #{})
                                     nil)) instructions)]
  (def i instructions)
  (def c cnt)
  (def vs vals-with-set)
  (first (filter #(second %) vals-with-set)))

(filter #(= (count (second %)) c) vs)

(map #(count (second %)) vs)

(assoc [["nop" 1] ["jmp" 2] 3] 1 "egg")
