(ns day-five)

(def sample "FBFBBFFRLR")

(defn ceil
  [x]
  (int (Math/ceil x)))

(reduce
 (fn [{mn :min mx :max} char]
   (if (= char \F)
    {:min (+ (ceil (/ (- mx mn) 2)) mn) :max mx}
    {:min mn :max ()}))
 sample)

(+ (int (/ (- 0 127) 2)) 127)

(+ (int (/ (- 127 0) 2)) 0)
