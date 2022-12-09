(ns advent-of-code.2022.6)

(defn distinct-consecutive-end
  [s distinct-count]
  (frequencies (take (dec distinct-count) s))

  (->> s
       (partition distinct-count 1)
       (keep-indexed (fn [i s] (when (= distinct-count (count (set s)))
                                 (+ distinct-count i))))
       first))

(defn p1
  [i]
  (distinct-consecutive-end i 4))

(defn p2
  [i]
  (distinct-consecutive-end i 14))
