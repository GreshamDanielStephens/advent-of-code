(ns advent-of-code.2022.1)

(defn p1
  [i]
  (->> i
       (re-seq #"(\d+)|\n\n")
       (reductions (fn [agg [_ d]] (if d (+ agg (bigdec d)) 0)) 0)
       (apply max)))

(defn p2
  [i]
  (->> i
       (re-seq #"(\d+)|\n\n")
       (reductions (fn [agg [_ d]] (if d (+ agg (bigdec d)) 0)) 0)
       (partition-by zero?)
       (map last)
       (sort)
       (take-last 3)
       (apply +)))
