(ns advent-of-code.2022.8
  (:require [clojure.string :as str]))

(defn map-grid-indexed
  [f grid]
  (vec (map-indexed (fn [y row]
                      (vec (map-indexed
                             (fn [x e] (f [y x] e))
                             row)))
                    grid)))

(defn column
  [[_ x] grid]
  (mapv #(get % x) grid))

(defn row
  [[y _] grid]
  (get grid y))

(defn map-grid-with-context
  [f grid]
  (map-grid-indexed (fn [[y x :as coord] v]
                      (let [c (column coord grid)
                            r (row coord grid)]
                        (f v [(reverse (take y c)) (drop (inc y) c)
                              (reverse (take x r)) (drop (inc x) r)])))
                    grid))

(defn parse-forest
  [i]
  (->> i
       (str/split-lines)
       (map-grid-indexed (fn [_ v] (bigdec (str v))))))

(defn base-solution
  [i element-fn reduce-fn]
  (->> i
       (parse-forest)
       (map-grid-with-context element-fn)
       flatten
       reduce-fn))

(defn visible-view
  [v view]
  (every? #(< % v) view))

(defn visible?'
  [v views]
  (->> views
       (map (partial visible-view v))
       (some boolean)))

(defn p1'
  [i]
  (base-solution i
                 visible?'
                 #(->> (filter true?) count)))

(defn scenic-score-view
  [v view]
  (count (take (inc (count (take-while #(< % v) view)))
               view)))

(defn scenic-score
  [v views]
  (->> views
       (map (partial scenic-score-view v))
       (reduce * 1)))

(defn p2
  [i]
  (base-solution i
                 scenic-score
                 #(apply max %)))
