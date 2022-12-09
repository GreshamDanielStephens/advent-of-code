(ns advent-of-code.2022.4
  (:require [clojure.string :as str]))

(defn base-solution
  [s pred]
  (->> s
       (str/split-lines)
       (map (comp #(map bigdec %) #(re-seq #"\d+" %)))
       (filter pred)
       count))

(defn sec-contained?
  [[a b c d]]
  (or (and (<= a c) (>= b d))
      (and (<= c a) (>= d b))))

(defn p1
  [s]
  (base-solution s sec-contained?))

(defn sec-overlap?
  [[a b c d :as sec]]
  (or (sec-contained? sec)
      (and (<= a c) (>= b c))
      (and (<= a d) (>= b d))))

(defn p2
  [s]
  (base-solution s sec-overlap?))