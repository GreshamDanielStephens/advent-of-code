(ns advent-of-code.2022.3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def priority
  (zipmap (seq "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (map inc (range))))

(defn shared-items
  [s]
  (->> (subs s (/ (count s) 2))
       (filter (set (subs s 0 (/ (count s) 2))))
       distinct))

(defn p1
  [s]
  (->> s
       (str/split-lines)
       (mapcat shared-items)
       (map priority)
       (reduce + 0)))

(defn p2
  [s]
  (->> s
       (str/split-lines)
       (partition-all 3)
       (map #(->> % (map set) (apply set/intersection) first))
       (map priority)
       (reduce + 0)))