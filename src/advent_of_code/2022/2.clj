(ns advent-of-code.2022.2
  (:require [clojure.string :as str]))

(defn p1
  [i]
  (let [score (fn [[a x]]
               (cond
                 (= a x) (+ 4 x)
                 (= (mod (inc a) 3) x) (+ 7 x)
                 :else (inc x)))]
   (->> i
        (str/split-lines)
        (map (fn [[a _ x]] [(- (int a) (int \A)) (- (int x) (int \X))]))
        (map score)
        (reduce + 0))))


(defn p2
  [i]
  (let [score (fn [[a x]]
               (case x
                 0 (+ 1 (mod (dec a) 3))
                 1 (+ 4 a)
                 2 (+ 7 (mod (inc a) 3))))]
   (->> i
        (str/split-lines)
        (map (fn [[a _ x]] [(- (int a) (int \A)) (- (int x) (int \X))]))
        (map score)
        (reduce + 0))))
