(ns advent-of-code.2022.9
  (:require [clojure.string :as str]))

(defn parse-commands
  [i]
  (->> i
       (str/split-lines)
       (mapcat (fn [[c _ & more]]
                 (repeat (bigdec (apply str more))
                         (case c \R [1 0] \L [-1 0] \U [0 1] \D [0 -1]))))))

(defn max-dist
  [[x1 y1] [x2 y2]]
  (max (- x1 x2) (- x2 x1) (- y1 y2) (- y2 y1)))

(defn add-vecs
  [v1 v2]
  (mapv +' v1 v2))

(defn solve-diagonal
  [[x1 y1 :as h] [x2 y2 :as t] shift]
  (if (and (>= 1 (max (- x1 x2) (- x2 x1)))
           (>= 1 (max (- y1 y2) (- y2 y1))))
    (mapv - h shift)
    t))

(defn apply-command
  [{:keys [head tail]} shift]
  (let [h' (add-vecs head shift)
        t' (if (> (max-dist h' tail) 1)
             (solve-diagonal h' (add-vecs tail shift) shift) tail)]
    {:head h' :tail t'}))

(defn p1
  [i]
  (->> i
       parse-commands
       (reductions apply-command
                   {:head [0 0] :tail [0 0]})
       (map :tail)
       (distinct)
       (count)))

;;;;;;;;;;;

(defn cascade-shift
  [parent-pos [f & following]]
  (let [required-shift (->> f
                            (mapv - parent-pos)
                            (mapv min [1 1])
                            (mapv max [-1 -1]))
        f' (if (> (max-dist parent-pos f) 1)
             (mapv + f required-shift)
             f)]
    (lazy-cat [f']
              (when (seq following)
                (cascade-shift f' following)))))

(defn apply-general-command
  [[h & parts] shift]
  (let [h' (mapv + h shift)]
    (concat [h'] (cascade-shift h' parts))))

(defn visualise
  [size parts]
  (dotimes [y size]
    (dotimes [x size]
      (let [x' (- x (int (/ size 2)))
            y' (- (int (/ size 2)) y)
            i (.indexOf parts [x' y'])]
        (if (= -1 i)
          (print ".")
          (print i))))
    (println))
  (println)
  parts)

(defn base-solution
  [i knots visualise?]
  (->> i
       parse-commands
       (reductions apply-general-command
                   (repeat knots [0 0]))
       (map (if visualise? (partial visualise 50) identity))
       (map last)
       (distinct)
       (count)))

(defn p1-new
  [i]
  (base-solution i 2 false))

(defn p2
  [i]
  (base-solution i 10 true))