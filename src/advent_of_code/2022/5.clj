(ns advent-of-code.2022.5
  (:require [clojure.string :as str]))

(defn parse-stack
  [stacks]
  (let [[nos & more] (->> stacks
                          (str/split-lines)
                          reverse
                          (map #(take-nth 4 (rest %))))]
    (zipmap nos (->> more
                     (apply map vector)
                     (map (fn [c] (take-while #(not= \space %) c)))))))

(defn parse-commands
  [commands]
  (->> commands
       (str/split-lines)
       (map #(re-seq #"\d+" %))))

(defn apply-command
  [can-take-stack? stack [no [from] [to]]]
  (-> stack
      (update from #(drop-last (bigdec no) %))
      ;; change to reverse for part 1
      (update to concat ((if can-take-stack? identity reverse) (take-last (bigdec no) (get stack from))))))

(defn base-solution
  [i can-take-stack?]
  (let [[stacks commands] (str/split i #"\n\n")
        s (parse-stack stacks)
        cs (parse-commands commands)]
    (->> cs
         (reduce (partial apply-command can-take-stack?) s)
         (sort-by key)
         (map (comp last val))
         (apply str))))

(defn p1
  [i]
  (base-solution i false))

(defn p2
  [i]
  (base-solution i true))
