(ns advent-of-code.2022.7
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn associate-line-info
  [{:keys [path] :as i} line]
  (let [[_
         _ _ cd-root cd-up cd-dir
         _ file-size file-name] (re-matches #"(\$ cd ((/)|(\.\.)|(.+)))|((\d+) (.+))" line)]
    (cond
      cd-root (assoc i :path [])
      cd-up (assoc i :path (subvec path 0 (dec (count path))))
      cd-dir (assoc i :path (conj path cd-dir))
      file-size (assoc-in i (concat [:fs] path [file-name]) {:type :file
                                                             :size (bigdec file-size)})
      :else i)))

(defn calculate-fs
  [info-lines]
  (->> info-lines
       (reduce associate-line-info nil)
       (:fs)
       (walk/postwalk (fn [x]
                        (if (and (map? x) (not (:type x)))
                          {:type :dir
                           :size (->> x
                                      (map (comp :size val))
                                      (reduce +' 0))
                           :children x}
                          x)))))

(defn traverse-fs
  [f agg fs]
  (case (:type fs)
    :dir (f (reduce (fn [agg' [_ child]]
                      (traverse-fs f agg' child))
                    agg
                    (:children fs))
            fs)
    :file (f agg fs)))

(defn p1
  [s]
  (->> s
       (str/split-lines)
       calculate-fs
       (traverse-fs (fn [agg {:keys [type size]}]
                      (if (and (= :dir type)
                               (<= size 100000))
                        (+' agg size)
                        agg)) 0)))

(defn p2
  [s]
  (let [fs (->> s
                (str/split-lines)
                calculate-fs)
        total-used (:size fs)
        saving-needed (- total-used 40000000)]
    (traverse-fs (fn [agg {:keys [type size]}]
                   (if (and (= :dir type)
                            (>= size saving-needed)
                            (< size agg))
                     size
                     agg))
                 total-used
                 fs)))
