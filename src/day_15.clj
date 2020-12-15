(ns day-15
  (:require
    [clojure.test :refer [deftest is]]))

(def data
  [0 14 6 20 1 4])

;; Van Eck's sequence https://youtu.be/etMJxB-igrc
(defn turn [[prev i mem]]
  (let [seen (get mem prev)
        age (if seen (- i seen) 0)]
    [age (inc i) (assoc mem prev i)]))

(defn game [seed]
  (->> (iterate turn
         [(last seed)
          (dec (count seed))
          (into {} (map-indexed (fn [i n] [n i]) seed))])
    rest
    (map first)
    (concat seed)))

(defn part-1 [seed]
  (nth (game seed) 2019))

(defn part-2 [seed]
  (nth (game seed) 29999999))

(def sample
  [0 3 6])

(deftest test-part-1
  (is (= (part-1 sample) 436))
  (is (= (part-1 data) 257)))

(deftest test-part-2
  (is (= (part-2 sample) 175594))
  (is (= (part-2 data) 8546398)))
