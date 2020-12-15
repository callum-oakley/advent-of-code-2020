(ns day-15
  (:require
    [clojure.test :refer [deftest is]]))

;; Van Eck's sequence https://youtu.be/etMJxB-igrc
(defn turn [[prev i mem]]
  (let [seen (get mem prev)
        age (if seen (- i seen) 0)]
    [age (inc i) (assoc! mem prev i)]))

(defn game [seed n]
  (nth
    (->> (iterate turn
           [(last seed)
            (dec (count seed))
            (transient
              (apply assoc (vec (repeat n nil))
                (flatten (map-indexed (fn [i m] [m i]) (drop-last seed)))))])
      rest
      (map first)
      (concat seed))
    (dec n)))

(def sample
  [0 3 6])

(def data
  [0 14 6 20 1 4])

(deftest test-part-1
  (is (= (game sample 2020) 436))
  (is (= (game data 2020) 257)))

(deftest test-part-2
  (is (= (game sample 30000000) 175594))
  (is (= (game data 30000000) 8546398)))
