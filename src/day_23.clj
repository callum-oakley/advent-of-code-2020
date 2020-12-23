(ns day-23
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(defn initial-state [cups]
  {:cups (transient
           (into {(last cups) (first cups)} (map vec (partition 2 1 cups))))
   :current (first cups)
   :maxcup (apply max cups)})

(defn move [{:keys [cups current maxcup] :as state}]
  ;; ... current a b c d ... destination e ...
  (let [a (cups current) b (cups a) c (cups b) d (cups c)
        destination (->> (iterate #(if (= % 1) maxcup (dec %)) current)
                      rest
                      (some #(when (not (#{a b c} %)) %)))
        e (cups destination)]
    (assoc state :cups (assoc! cups current d destination a c e) :current d)))

(defn game [n cups]
  (first (drop n (iterate move (initial-state cups)))))

(defn part-1 [cups]
  (let [cups* (:cups (game 100 cups))]
    (str/join (take (dec (count cups)) (rest (iterate cups* 1))))))

(defn part-2 [cups]
  (let [cups* (:cups (game 10000000 (concat cups (range 10 1000001))))
        a (cups* 1) b (cups* a)]
    (* a b)))

(def sample
  [3 8 9 1 2 5 4 6 7])

(def data
  [5 6 2 8 9 3 1 4 7])

(deftest test-part-1
  (is (= (part-1 sample) "67384529"))
  (is (= (part-1 data) "38925764")))

(deftest test-part-2
  (is (= (part-2 sample) 149245887792))
  (is (= (part-2 data) 131152940564)))
