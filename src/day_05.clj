(ns day-05
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is are]]))

(def data
  (str/split-lines (slurp "data/input_05.txt")))

(defn decode [s]
  (read-string (apply str "2r" (map {\F \0 \B \1 \L \0 \R \1} s))))

(defn part-1 [passes]
  (apply max (map decode passes)))

(defn part-2 [passes]
  (let [occupied (set (map decode passes))
        seats (set (range (apply min occupied) (inc (apply max occupied))))]
    (first (set/difference seats occupied))))

(deftest test-decode
  (are [pass seat] (= (decode pass) seat)
    "FBFBBFFRLR" 357
    "BFFFBBFRRR" 567
    "FFFBBBFRRR" 119
    "BBFFBBFRLL" 820))

(deftest test-part-1
  (is (= (part-1 data) 901)))

(deftest test-part-2
  (is (= (part-2 data) 661)))
