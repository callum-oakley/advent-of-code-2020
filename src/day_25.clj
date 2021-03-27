(ns day-25
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (map read-string (str/split-lines (slurp "data/input_25.txt"))))

(defn steps [subject]
  (iterate #(mod (* subject %) 20201227) 1))

(defn loops [key]
  (some (fn [[n val]] (when (= val key) n)) (map-indexed vector (steps 7))))

(defn part-1 [[card door]]
  (first (drop (loops door) (steps card))))

(def sample
  [5764801 17807724])

(deftest test-part-1
  (is (= (part-1 sample) 14897079))
  (is (= (part-1 data) 7032853)))
