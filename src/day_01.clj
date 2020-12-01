(ns day-01
  (:require
    [clojure.math.combinatorics :as comb]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def expense-report
  (map read-string (str/split-lines (slurp "data/input_01.txt"))))

(defn process-report [n report]
  (->> (comb/combinations report n)
    (filter #(= (apply + %) 2020))
    first
    (apply *)))

(defn part-1 [report]
  (process-report 2 report))

(defn part-2 [report]
  (process-report 3 report))

(deftest test-part-1
  (is (= (part-1 [1721 979 366 299 675 1456]) 514579))
  (is (= (part-1 expense-report) 100419)))

(deftest test-part-2
  (is (= (part-2 [1721 979 366 299 675 1456]) 241861950))
  (is (= (part-2 expense-report) 265253940)))
