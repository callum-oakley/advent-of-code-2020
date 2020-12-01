(ns day-01
  (:require
    [clojure.string :as s]
    [clojure.test :refer [deftest is]]))

(defn subseq-of-len
  "Produces all the subsequences of coll of length n."
  [n coll]
  (cond
    (zero? n) ['()]
    (empty? coll) []
    :else (concat
            (map #(conj % (first coll)) (subseq-of-len (dec n) (next coll)))
            (subseq-of-len n (next coll)))))

(deftest test-subseq-of-len
  (is (= (subseq-of-len 2 [0 1 2]) [[0 1] [0 2] [1 2]])))

(defn process-report [n report]
  (->> (subseq-of-len n report)
    (filter #(= (apply + %) 2020))
    first
    (apply *)))

(defn part-1 [report]
  (process-report 2 report))

(def expense-report
  (map read-string (s/split-lines (slurp "data/input_01.txt"))))

(deftest test-part-1
  (is (= (part-1 [1721 979 366 299 675 1456]) 514579))
  (is (= (part-1 expense-report) 100419)))

(defn part-2 [report]
  (process-report 3 report))

(deftest test-part-2
  (is (= (part-2 [1721 979 366 299 675 1456]) 241861950))
  (is (= (part-2 expense-report) 265253940)))
