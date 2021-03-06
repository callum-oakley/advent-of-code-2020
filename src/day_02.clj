(ns day-02
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [line]
  (let [[_ n m [c] pass] (re-find #"(\d+)-(\d+) (.): (.*)" line)]
    [(read-string n) (read-string m) c pass]))

(def data
  (map parse (str/split-lines (slurp "data/input_02.txt"))))

(defn valid-1? [[min max c pass]]
  (<= min (count (filter #{c} pass)) max))

(def part-1
  (count (filter valid-1? data)))

(defn valid-2? [[i j c pass]]
  (= (count (filter #{c} (map #(nth pass (dec %)) [i j]))) 1))

(def part-2
  (count (filter valid-2? data)))

(deftest test-valid-1?
  (is (valid-1? [1 3 \a "abcde"]))
  (is (not (valid-1? [1 3 \b "cdefg"])))
  (is (valid-1? [2 9 \c "ccccccccc"])))

(deftest test-valid-2?
  (is (valid-2? [1 3 \a "abcde"]))
  (is (not (valid-2? [1 3 \b "cdefg"])))
  (is (not (valid-2? [2 9 \c "ccccccccc"]))))

(deftest test-part-1
  (is (= part-1 393)))

(deftest test-part-2
  (is (= part-2 690)))
