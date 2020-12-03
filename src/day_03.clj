(ns day-03
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def data
  (str/split-lines (slurp "data/input_03.txt")))

(defn path [dy dx height width]
  (for [i (range)
        :let [y (* dy i) x (* dx i)]
        :while (< y height)]
    [y (mod x width)]))

(defn count-trees [grid dy dx]
  (->> (path dy dx (count grid) (count (first grid)))
    (filter #(= (get-in grid %) \#))
    count))

(defn part-1 [grid]
  (count-trees grid 1 3))

(defn part-2 [grid]
  (apply * (map #(apply count-trees grid %)
             [[1 1] [1 3] [1 5] [1 7] [2 1]])))

(def sample
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(deftest test-part-1
  (is (= (part-1 sample) 7))
  (is (= (part-1 data) 200)))

(deftest test-part-2
  (is (= (part-2 sample) 336))
  (is (= (part-2 data) 3737923200)))
