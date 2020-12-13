(ns day-13
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def data
  (str/split-lines (slurp "data/input_13.txt")))

(defn parse [[earliest-departure busses]]
  [(read-string earliest-departure)
   (->> (str/split busses #",")
     (map-indexed (fn [i bus] {:index i :id (read-string bus)}))
     (filter (comp int? :id)))])

(defn part-1 [[earliest-departure busses]]
  (let [wait #(mod (- earliest-departure) %)
        bus (apply min-key wait (map :id busses))]
    (* bus (wait bus))))

;; It's the Chinese remainder theorem! Algorithm described here:
;; https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
(defn part-2 [[_ [{:keys [index id]} & rem]]]
  (loop [t (- id index) step id rem rem]
    (if-let [{:keys [index id]} (first rem)]
      (if (zero? (mod (+ t index) id))
        (recur t (* step id) (rest rem))
        (recur (+ t step) step rem))
      t)))

(def sample
  ["939" "7,13,x,x,59,x,31,19"])

(deftest test-part-1
  (is (= (part-1 (parse sample)) 295))
  (is (= (part-1 (parse data)) 5257)))

(deftest test-part-2
  (is (= (part-2 (parse sample)) 1068781))
  (is (= (part-2 (parse data)) 538703333547789)))
