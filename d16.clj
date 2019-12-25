(ns d16
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1 "12345678")
(def ZERO (int \0))

(defn parse [s] (->> s str/trim seq (map #(- (int %) ZERO))))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(def seq0 (interleave (repeat 0) (interleave (repeat 1) (repeat -1))))
(defn seq-n [n] (drop 1 (apply interleave (repeat (inc n) seq0))))

(defn apply-1 [arg n]
  (->> arg (map * (seq-n n)) (apply +) str last int (- ZERO) -))

(defn run-1 [arg] (map #(apply-1 arg %) (range (count arg))))
(defn run [arg n] (subs (apply str (nth (iterate run-1 (parse arg)) n)) 0 8))

(defn one [] (run (slurp "16.in") 100))

(let [n 39]
  (dotimes [i n]
    (->> i seq-n (take n) (map {-1 "-" 1 "+" 0 0}) println)))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one)) ; 50879490 too low
  (println "2." (two)))

(deftest everything
  (testing "a-tests"
    (is (= (run test1 4) "01029498"))
    (is (= (run "80871224585914546619083218645595" 100) "24176176"))
    (is (= (run "19617804207202209144916044189917" 100) "73745418"))
    (is (= (run "69317163492948606335995924319873" 100) "52432133"))
    ))
