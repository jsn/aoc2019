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

(def test2-1 "03036732577212944063491565474664")

(defn halve [xs] (let [v (vec xs)] (subvec v (/ (count v) 2))))

(defn run2-1 [xs]
  (->> xs
       reverse
       (reductions +)
       (map #(mod % 10))
       reverse
       vec))

(defn run2-n [arg n] (nth (iterate run2-1 arg) n))

(defn repeat-vec [v n]
  (apply vector (take (* n (count v)) (cycle v))))

(defn run2 [s n]
  (let [pos (Integer. (subs s 0 7))
        tail (subvec (repeat-vec (parse s) 10000) pos)
        tail' (run2-n tail n)
        rv (subvec tail' 0 8)]
    (apply str rv)))

'(let [n 39]
  (dotimes [i n]
    (->> i seq-n (take n) (map {-1 "-" 1 "+" 0 0}) println)))

(defn two []
  (time (run2 (slurp "16.in") 100)))

(defn -main [& args]
  (println "1." (one)) ; 50879490 too low
  (println "2." (two)))

(deftest everything
  (testing "a-tests"
    (is (= (run test1 4) "01029498"))
    (is (= (run "80871224585914546619083218645595" 100) "24176176"))
    (is (= (run "19617804207202209144916044189917" 100) "73745418"))
    (is (= (run "69317163492948606335995924319873" 100) "52432133"))
    )
  (testing "b-tests"
    (is (= (run2 "03036732577212944063491565474664" 100) "84462026"))
    (is (= (run2 "02935109699940807407585447034323" 100) "78725270"))
    ))
