(ns d04
  (:require [clojure.test :refer :all])
  (:gen-class))

(def in1 138241)
(def in2 674034)

(defn kosher? [n]
  (let [s (str n)
        pairs (partition 2 1 (map int s))]
    (and (= 6 (count s))
         (some #(apply = %) pairs)
         (not-any? #(apply > %) pairs))))

(defn one []
  (count (filter kosher? (range in1 in2))))

(defn kosher2? [n]
  (let [xs (->> n str (partition-by identity) (map count))]
    (some #{2} xs)))

(defn two []
  (count (filter kosher2? (filter kosher? (range in1 in2)))))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a-tests"
    (is (kosher? 111111))
    (is (not (kosher? 223450)))
    (is (not (kosher? 123789))))
  (testing "b-tests"
    (let [ksh #(and (kosher? %) (kosher2? %))]
      (is (ksh 112233))
      (is (not (ksh 123444)))
      (is (not (ksh 123789)))))
  (testing "main"
    (is (= nil (-main)))))
