(ns d06
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1
"COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(defn str->pairs [s] (->> s str/split-lines (map #(str/split % #"\)"))))
(defn read-input [path] (->> path slurp str->pairs))

(defn pairs->tree [pairs]
  (loop [rv {}
         pairs pairs]
    (if-not (seq pairs) rv
      (let [[l r] (first pairs)]
        (recur (assoc rv l (conj (get rv l []) r)) (rest pairs))))))

(defn tree->lengths [tree]
  (loop [queue (tree "COM")
         depth 1
         rv {"COM" 0}]
    (if-not (seq queue) rv
      (recur
        (apply concat (map tree queue))
        (inc depth)
        (into rv (for [x queue] [x depth]))))))
         
(defn run-one [s]
  (->> s str->pairs pairs->tree tree->lengths vals (reduce +)))

(defn one [] (run-one (slurp "06.in")))

(def test2
"COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(defn run-two [s]
  (let [rtree (->> s str->pairs (map reverse) pairs->tree)
        parent #(-> % rtree first)
        ancestry #(->> % (iterate parent) (take-while some?) reverse)]
    (loop [n1 (-> "YOU" parent ancestry)
           n2 (-> "SAN" parent ancestry)]
      (if-not (= (first n1) (first n2))
        (+ (count n1) (count n2))
        (recur (rest n1) (rest n2))))))

(defn two [] (run-two (slurp "06.in")))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 1564 (count (read-input "06.in")))))
  (testing "a-tests"
    (is (= 42 (run-one test1))))
  (testing "b-tests"
    (is (= 4 (run-two test2))))
  (testing "main"
    (is (= nil (-main)))))
