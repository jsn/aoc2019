(ns d22
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1
["deal with increment 7
deal into new stack
deal into new stack"
[0 3 6 9 2 5 8 1 4 7]])

(def test2
["cut 6
deal with increment 7
deal into new stack"
[3 0 7 4 1 8 5 2 9 6]])

(def test3
["deal with increment 7
deal with increment 9
cut -2"
[6 3 0 7 4 1 8 5 2 9]])

(def test4
["deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1"
[9 2 5 8 1 4 7 0 3 6]])

(defn match-line [l]
  (condp re-matches l
    #"^deal into new stack$" [:new]
    #"^cut (.+)$" :>> #(vector :cut (Integer. (% 1)))
    #"^deal with increment (.+)$" :>> #(vector :increment (Integer. (% 1)))
    (throw (ex-info "no match" {:line l}))))

(defn parse [s] (->> s str/split-lines (map match-line)))

(defn op->fn [[tag arg]]
  (case tag
    :new #(- -1 %)
    :cut #(- % arg)
    :increment #(* arg %)
    (throw (ex-info "bad tag" {:tag tag}))))

(defn ops->fn* [ops] (->> ops reverse (map op->fn) (apply comp)))

(defn op->ab [[tag arg] [a b]]
  (case tag
    :new [(- a) (- -1 b)]
    :cut [a (- b arg)]
    :increment [(* a arg) (* b arg)]
    (throw (ex-info "bad tag" {:tag tag}))))

(defn ops->ab [ops n]
  (loop [[op & ops'] ops
         ab [1 0]]
    (let [ab' (mapv #(mod % n) (op->ab op ab))]
      (if (seq ops') (recur ops' ab')
        ab'))))

(defn ops->fn [ops n]
  (let [[a b] (ops->ab ops n)]
    #(mod (+ (* a %) b) n)))

(defn project [f n]
  (let [r (range n)
        kv (zipmap (map #(mod (f %) n) r) r)]
    (mapv kv r)
  ))

;(-> (test3 0) parse (ops->fn2 10) (project 10))

(defn run1-10 [s] (let [f (-> s parse (ops->fn 10))] (project f 10)))

(defn one []
  (let [f (-> "22.in" slurp parse (ops->fn 10007))]
    (f 2019)))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a tests"
    (is (= (run1-10 (test1 0)) (test1 1)))
    (is (= (run1-10 (test2 0)) (test2 1)))
    (is (= (run1-10 (test3 0)) (test3 1)))
    (is (= (run1-10 (test4 0)) (test4 1)))
    ))
