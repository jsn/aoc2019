(ns d12
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1
"<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>")

(def test2
"<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>")

(defn parse [s]
  (for [m (->> (str "[" (str/replace s #"[^\d\s-]" "") "]")
               read-string
               (partition 3)
               (map vec))]
    {:p m :v [0 0 0]}))

(defn gravity-1 [m1 m2]
  (let [v (mapv #(+ %1 (compare %3 %2)) (:v m1) (:p m1) (:p m2))]
    (assoc m1 :v v)))

(defn velocity-1 [m] (assoc m :p (mapv + (:p m) (:v m))))

(defn step [moons]
  (mapv velocity-1 (map #(reduce gravity-1 % moons) moons)))

(defn steps [n moons] (->> moons (iterate step) (drop n) first))

(defn p-energy [m] (reduce + (map #(Math/abs %) (:p m))))
(defn k-energy [m] (reduce + (map #(Math/abs %) (:v m))))
(defn energy-1 [m] (* (p-energy m) (k-energy m)))
(defn energy [moons] (reduce + (map energy-1 moons)))

(defn one []
  (->> "12.in" slurp parse (steps 1000) energy))

(defn slice [moons i]
  (mapv #(get % i) (interleave (map :p moons) (map :v moons))))

(defn find-period-1 [moons i]
  (let [s (slice moons i)]
    (loop [moons' moons
           cnt 0]
      (let [s' (slice moons' i)]
        (if (and (= s s') (pos? cnt)) cnt
          (recur (step moons') (inc cnt)))))))

(defn find-periods [moons]
  (map #(find-period-1 moons %) [0 1 2]))

(defn gcd 
      [a b]
      (if (zero? b)
      a
      (recur b, (mod a b))))
 
(defn lcm 
      [a b]
      (/ (* a b) (gcd a b)))

(defn find-period [moons]
  (reduce lcm (find-periods moons)))

(defn two []
  (-> "12.in" slurp parse find-period))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "gravity"
    (is (= (gravity-1 {:v [0 0 0] :p [3 2 8]} {:p [5 2 1]})
           {:v [1 0 -1] :p [3 2 8]})))
  (testing "energy"
    (is (= (->> test1 parse (steps 10) energy) 179))
    (is (= (->> test2 parse (steps 100) energy) 1940)))

  (testing "b-tests"
    (is (= (-> test1 parse find-period) 2772))
    (is (= (-> test2 parse find-period) 4686774924)))

  (testing "main"
    (is (= nil (-main)))))
