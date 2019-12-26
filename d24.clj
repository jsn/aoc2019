(ns d24
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1 [
"....#
#..#.
#..##
..#..
#...."

"####.
....#
##..#
.....
##..."

".....
.....
.....
#....
.#..."])

(defn read-input [] (slurp "24.in"))

(defn pic->world [pic]
  (let [a (str/split-lines pic)
        aref #(get (a %2) %1)
        w (count (first a))
        h (count a)
        as (for [x (range w) y (range h)] [[x y] (aref x y)])
        cells (into {} as)]
    {:w w :h h :cells cells}))

(defn world->pic [{:keys [w h cells]}]
  (->> (for [y (range h) x (range w)] (cells [x y]))
       (partition w)
       (map #(apply str %))
       (str/join \newline)))

(defn next-state [cells p]
  (let [me (cells p)
        cnt (->> [[-1 0] [1 0] [0 -1] [0 1]]
                 (map #(mapv + p %))
                 (map cells)
                 (filter #{\#})
                 count)]
    (if (= me \#)
      (if (= cnt 1) me \.)
      (if (#{1 2} cnt) \# \.))))

(defn turn [{:keys [w h cells] :as world}]
  (let [cells' (for [x (range w) y (range h) :let [p [x y]]]
                 [p (next-state cells p)])]
    (assoc world :cells (into {} cells'))))

(defn run-n [world n] (nth (iterate turn world) n))

(defn find-repeated [world]
  (loop [seen #{(world->pic world)}
         world world]
    (let [world' (turn world)
          pic' (world->pic world')]
      (if (seen pic') world'
        (recur (conj seen pic') world')))))

(defn point->i [{:keys [w h]} [x y]] (+ (* y h) x))

(defn score [{:keys [w h cells]}]
  (->> cells
       (filter #(= \# (last %)))
       (map (fn [[[x y] _]] (Math/pow 2 (+ x (* y w)))))
       (apply +)
       int))

(defn one [] (-> "24.in" slurp pic->world find-repeated score))

(defn pic->world3d [pic]
  (let [a (str/split-lines pic)
        aref #(get (a %2) %1)
        w (count (first a))
        h (count a)
        as (for [x (range w) y (range h) :when (not= [x y] [2 2])]
             [[x y 0] (aref x y)])
        cells (into {} as)]
    {:w w :h h :cells cells}))

(defn world3d->pic [{:keys [w h cells]} z]
  (->> (for [y (range h) x (range w)] (get cells [x y z] \?))
       (partition w)
       (map #(apply str %))
       (str/join \newline)))

(defn top [[x y z]]
  (cond 
    (= [2 3] [x y]) (mapv #(vector % 4 (dec z)) (range 5))
    (= 0 y) [[2 1 (inc z)]]
    :else [[x (dec y) z]]))

(defn rotate [n [x y z]]
  (loop [[x y z] [x y z] n (mod n 4)]
    (if (zero? n) [x y z] (recur [y (- 4 x) z] (dec n)))))

(defn neighbors [p]
  (mapcat (fn [i] (->> p (rotate i) top (map #(rotate (- i) %)))) (range 4)))

(defn next-state-3d [cells p]
  (let [me (cells p)
        cnt (->> (neighbors p)
                 (map cells)
                 (filter #{\#})
                 count)]
    (if (= me \#)
      (if (= cnt 1) me \.)
      (if (#{1 2} cnt) \# \.))))

(defn turn3d [{:keys [w h cells] :as world}]
  (let [zr (->> cells keys (map last) set)
        cells' (for [x (range w)
                     y (range h) :when (not= [x y] [2 2])
                     z (range (dec (apply min zr)) (+ 2 (apply max zr)))
                     :let [p [x y z]]]
                 [p (next-state-3d cells p)])]
    (assoc world :cells (into {} cells'))))

(defn run-3d [world n] (nth (iterate turn3d world) n))

(defn score-3d [{:keys [cells]}] (->> cells vals (filter #{\#}) count))

(defn run-two [pic n] (-> pic pic->world3d (run-3d n) (score-3d)))

(defn two [] (run-two (read-input) 200))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a tests"
    (is (= (-> (test1 0) pic->world (run-n 4) world->pic) (test1 1)))
    (is (= (-> (test1 0) pic->world find-repeated world->pic) (test1 2)))
    (is (= (-> (test1 0) pic->world find-repeated score) 2129920)))
  (testing "b tests"
    (is (= (run-two (test1 0) 10) 99))))
