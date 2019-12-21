(ns d03
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(-> "3.in" slurp str/split-lines count)

(def test1
"R8,U5,L5,D3
U7,R6,D4,L4")

(def test2
"R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")

(def test3
"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(defn read-input [path] (->> path io/reader line-seq))
(defn parse-move [move] [(first move) (Integer. (subs move 1))])
(defn parse-moves [path] (map parse-move (str/split path #",")))

(defn move-point [[x y] [dir delta]]
  (case dir
    \R [(+ x delta) y]
    \L [(- x delta) y]
    \U [x (+ y delta)]
    \D [x (- y delta)]
    (throw (ex-info "strange move" {:move [dir delta]}))))

(defn moves->points [moves] (reductions move-point [0 0] moves))

(defn make-line [[[x1 y1] [x2 y2]]]
  (into [] (interleave (sort [x1 x2]) (sort [y1 y2]))))

(defn points->lines [points]
  (->>
    points
    (partition 2 1)
    (map make-line)))

(defn str->points [s] (-> s parse-moves moves->points))
(defn str->lines [s] (-> s str->points points->lines))

(defn c-range [c1 c2]
  (if (= c1 c2) [c1]
    (range c1 c2 (if (<= c1 c2) 1 -1))))

(defn point-range [[x1 y1] [x2 y2]]
  (into [] (for [x (c-range x1 x2) y (c-range y1 y2)] [x y])))

(defn point-range-all [p1 p2] (conj (point-range p1 p2) p2))

(defn maybe-assoc [coll k v] (if (contains? coll k) coll (assoc coll k v)))

(defn trace-pair [rv ac [p1 p2]]
  (loop [rv rv
         ac ac
         points (point-range p1 p2)]
    (if-not (seq points) [rv ac]
      (recur (maybe-assoc rv (first points) ac)
             (inc ac)
             (rest points)))))

(defn trace-points [points]
  (loop [rv {}
         ac 0
         pairs (partition 2 1 points)]
    (if (seq pairs)
      (let [[rv ac] (trace-pair rv ac (first pairs))]
        (recur rv ac (rest pairs)))
      (maybe-assoc rv (last points) (inc ac)))))

(defn cross [[ax1 ay1 ax2 ay2] [bx1 by1 bx2 by2]]
  (let [x1 (max ax1 bx1)
        y1 (max ay1 by1)
        x2 (min ax2 bx2)
        y2 (min ay2 by2)]
    (when (and (<= x1 x2) (<= y1 y2))
      [x1 y1 x2 y2])))

(defn best-coord [x1 x2]
  (if (<= x1 0 x2) 0
    (if (> x1 0) x1 x2)))

(defn best-point [[x1 y1 x2 y2]] [(best-coord x1 x2) (best-coord y1 y2)])

(defn crosses [ls1 ls2]
  (map best-point (filter seq (for [l1 ls1 l2 ls2] (cross l1 l2)))))

(defn origin-dist [[x y]] (+ (Math/abs x) (Math/abs y)))

(defn best-dist [ls1 ls2]
  (apply min (filter pos? (map origin-dist (crosses ls1 ls2)))))

(defn run-one [s]
  (->> s str/split-lines (map str->lines) (apply best-dist)))

(defn one []
  (->> "3.in" slurp run-one))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a-tests"
    (is (= (run-one test1) 6))
    (is (= (run-one test2) 159))
    (is (= (run-one test3) 135)))
  (testing "test one"
    (is (= (one) 293)))
  (testing "main"
    (is (= (-main) nil))))


