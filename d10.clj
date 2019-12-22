(ns d10
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test0
".#..#
.....
#####
....#
...##")

(def test1
"......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(def test2
"#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

(defn parse [s]
  (let [lines (str/split-lines s)
        w (count (first lines))
        h (count lines)
        chrs (filter #{\. \#} s)
        points (for [y (range h) x (range w)] [x y])]
    (map second (filter #(#{\#} (first %)) (map vector chrs points)))))

(defn sign [x] (if (zero? x) 0 (/ x (Math/abs x))))

(defn v-angle [dx dy]
  (if (zero? dx)
    [0 (sign dy)]
    [(sign dx) (/ dy dx)]))

(defn angle [[x1 y1] [x2 y2]] (v-angle (- x2 x1) (- y2 y1)))

(defn without [x xs] (filter #(not= x %) xs))

(defn run-one [s]
  (let [points (parse s)
        angles (for [p1 points :let [ps (without p1 points)]]
                 [p1 (count (into #{} (map #(angle p1 %) ps)))])]
    (last (sort-by second angles))))

(defn one [] (run-one (slurp "10.in")))

(defn v-dist [& coords] (reduce + (map #(* % %) coords)))

(defn polar [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (conj (v-angle dx dy) (v-dist dx dy))))

(defn sector [[a1 a2 a3]]
  (if (zero? a1)
    (case a2
      -1 0
      1 2
      0 4
      (throw (ex-info "bad metric" {:a [a1 a2 a3]})))
    (if (pos? a1) 1 3)))

(defn cmp-angle [[a1 a2 a3] [b1 b2 b3]]
  (let [sa (sector [a1 a2 a3])
        sb (sector [b1 b2 b3])]
    (if (not= sa sb) (compare sa sb)
      (case sa
        0 0
        1 (compare a2 b2)
        2 0
        3 (compare a2 b2)
        (throw (ex-info "bad compare" {:a [a1 a2 a3] :b [b1 b2 b3]}))))))

(defn cmp-dist [[a1 a2 a3] [b1 b2 b3]] (compare a3 b3))

(defn make-metrics [p points]
  (->> points
       (without p)
       (map #(conj (polar p %) %))
       (sort cmp-angle)
       (partition-by #(subvec % 0 2))
       (map #(sort cmp-dist %))))

(def test-big
".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(defn run-two [s]
  (let [points (parse s)
        [p _] (run-one s)
        ms (map #(map last %)(make-metrics p points))]
    (loop [ms ms
           rv []]
      (if-not (seq ms) (vec (apply concat rv))
        (recur (filter seq (map rest ms)) (conj rv (map first ms)))))))

(run-two test-big)

(defn two []
  (let [kills (run-two (slurp "10.in"))
        [x y] (kills (dec 200))]
    (+ y (* x 100))))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a-tests"
    (is (= (run-one test0) [[3 4] 8]))
    (is (= (run-one test1) [[5 8] 33]))
    (is (= (run-one test2) [[1 2] 35])))
  (testing "b-tests"
    (let [kills (run-two test-big)]
      (is (= (kills (dec 1))   [11,12]))
      (is (= (kills (dec 2))   [12,1]))
      (is (= (kills (dec 3))   [12,2]))
      (is (= (kills (dec 10))  [12,8]))
      (is (= (kills (dec 20))  [16,0]))
      (is (= (kills (dec 50))  [16,9]))
      (is (= (kills (dec 100)) [10,16]))
      (is (= (kills (dec 199)) [9,6]))
      (is (= (kills (dec 200)) [8,2]))
      (is (= (kills (dec 201)) [10,9]))
      (is (= (kills (dec 299)) [11,1]))))
  (testing "main"
    (is (= nil (-main)))))
