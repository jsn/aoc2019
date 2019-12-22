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

(defn angle [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (if (= 0 dx dy) 0
      (if (zero? dx) [0 (/ dy (Math/abs dy))]
        (if (zero? dy) [(/ dx (Math/abs dx)) 0]
          [(/ dx (Math/abs dx)) (/ dy dx)])))))

(defn run-one [s]
  (let [points (parse s)
        angles (for [p1 points]
                 [p1 (dec (count (into #{} (map #(angle p1 %) points))))])]
    (last (sort-by second angles))))

(defn one [] (run-one (slurp "10.in")))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 100 (count (read-input "1.in")))))
  (testing "a-tests"
    (is (= (run-one test0) [[3 4] 8]))
    (is (= (run-one test1) [[5 8] 33]))
    (is (= (run-one test2) [[1 2] 35])))
  (testing "main"
    (is (= nil (-main)))))
