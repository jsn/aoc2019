(ns d20
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1
"         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       ")

(def test2
"                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               ")

(defn letter? [c] (apply <= (map int [\A c \Z])))

(defn label [[x y] aref]
  (cond
    (letter? (aref (dec x) y)) (str (aref (- x 2) y) (aref (dec x) y))
    (letter? (aref (inc x) y)) (str (aref (inc x) y) (aref (+ x 2) y))
    (letter? (aref x (dec y))) (str (aref x (- y 2)) (aref x (dec y)))
    (letter? (aref x (inc y))) (str (aref x (inc y)) (aref x (+ y 2)))
    :else nil))

(defn labels->pairs [[[l1 [x1 y1]] [l2 [x2 y2]]]]
  (case l1
    "AA" [[:entry [x1 y1]]]
    "ZZ" [[:exit [x1 y1]]]
    [[[x1 y1] [x2 y2]] [[x2 y2] [x1 y1]]]))

(defn find-labels [cells aref]
  (->>
    (for [[[x y] t] cells
          :let [l (label [x y] aref)]
          :when (and (= \. t) l)]
      [l [x y]])
    (group-by first)
    (map second)
    (map labels->pairs)
    (apply concat)
    (into {})))

(defn pic->world [pic]
  (let [a (str/split-lines pic)
        aref #(get (a (+ %2 2)) (+ %1 2))
        w (- (count (first a)) 4)
        h (- (count a) 4)
        as (for [x (range w)
                 y (range h) :let [t (aref x y)] :when (#{\# \.} t)]
             [[x y] t])
        cells (into {} as)
        labels (find-labels cells aref)]
    {:w w :h h :cells cells :portals labels}
    ))

(defn neighbors [world [x y]]
  (filter #(= \. (get-in world [:cells %]))
          [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]]))

(defn connected [world [x y]] (get-in world [:portals [x y]]))

(defn next-move [world p]
  (let [ne (neighbors world p)]
    (if-let [c (connected world p)] (conj ne c) ne)))

(defn trace [world p neighb-f pred]
  (loop [inner {}
         border {p 0}
         gen 1]
    (let [inner' (merge inner border)
          border' (zipmap
                    (->> border
                         keys
                         (mapcat #(neighb-f world %))
                         (remove inner'))
                    (repeat gen))]
      (if-not (seq border')
        nil
        (if-let [found (some #(and (pred %) %) (keys border'))]
          gen
          (recur inner' border' (inc gen)))))))

(-> test1 pic->world) ; world->pic println)

(defn run-one [pic use-portals]
  (let [neighb-f (if use-portals next-move neighbors)
        world (pic->world pic)
        entry (get-in world [:portals :entry])
        exit (get-in world [:portals :exit])]
    (trace world entry neighb-f #(= exit %))))

(defn world->pic [{:keys [w h cells]}]
  (->> (for [y (range h) x (range w)] (get cells [x y] \space))
       (partition w)
       (map #(apply str %))
       (str/join \newline)))

;(-> test1 pic->world world->pic println)

(defn one []
  (run-one (slurp "20.in") true))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a-tests"
    (is (= (run-one test1 false) 26))
    (is (= (run-one test1 true) 23))
    (is (nil? (run-one test2 false)))
    (is (= (run-one test2 true) 58))
    ))
