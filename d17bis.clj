(ns d17bis
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(def test1
"..#..........
..#..........
##O####...###
#.#...#...#.#
##O###O###O##
..#...#...#..
..#####...^..")

(defn neighborhood [aref p]
  (for [dp [[0 0] [-1 0] [1 0] [0 -1] [0 1]]]
    (apply aref (map + p dp))))

(defn run-one [s]
  (let [a (str/split-lines s)
        aref #(get (a %2) %1)
        w (count (first a))
        h (count a)
        as (for [x (range 1 (dec w))
                 y (range 1 (dec h))
                 :when (= 5
                          (count (filter #{\# \O} (neighborhood aref [x y]))))]
             (* x y))]
    (apply + as)))

(defn read-input [] (read-string (str "[" (slurp "17.in") "]")))

(defn get-picture []
  (->> (read-input) vm/create (#(vm/run-sync % [])) (map char) (apply str)))

(def PICTURE (get-picture))

(println PICTURE)

(defn one [] (run-one PICTURE))

(def DIRS-S "^>v<")
(def DIRS [[0 -1] [1 0] [0 1] [-1 0]])

(defn pic->world [pic]
  (let [a (str/split-lines pic)
        aref #(get (a %2) %1)
        w (count (first a))
        h (count a)
        as (for [x (range w) y (range h)] [[x y] (aref x y)])
        [p d] (first (filter #(#{\^ \> \v \<} (last %)) as))
        d (str/index-of DIRS-S d)
        cells (assoc (into {} as) p \#)]
    {:w w :h h :p p :d d :cells cells}))

(defn world->pic [{:keys [w h p d cells]}]
  (let [cells' (assoc cells p (get DIRS-S d))]
    (->> (for [y (range h) x (range w)] (cells' [x y]))
         (partition w)
         (map #(apply str %))
         (str/join \newline))))

(def ^:dynamic *WORLD* (pic->world PICTURE))

(defn point-add [p dp] (mapv + p dp))

(defn try-move [d p]
  (let [dp (DIRS (mod d 4))
        p' (point-add p dp)
        t' (get-in *WORLD* [:cells p'])]
    (if (= t' \#) p' nil)))

(defn trace [p d ac]
  (let [dr (inc d) dl (dec d)]
    (lazy-seq
      (condp try-move p
        d :>> #(trace % d (inc ac))
        dr :>> #(cons ac (cons \R (trace % dr 1)))
        dl :>> #(cons ac (cons \L (trace % dl 1)))
        (cons ac nil)))))

(defn path-string []
  (apply str (remove #(= % 0) (trace (:p *WORLD*) (:d *WORLD*) 0))))

(defn find-subs [s]
  (let [[_ a b c]
        (re-matches #"^(.{1,20}?)\1*(.{1,20}?)\2*\1*(.{1,20}?)(\1|\2|\3*)+$" s)]
    [a b c]))

(defn path->subs [path abc]
  (loop [path path
         abc abc
         sub \A]
    (if (empty? abc) path
      (recur
        (str/replace path (first abc) (str sub))
        (rest abc)
        (-> sub int inc char)))))

(defn input-two []
  (let [path (path-string)
        abc (find-subs path)
        psubs (str/join "," (seq (path->subs path abc)))
        abcs (str/join
               "\n"
               (map #(subs (str/replace %1 #"(R|L)" ",$1,") 1) abc))]
    (str/join "\n" [psubs abcs "n" ""])))

(def MOVES-ASCII (mapv int (input-two)))

(defn run-moves []
  (let [code (assoc (read-input) 0 2)
        in MOVES-ASCII
        out (vm/run-sync (vm/create code) in)]
    (doseq [v out] (when (< v 255) (print (char v))))
    (last out)))

(defn two [] (run-moves))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a tests"
    (is (= (run-one test1) 76))))
