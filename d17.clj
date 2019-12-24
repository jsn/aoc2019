(ns d17
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
  (let [code (read-input) in (chan) out (chan)]
    (vm/run (vm/create code) in out)
    (<!! (go (loop [rv []]
               (let [v (<! out)]
                 (if-not v (apply str rv)
                   (recur (conj rv (char v))))))))))

(defonce PICTURE (get-picture))

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

(defn point-add [p dp] (mapv + p dp))

(defn movement [p d n]
  (let [dp (DIRS d)]
    (reductions point-add p (repeat n dp))))

(def SUBS {:A [:R 10 :R 8 :L 10 :L 10]
           :B [:R 8 :L 6 :L 6]
           :C [:L 10 :R 10 :L 6]})

(def MOVES [:A :B :B :A :C :B :C :C :B :A])

(defn move->str [m] (if (keyword? m) (subs (str m) 1) (str m)))

(defn moves->str []
  (let [main (str/join "," (map move->str MOVES))
        funs (map #(str/join "," (map move->str (SUBS %))) [:A :B :C])]
    (str (str/join "\n" (conj funs main)) "\nn\n")
  ))

(def MOVES-ASCII (map int (moves->str)))

(defn run-moves []
  (let [code (assoc (read-input) 0 2)
        in (chan (count MOVES-ASCII))
        out (chan)]
    (doseq [m MOVES-ASCII] (>!! in m))
    (vm/run (vm/create code) in out)
    (<!! (go (loop []
               (let [v (<! out)]
                 (if-not v nil
                   (do
                     (if (> v 255)
                       (println "END:" v)
                       (print (char v)))
                     (flush)
                     (recur)))))))))

(defn move [{:keys [p d cells] :as world} cmd]
  (case cmd
    :L (assoc world :d (mod (dec d) 4))
    :R (assoc world :d (mod (inc d) 4))
    (if (#{:A :B :C} cmd) (reduce move world (SUBS cmd))
      (if-not (integer? cmd)
        (throw (ex-info "unknown command" {:cmd cmd}))
        (let [path (movement p d cmd)
              p' (last path)
              tiles (map #(if (= \. (cells %)) \! \x) path)
              cells' (into cells (map vector path tiles))]
          (assoc world :p p' :cells cells'))))))

(defn run-two [moves]
  (world->pic (reduce move (pic->world PICTURE) moves)))

; (println (run-two MOVES))

(defn two [] (run-moves))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a tests"
    (is (= (run-one test1) 76)))
  (testing "b tests"
    (let [pic (run-two MOVES)]
      (is (= false (str/includes? pic "#")))
      (is (= false (str/includes? pic "!")))
      (is (not= false (str/includes? pic "x"))))))
