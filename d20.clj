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

(defn label [[x y] aref]
  )

(defn labels [cells aref]
  (for [[[x y] t] cells :when (= \. t)] [x y]))

(defn pic->world [pic]
  (let [a (str/split-lines pic)
        aref #(get (a (+ %2 2)) (+ %1 2))
        w (- (count (first a)) 4)
        h (- (count a) 4)
        as (for [x (range w)
                 y (range h) :let [t (aref x y)] :when (#{\# \.} t)]
             [[x y] t])
        cells (into {} as)]
    {:w w :h h :cells cells}
    (labels cells aref)
    ))

(-> test1 pic->world); world->pic println)

(defn world->pic [{:keys [w h cells]}]
  (->> (for [y (range h) x (range w)] (get cells [x y] \space))
       (partition w)
       (map #(apply str %))
       (str/join \newline)))

(-> test1 pic->world); world->pic println)

(defn one []
  "not implemented.")

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
