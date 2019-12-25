(ns d18
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1
"#########
#b.A.@.a#
#########")

(def test2
"#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(defn pic->world [pic]
  (let [a (str/split-lines pic)
        aref #(get (a %2) %1)
        w (count (first a))
        h (count a)
        as (for [x (range w) y (range h)] [[x y] (aref x y)])
        p (first (first (filter #(= \@ (last %)) as)))
        cells (assoc (into {} as) p \.)]
    {:w w :h h :p p :cells cells}))

(defn world->pic [{:keys [w h p cells]}]
  (let [cells' (assoc cells p \@)]
    (->> (for [y (range h) x (range w)] (cells' [x y]))
         (partition w)
         (map #(apply str %))
         (str/join \newline))))

(defn one []
  "not implemented.")

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
