(ns d19
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "19.in") "]")))

(def VM (vm/create (read-input)))

(defn tile-at [x y]
  (let [outs (vm/run-sync VM [x y])
        rv (outs 0)]
    (when-not (#{0 1} rv) (throw (ex-info "bad output" {:out rv})))
    rv))

(defn scan-space []
  (loop [cnt 0
         [[x y] & ps] (for [y (range 50) x (range 50)] [x y])]
    (let [rv (tile-at x y)
          cnt (+ cnt rv)]
      (when (zero? x)
        (println)
        (print (format "%2d " y)))
      (print (get ".#" rv))
      (if (seq ps) (recur cnt ps) cnt))))

(defn one []
  (time (scan-space)))

(defn find-first-1 [y xs]
  (->> xs (filter #(pos? (tile-at % y))) first))

(defn bounds [y xl xr]
  (let [xl' (->> xl (+ 3) (range xl) (find-first-1 y))
        xr' (->> xr (+ 3) (range xr) reverse (find-first-1 y))]
    [xl' xr']))

(defn rows
  ([] (cons [0 0 0] (cons [1 1 0] (rows 2 1 1))))
  ([y xl xr]
   (let [[xl xr] (bounds y xl xr)]
     (lazy-seq (cons [y xl xr] (rows (inc y) xl xr))))))

(defn window [h]
  (let [top (rows)
        bottom (drop (dec h) top)]
    (map vector top bottom)))

(defn contains-rect? [w [[y1 xl1 xr1] [y2 xl2 xr2]]] (<= (+ xl2 w -1) xr1))

(defn find-rect [w h]
  (let [win (first (filter #(contains-rect? w %) (window h)))
        [[y _ _] [_ x _]] win]
    [x y]))

(defn two []
  (time (let [[x y] (find-rect 100 100)] (+ y (* x 10000)))))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "regressions"
    '(is (= (one)
           (apply + (for [[y xl xr] (take 50 (rows)) :let [xr (min xr 49)]]
                      (max (- xr xl -1) 0)))))))
