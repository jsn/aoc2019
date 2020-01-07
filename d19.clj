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
      (when (zero? x) (println))
      (print (get ".#" rv))
      (if (seq ps) (recur cnt ps) cnt))))

(defn one []
  (time (scan-space)))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
