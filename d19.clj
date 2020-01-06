(ns d19
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "19.in") "]")))

(defn scan-space []
  (let [code (read-input)
        vm (vm/create code)]
    (loop [cnt 0
           [[x y] & ps] (for [y (range 50) x (range 50)] [x y])]
      (let [outs (vm/run-sync vm [x y])
            rv (outs 0)]
        (when (zero? x) (println))
        (if-not (#{0 1} rv)
          (throw (ex-info "bad output" {:out rv}))
          (print (if (zero? rv) "." "#")))
        (let [cnt (+ cnt rv)]
          (if (seq ps) (recur cnt ps)
            cnt))))))

(defn one []
  (time (scan-space)))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
