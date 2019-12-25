(ns d19
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "19.in") "]")))

(defn scan-space []
  (let [code (read-input)]
    (loop [cnt 0
           [[x y] & ps] (for [y (range 50) x (range 50)] [x y])]
      (let [in (chan 2) out (chan 1)]
        (vm/run (vm/create code) in out)
        (when (zero? x) (println))
        (>!! in x)
        (>!! in y)
        (let [rv (<!! out)]
          (if-not (#{0 1} rv)
            (throw (ex-info "bad output" {:out rv}))
            (print (if (zero? rv) "." "#")))
          (let [cnt (+ cnt rv)]
            (if (seq ps) (recur cnt ps)
              cnt)))))))
(defn one []
  (scan-space))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
