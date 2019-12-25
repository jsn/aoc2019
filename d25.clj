(ns d25
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "25.in") "]")))

(defn runner []
  (let [code (read-input)
        in (chan) out (chan)]
    (vm/run (vm/create code) in out)
    (go (loop []
          (let [c (char (<! out))]
            (when c
              (print c)
              (when (= \newline c) (flush))
              (recur)))))
    (loop []
      (when-let [l (read-line)]
        (let [l
              (case l
                "a" "west"
                "d" "east"
                "s" "south"
                "w" "north"
                "i" "inv"
                l)]
          (doseq [c l] (>!! in (int c)))
          (>!! in (int \newline))
          (recur))))
    ))

(defn one [] (runner))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
