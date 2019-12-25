(ns d21
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "21.in") "]")))

(defn runner []
  (let [code (read-input)
        in (chan) out (chan)]
    (vm/run (vm/create code) in out)
    (go (loop []
          (let [c (<! out)]
            (when c
              (print (char c))
              (when (= (int \newline) c) (flush))
              (recur)))))
    (loop []
      (when-let [l (read-line)]
        (doseq [c l] (>!! in (int c)))
        (>!! in (int \newline))
        (recur)))
    ))

(defn one [] (while true (runner)))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
