(ns d21
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "21.in") "]")))

(def VM (vm/create (read-input)))

(defn run-sync [input]
  (let [output (->> input
                    str/split-lines
                    (map str/trim)
                    (remove empty?)
                    (map #(conj (mapv int %) (int \newline)))
                    (apply concat)
                    (vm/run-sync VM))
        rv (last output)]
    (->> output
         (map #(cond-> % (< % 256) char))
         (apply str)
         println)
    (if (> rv 256) rv nil)))

(defn run-one []
  (run-sync
    "NOT A J
    NOT C T
    AND D T
    OR T J
    WALK"))

(defn one [] (run-one))

(defn two []
  (run-sync
    "
    NOT J J
    AND A J
    AND B J
    AND C J
    NOT J J
    AND D J

    OR E T
    OR H T
    AND T J

    RUN"))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))
