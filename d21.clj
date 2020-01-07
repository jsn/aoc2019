(ns d21
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "21.in") "]")))

(def VM (vm/create (read-input)))

(defn runner []
  (let [in (chan) out (chan)]
    (vm/run VM in out)
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

(defn run-two []
  (run-sync
    "NOT A J

    NOT C T
    AND D T
    OR T J

    NOT B T
    AND D T
    OR T J

    RUN"))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
