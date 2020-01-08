(ns d25
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "25.in") "]")))

(defn translate-cmd [l]
  (case l
    "a" "west"
    "d" "east"
    "s" "south"
    "w" "north"
    "i" "inv"
    ">" (do
          (print "Filename: ")
          (flush)
          (slurp (read-line)))
    l))

(defn line-xform [rf]
  (let [line (volatile! [])]
    (fn
      ([] (rf))
      ([result]
       (let [l @line]
         (vreset! line [])
         (cond-> result
           (seq l) (rf (apply str l))
           true rf)))
      ([result c]
       (let [c (char c)]
         (if (= c \newline)
           (let [l @line]
             (vreset! line [])
             (rf result (apply str l)))
           (vswap! line conj c)))))))

(defn lines-xform [rf]
  (let [lines (volatile! [])]
    (fn
      ([] (rf))
      ([result]
       (let [ls @lines]
         (vreset! lines [])
         (cond-> result
           (seq ls) (rf ls)
           true rf)))
      ([result l]
       (vswap! lines conj l)
       (if (> (count @lines) 100)
         (reduced (rf result (conj @lines ::overflow)))
         (when (re-matches #"^Command\?$" l)
           (rf result @lines)
           (vreset! lines [])))))))

(defn runner []
  (let [code (read-input)
        in (chan)
        out (chan 1 (comp line-xform lines-xform))]

    (vm/run (vm/create code) in out)

    (go (loop []
          (if-let [ls (<! out)]
            (do
              (doseq [l ls] (println l))
              (recur))
            (println ">>>>>>>>>>>>> VM HALT <<<<<<<<<<<<<<"))))

    (loop []
      (when-let [l (read-line)]
        (doseq [c (translate-cmd l)] (>!! in (int c)))
        (>!! in (int \newline))
        (recur))))
    )

(defn one [] (runner))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
