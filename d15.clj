(ns d15
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(def CMDS [[0 -1] [0 1] [-1 0] [1 0]])

(defn coll->range [coll]
  (let [x1 (apply min coll)
        x2 (apply max coll)]
    (range x1 (inc x2))))

(defn tile->char [t] (if (integer? t) \.  (or t \space)))

(defn print-field [h]
  (println "-----------")
  (let [ps (keys h)
        xs (map first ps)
        ys (map second ps)
        xr (coll->range xs)
        yr (coll->range ys)]
    (println "x =" (apply min xr) (apply max xr)
             "y =" (apply min yr) (apply max yr))
    (doseq [y yr]
      (println
        (apply str (map #(tile->char (h [% y])) xr))))))

(defn neighbors [p]
  (for [cmd [1 2 3 4]] {:cmd cmd :p (mapv + p (CMDS (dec cmd)))}))

(defn next-move [h p]
  (let [ps (map #(assoc % :w (get h (:p %) 0)) (neighbors p))
        ps (sort-by :w (filter #(integer? (:w %)) ps))]
    (:cmd (first ps))))

(defn player [in out]
  (go
    (loop [p [0 0]
           h {p 1}
           marks []
           cnt 0]
      (when (zero? (mod cnt 1000))
        (println cnt)
        (print-field (apply assoc h p \@ [0 0] \0 marks)))
      (if (= 7000 cnt)
        {:p p :target (marks 0) :h h}
        (let [cmd (next-move h p)
              rep (do
                    (>! out cmd)
                    (<! in))
              dp (CMDS (dec cmd))
              p2 (mapv + p dp)]
          (let [p (if (zero? rep) p p2)
                w (get h p 0)
                marks (if (= rep 2) (conj marks p2 \%) marks)
                tile ([\# \. \%] rep)]
            (recur p (assoc h p2 tile p (inc w)) marks (inc cnt))))))))

(defn explore-maze [code]
  (let [in (chan) out (chan)]
    (vm/run (vm/create code) in out)
    (<!! (player out in))))

(defn trace
  ([world]
   (trace world [0 0]))
  ([{:keys [h p target] :as world} origin]
   (trace world origin (fn [p t] (= p target))))
  ([{:keys [h p target]} origin pred]
   (let [h (into {} (filter #(-> % second integer? not) h))
         block? #(= \# (h %))]
     (loop [inner {}
            border {origin 0}
            gen 1]
       (let [inner' (merge inner border)
             border' (into {}
                           (map vector
                                (mapcat #(->> %
                                              neighbors
                                              (map :p)
                                              (remove block?)
                                              (remove inner'))
                                        (keys border))
                                (repeat gen)))]

         ; (print-field (assoc (merge h inner') [0 0] \0 target \% p \@))
         (if-not (seq border')
           (dec gen)
           (if-let [found (some #(and (apply pred %) %) border')]
             found
             (recur inner' border' (inc gen))))
         )))))

(defn one []
  (->> "15.in" read-input explore-maze str (spit "15.edn"))
  (-> "15.edn" slurp read-string trace))

(defn -main [& args]
  (let [[oxy-p oxy-dist] (one)]
    (println "1." oxy-p oxy-dist)
    (println "2." (-> "15.edn" slurp read-string (trace oxy-p)))))

(deftest everything
  (testing "nothing"
    (is (= nil nil))))
