(ns d23
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            [vm])
  (:gen-class))

(defn read-input [] (read-string (str "[" (slurp "23.in") "]")))
(def INPUT (read-input))

(defn run-one [n]
  (let [in (chan) out (chan)
        in' (chan) out' (chan)]
    (vm/run (vm/create INPUT) in' out')
    (go (do
          (>! in' n)
          (while true
            (let [x (<! in)]
              (if (vector? x) (doseq [v x] (>! in' v)) (>! in' x))))))
    (go (while true (>! out [(<! out') (<! out') (<! out')])))
    [in out]
    ))

(def N 50)
(def EMPTY clojure.lang.PersistentQueue/EMPTY)

(defn run-network []
  (let [nodes (map run-one (range N))
        ins (mapv first nodes)
        outs (mapv second nodes)
        ins-map (zipmap ins (range N))
        outs-map (zipmap outs (range N))
        queues (vec (repeat N EMPTY))]
    (loop [queues queues]
      (let [i-alts
            (for [i (range N) :let [q (queues i) c (ins i)]]
              [c (if (= EMPTY q) -1 (peek q))])
            alts (vec (concat outs i-alts))
            [v c] (async/alts!! alts)]
        (if-let [node (ins-map c)]
          (recur (assoc queues node (rest (queues node))))
          (let [node (outs-map c)
                [dst x y] v]
            (if (= dst 255)
              v
              (recur (assoc queues dst (conj (queues dst) [x y]))))))))))

(defn one []
  (run-network))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
