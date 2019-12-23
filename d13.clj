(ns d13
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            
            [vm :as vm])
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(defn coll->range [coll]
  (let [x1 (apply min coll)
        x2 (apply max coll)]
    (range x1 (inc x2))))

(defn print-field [h]
  (when-let [score (h [-1 0])]
    (println "Score:" score))
  (let [ps (keys (dissoc h [-1 0]))
        xs (map first ps)
        ys (map second ps)]
    (doseq [y (coll->range ys)]
      (println
        (apply str (map #(get ".+%-o" (get h [% y] 0)) (coll->range xs)))))))

(defn terminal [in]
  (go
    (loop [h {}
           cnt 0]
      (let [x (<! in)
            y (<! in)
            tile (<! in)]
        (when (and (> cnt 855) (= 1 (mod cnt 50)))
          (println "cnt" cnt)
          (print-field h))
        (if-not x
          h
          (let [old (h [x y])]
            (when (and (= old 2) (not= old tile))
              (println "HIT" [x y tile]))
            (recur (assoc h [x y] tile) (inc cnt))))))))

'(let [in (chan 20)]
  (doseq [i [1,2,3,6,5,4]] (>!! in i))
  (close! in)
  (print-field (<!! (terminal in))))

(defn one []
  (let [in (chan 10)
        out (chan 10)
        vm (vm/create (read-input "13.in"))]
    (vm/run vm in out)
    (let [h (<!! (terminal out))]
      (print-field h)
      (->> h vals (filter #{2}) count))))

(def pat ",1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,")
(def rep ",1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,")

(defn patch [code] (str/replace code pat rep))

(defn read-and-patch-input [path]
  (read-string (str "[" (patch (slurp path)) "]")))

(defn two []
  (let [in (chan)
        out (chan)
        vm (vm/create (assoc (read-and-patch-input "13.in") 0 2))]
    (go (loop [] (>! in 0) (recur)))
    (vm/run vm in out)
    (let [h (<!! (terminal out))]
      (print-field h)
      (h [-1 0]))))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "main"
    (is (= nil (-main)))))
