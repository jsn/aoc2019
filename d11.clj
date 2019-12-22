(ns d11
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]]
            
            [vm :as vm])
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(defn robot
  ([in out] (robot in out {}))
  ([in out h]
   (<!!
     (go
       (loop [h h
              p [0 0]
              di 0]
         (>! out (get h p 0))
         (let [paint (<! in)
               turn (<! in)]
           (if-not paint
             h
             (let [h (assoc h p paint)
                   di (mod ((if (zero? turn) dec inc) di) 4)
                   p (mapv + p (dirs di))]
               (recur h p di)))))))))

(defn coll->range [coll]
  (let [x1 (apply min coll)
        x2 (apply max coll)]
    (range x1 (inc x2))))

(defn print-field [h]
  (let [ps (keys h)
        xs (map first ps)
        ys (map second ps)]
    (println (count ps))
    (doseq [y (coll->range ys)]
      (println (apply str (map #([\. \#] (get h [% y] 0)) (coll->range xs)))))))

'(let [in (chan 20)
      out (chan 20)]
  (doseq [i [1 0 0 0 1 0 1 0 0 1 1 0 1 0]] (>!! in i))
  (close! in)
  (print-field (robot in out)))

(defn one []
  (let [in (chan 10)
        out (chan 10)
        vm (vm/create (read-input "11.in"))]
    (vm/run vm in out)
    (count (robot out in))))

(defn two []
  (let [in (chan 10)
        out (chan 10)
        vm (vm/create (read-input "11.in"))]
    (vm/run vm in out)
    (print-field (robot out in {[0 0] 1}))))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 100 (count (read-input "1.in")))))
  (testing "main"
    (is (= nil (-main)))))
