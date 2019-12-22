(ns vm-d07-tests
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async :refer [chan go close! <! >!]]
            [clojure.core.async :as async :refer [chan thread close! <!! >!!]]
            [clojure.math.combinatorics :as combo]
            
            [vm :as vm]
            )
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(def INPUT (read-input "07.in"))

(def test1 [1002,4,3,4,33])
(def test2 [3,9,8,9,10,9,4,9,99,-1,8])
(def test3 [3,9,7,9,10,9,4,9,99,-1,8])

(def testn [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(def test-1 [[3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
             43210])
(def test-2 [[3,23,3,24,1002,24,10,24,1002,23,-1,23,
              101,5,23,23,1,24,23,23,4,23,99,0,0]
             54321])
(def test-3 [[3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
              1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
             65210])

(defn run-amp-1 [code input phase]
  (last (vm/run-seq code [phase input])))

(defn run-amps [code sq] (reduce #(run-amp-1 code %1 %2) 0 sq))

(defn brute-force [code]
  (->> 5 range combo/permutations (map #(run-amps code %)) (apply max)))

(defn one [] (brute-force INPUT))

(def test-4 [[3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
              27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
             139629729])

(def test-5
  [[3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
    -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
    53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
   18216])

(defn run-amps-b [code sq]
  (let [chans (mapv (fn [phase] (let [c (chan 20)] (>!! c phase) c)) sq)
        last-out (chan 20)
        first-in (first chans)
        chans (conj chans last-out)]
    (doseq [[in out] (partition 2 1 chans)]
      (vm/run (vm/create code 0) in out))
    (>!! first-in 0)
    (loop [rv nil]
      (let [v (<!! last-out)]
        ; (println ">>" v)
        (if v
          (do
            (>!! first-in v)
            (recur v))
          rv)))))

(defn brute-force-b [code]
  (->> (range 5 10) combo/permutations (map #(run-amps-b code %)) (apply max)))

(defn two [] (brute-force-b INPUT))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "vm-tests"
    (let [run #(-> %1 (vm/run-seq [%2]) last)]
      (is (= (run test2 8) 1))
      (is (= (run test2 7) 0))

      (is (= (run test3 8) 0))
      (is (= (run test3 7) 1))

      (is (= (run testn 7) 999))
      (is (= (run testn 8) 1000))
      (is (= (run testn 19) 1001))))
  (testing "a-tests"
    (is (= (brute-force (test-1 0)) (test-1 1)))
    (is (= (brute-force (test-3 0)) (test-3 1)))
    (is (= (brute-force (test-2 0)) (test-2 1))))
  
  (testing "b-tests"
    (is (= (brute-force-b (test-4 0)) (test-4 1)))
    (is (= (brute-force-b (test-5 0)) (test-5 1))))
  (testing "a"
    (is (= 511 (count INPUT)))))
