(ns vm
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async :refer [chan go close!
                                                  <!! >!! <! >!]])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1 [1002,4,3,4,33])
(def test2 [3,9,8,9,10,9,4,9,99,-1,8])
(def test3 [3,9,7,9,10,9,4,9,99,-1,8])

(def testn [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(def test-quine [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
(def test-16d [1102,34915192,34915192,7,4,7,99,0])
(def test-bignum [104,1125899906842624,99])

(def INPUT (read-string (str "[" (slurp "09.in") "]")))

(defrecord VM [mem pc op halt base])

(defn- parse-op [n]
  (let [s (format "%05d" n)
        opc (mod n 100)]
    (into [opc] (reverse (map #(- (int %) (int \0)) (subs s 0 3))))))

(defn create
  ([mem] (create mem 0))
  ([mem pc]
   (let [mem (into [] (concat mem (replicate (* 10 (count mem)) 0)))
         op (parse-op (mem pc))]
     (->VM mem pc op false 0))))

(defn- vm-next
  ([^VM vm pc] (assoc vm :pc pc :op (parse-op ((.mem vm) pc))))
  ([^VM vm pc i newval & kvs]
   (let [mem (assoc (.mem vm) i newval)
         op (parse-op (mem pc))]
     (apply assoc vm :pc pc :mem mem :op op kvs))))

(defn- param [^VM vm i]
  (let [mode ((.op vm) i)
        v ((.mem vm) (+ (.pc vm) i))]
    (case (int ((.op vm) i))
      0 ((.mem vm) v)
      1 v
      2 ((.mem vm) (+ (.base vm) v))
      (throw (ex-info "unknown mode" {:mode mode})))))

(defn- param-ptr [^VM vm i]
  (let [mode ((.op vm) i)
        base (case (int mode)
               0 0
               2 (.base vm)
               (throw (ex-info "bad write param mode" {:mode mode})))]
    (+ base ((.mem vm) (+ (.pc vm) i)))))

(defn- op-3 [^VM vm f]
  (let [x (param vm 1)
        y (param vm 2)
        p (param-ptr vm 3)]
    (vm-next vm (+ (.pc vm) 4) p (f x y))))

(defn- cond-jump [^VM vm pred]
  (let [pc (if (pred (param vm 1))
             (param vm 2)
             (+ 3 (.pc vm)))]
    (vm-next vm pc)))

(defn- vm-in [^VM vm v]
  (assoc (vm-next vm (+ 2 (.pc vm)) (param-ptr vm 1) v) :halt nil))

(defn- vm-out [^VM vm]
  (assoc (vm-next vm (+ 2 (.pc vm))) :halt nil))

(defn- halt [^VM vm reason & etc] (assoc vm :halt (apply vector reason etc)))

(defn- run1 [^VM vm]
  (case (int ((.op vm) 0))
    99 (halt vm :halt)
    1 (op-3 vm +)
    2 (op-3 vm *)
    3 (halt vm :in)
    4 (let [v (param vm 1)] (halt vm :out v))
    5 (cond-jump vm #(not (zero? %)))
    6 (cond-jump vm zero?)
    7 (op-3 vm #(if (< %1 %2) 1 0))
    8 (op-3 vm #(if (= %1 %2) 1 0))
    9 (assoc (vm-next vm (+ 2 (.pc vm))) :base (+ (.base vm) (param vm 1)))
    (throw (ex-info "unknown op" {:vm vm}))))

(defn run
  ([vm] (run vm (chan)))
  ([vm in] (run vm in (chan)))
  ([^VM vm in out]
   (go
     (loop [^VM vm vm]
       (if-let [[reason v] (.halt vm)]
         (case reason
           :halt (do
                   (close! out)
                   vm)
           :out (if (>! out v) (recur (vm-out vm)) vm)
           :in (recur (vm-in vm (<! in)))
           (throw (ex-info "weird halt" {:reason reason})))
         (recur (run1 vm)))))))

(defn run-sync [^VM vm ins]
  (loop [vm vm
         ins ins
         outs []]
    (if-let [[reason v] (.halt vm)]
      (case reason
        :halt outs
        :out (recur (vm-out vm) ins (conj outs v))
        :in (let [[in & ins] ins]
              (when (nil? in)
                (throw (ex-info "out of inputs" {:vm vm})))
              (recur (vm-in vm in) ins outs))
        (throw (ex-info "weird halt" {:reason reason})))
      (recur (run1 vm) ins outs))))

(defn run-seq [code inputs]
  (let [in (chan (count inputs))
        out (chan)
        vm (create code 0)]
    (doseq [v inputs] (>!! in v))
    (run vm in out)
    (take-while some? (repeatedly #(<!! out)))))

(defn one [] (run-seq INPUT [1]))
(defn two [] (run-seq INPUT [2]))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "parse-op"
    (is (= (parse-op 2) [2 0 0 0]))
    (is (= (parse-op 1302) [2 3 1 0])))
  (testing "b-tests"
    (let [run #(-> %1 (run-seq [%2]) last)]
      (is (= (run test2 8) 1))
      (is (= (run test2 7) 0))

      (is (= (run test3 8) 0))
      (is (= (run test3 7) 1))

      (is (= (run testn 7) 999))
      (is (= (run testn 8) 1000))
      (is (= (run testn 19) 1001))))
  (testing "d09 tests"
    (let [run #(-> %1 (run-seq [%2]) vec)]
      (is (= (run test-quine 0) test-quine))
      (is (= (run test-bignum 0) [(test-bignum 1)]))
      (is (= (-> (run test-16d 0) last str count) 16))))

  (testing "sync tests"
    (is (= (run-sync (create test2) [8]) [1]))
    (is (= (run-sync (create test2) [7]) [0]))
    ))
