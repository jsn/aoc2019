(ns vm
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async :refer [chan thread close! <!! >!!]])
  (:gen-class))

(def test1 [1002,4,3,4,33])
(def test2 [3,9,8,9,10,9,4,9,99,-1,8])
(def test3 [3,9,7,9,10,9,4,9,99,-1,8])

(def testn [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(defrecord VM [mem pc in out op halt])

(defn parse-op [n]
  (let [s (format "%05d" n)
        opc (mod n 100)]
    (into [opc] (reverse (map #(- (int %) (int \0)) (subs s 0 3))))))

(defn create-vm
  ([mem] (create-vm mem 0))
  ([mem pc] (create-vm mem pc (chan)))
  ([mem pc in] (create-vm mem pc in (chan)))
  ([mem pc in out]
   (let [op (parse-op (mem pc))]
     (->VM mem pc in out op false))))

(defn vm-next
  ([vm pc] (assoc vm :pc pc :op (parse-op ((.mem vm) pc))))
  ([vm pc i newval & kvs]
   (let [mem (assoc (.mem vm) i newval)
         op (parse-op (mem pc))]
     (apply assoc vm :pc pc :mem mem :op op kvs))))

(defn vm-in [vm pc i]
  (vm-next vm pc i (<!! (.in vm))))

(defn vm-out [vm pc v]
  (>!! (.out vm) v)
  (assoc vm :pc pc :op (parse-op ((.mem vm) pc))))

(defn vm-param [vm i]
  (let [mode ((.op vm) i)
        v ((.mem vm) (+ (.pc vm) i))]
    (case ((.op vm) i)
      0 ((.mem vm) v)
      1 v
      (throw (ex-info "unknown mode" {:mode mode})))))

(defn vm-param-ptr [vm i]
  (let [mode ((.op vm) i)]
    (when-not (zero? mode)
      (throw (ex-info "bad write param mode" {:mode mode})))
    ((.mem vm) (+ (.pc vm) i))))

(defn vm-op-3 [vm f]
  (let [x (vm-param vm 1)
        y (vm-param vm 2)
        p (vm-param-ptr vm 3)]
    (vm-next vm (+ (.pc vm) 4) p (f x y))))

(defn vm-cond-jump [vm pred]
  (let [pc (if (pred (vm-param vm 1))
             (vm-param vm 2)
             (+ 3 (.pc vm)))]
    (vm-next vm pc)))

(defn vm-run1 [vm]
  (case ((.op vm) 0)
    99 (assoc vm :halt true)
    1 (vm-op-3 vm +)
    2 (vm-op-3 vm *)
    3 (vm-in vm (+ 2 (.pc vm)) (vm-param-ptr vm 1))
    4 (vm-out vm (+ 2 (.pc vm)) (vm-param vm 1))
    5 (vm-cond-jump vm #(not (zero? %)))
    6 (vm-cond-jump vm zero?)
    7 (vm-op-3 vm #(if (< %1 %2) 1 0))
    8 (vm-op-3 vm #(if (= %1 %2) 1 0))
    (throw (ex-info "unknown op" {:vm vm}))))

(defn vm-run [vm]
  (if (.halt vm)
    (do
      (close! (.out vm))
      vm)
    (recur (vm-run1 vm))))

(defn vm-run-seq [code inputs]
  (let [in (chan (count inputs))
        vm (create-vm code 0 in)]
    (doseq [v inputs] (>!! in v))
    (thread (vm-run vm))
    (take-while some? (repeatedly #(<!! (.out vm))))))

(defn one []
  "not implemented.")

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "parse-op"
    (is (= (parse-op 2) [2 0 0 0]))
    (is (= (parse-op 1302) [2 3 1 0])))
  (testing "b-tests"
    (let [run #(-> %1 (vm-run-seq [%2]) last)]
      (is (= (run test2 8) 1))
      (is (= (run test2 7) 0))

      (is (= (run test3 8) 0))
      (is (= (run test3 7) 1))

      (is (= (run testn 7) 999))
      (is (= (run testn 8) 1000))
      (is (= (run testn 19) 1001)))))
