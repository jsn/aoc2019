(ns d05
  (:require [clojure.test :refer :all])
  (:gen-class))

(def test1 [1002,4,3,4,33])

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(defn parse-cmd [n]
  (let [s (format "%05d" n)
        opc (mod n 100)
        op (into [opc] (reverse (map #(- (int %) (int \0)) (subs s 0 3))))]
    (when-not (zero? (last op))
      (throw (ex-info "bad opcode" {:n n})))
    op))

(defn get-param [mem pc op i]
  (let [mode (op i)
        v (mem (+ pc i))]
    (case (op i)
      0 (mem v)
      1 v
      (throw (ex-info "unknown mode" {:mode mode})))))

(defn set-param [mem pc op i value]
  (let [mode (op i)
        v (mem (+ pc i))]
    (when-not (zero? mode)
      (throw (ex-info "bad write param mode" {:mode mode})))
    (assoc mem v value)))

(defn op3 [prg pc op]
  (let [f (case (op 0)
            1 +
            2 *
            7 #(if (< %1 %2) 1 0)
            8 #(if (= %1 %2) 1 0)
            (throw (ex-info "unknown opcode" {:op op})))
        x (get-param prg pc op 1)
        y (get-param prg pc op 2)]
    [(set-param prg pc op 3 (f x y)) (+ pc 4)]))

(def inputs (atom []))
(def outputs (atom []))

(defn input! []
  (let [[head & tail] @inputs]
    (reset! inputs tail)
    head))

(defn output! [v] (swap! outputs conj v))

(defn run1 [prg pc]
  (let [op (parse-cmd (prg pc))]
    (case (op 0)
      99 [prg nil]
      1 (op3 prg pc op)
      2 (op3 prg pc op)
      3 [(set-param prg pc op 1 (input!)) (+ pc 2)]
      4 (let [v (get-param prg pc op 1)]
          (output! v)
          [prg (+ pc 2)])
      5 (let [v (get-param prg pc op 1)]
          (if-not (zero? v)
            [prg (get-param prg pc op 2)]
            [prg (+ pc 3)]))
      6 (let [v (get-param prg pc op 1)]
          (if (zero? v)
            [prg (get-param prg pc op 2)]
            [prg (+ pc 3)]))
      7 (op3 prg pc op)
      8 (op3 prg pc op)
      (throw (ex-info "unknown op" {:op op})))))

(defn run [prg pc inp]
  (reset! inputs [inp])
  (reset! outputs [])
  (loop [prg prg
         pc pc]
    (let [[prg pc] (run1 prg pc)]
      (if pc
        (recur prg pc)
        @outputs))))

(defn one [] (run (read-input "05.in") 0 1))

(def test2 [3,9,8,9,10,9,4,9,99,-1,8])
(def test3 [3,9,7,9,10,9,4,9,99,-1,8])

(def testn [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(defn two [] (run (read-input "05.in") 0 5))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 678 (count (read-input "05.in")))))

  (testing "parse-cmd"
    (is (= (parse-cmd 2) [2 0 0 0]))
    (is (= (parse-cmd 1302) [2 3 1 0])))
  (testing "b-tests"
    (is (= (run test2 0 8) [1]))
    (is (= (run test2 0 7) [0]))

    (is (= (run test3 0 8) [0]))
    (is (= (run test3 0 7) [1]))

    (is (= (run testn 0 7) [999]))
    (is (= (run testn 0 8) [1000]))
    (is (= (run testn 0 19) [1001]))
    )
  (testing "main"
    (is (= nil (-main)))))
