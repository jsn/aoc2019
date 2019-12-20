(ns d02
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(def test-prg [1,9,10,3,2,3,11,0,99,30,40,50])

(defn run1 [prg pc]
  (let [opc (prg pc)]
    (if (= opc 99)
      [prg nil]
      (let [x (->> pc (+ 1) prg prg)
            y (->> pc (+ 2) prg prg)
            p (->> pc (+ 3) prg)
            op (case opc
                 1 +
                 2 *
                 (throw (ex-info "illegal op" {:pc pc :opc opc})))]
        [(assoc prg p (op x y)) (+ pc 4)]))))

(defn run
  ([prg] (run prg 0))
  ([prg pc]
   (let [[prg pc] (run1 prg pc)]
     (if pc
       (recur prg pc)
       prg))))

(def in-1 (read-input "2.in"))

(defn one [noun verb]
  (let [prg (assoc in-1 1 noun 2 verb)]
    (-> prg (run 0) (get 0))))

(defn two []
  (->>
    (for [noun (range 100) verb (range 100)]
      (when (= (one noun verb) 19690720)
        (+ (* 100 noun) verb)))
    (filter some?)
    first))

(defn -main [& args]
  (println "1." (one 12 2))
  (println "2." (two)))

(comment
; this is my scratch pad


(run test-prg)

(-main)

)


