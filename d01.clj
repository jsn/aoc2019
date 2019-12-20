(ns d01
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(def in-1 (read-input "1.in"))

(defn fuel [mass] (-> mass (quot 3) (- 2)))

(defn one [] (->> in-1 (map fuel) (reduce +)))

(defn rfuel [mass]
  (->> mass (iterate fuel) rest (take-while pos?) (reduce +)))

(defn two [] (->> in-1 (map rfuel) (reduce +)))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(comment
; this is my scratch pad

(= (fuel 12) 2)
(= (fuel 14) 2)
(= (fuel 1969) 654)
(= (fuel 100756) 33583)

(= (rfuel 12) 2)
(= (rfuel 1969) 966)
(= (rfuel 100756) 50346)

(-main)

)


