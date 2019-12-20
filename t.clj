(ns t
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(defn one []
  "not implemented.")

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(comment
; this is my scratch pad

(read-input "1.in")

(-main)

)


