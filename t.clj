(ns t
  (:require [clojure.test :refer :all])
  (:gen-class))

(defn read-input [path] (read-string (str "[" (slurp path) "]")))

(defn one []
  "not implemented.")

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 100 (count (read-input "1.in")))))
  (testing "main"
    (is (= nil (-main)))))
