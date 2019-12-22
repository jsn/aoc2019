(ns d08
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def w 25)
(def h  6)
(def INPUT (partition (* w h) (slurp "08.in")))

(defn ndigits [s d] (count (filter #{d} s)))
(defn make-pair [s] [(ndigits s \0) (* (ndigits s \1) (ndigits s \2))])

(defn one []
  (let [sq (* w h)
        lmap (into {} (map make-pair INPUT))
        k (apply min (keys lmap))]
    (lmap k)))

(defn two []
  (let [pxs (apply map (fn [& tail] (first (drop-while #{\2} tail))) INPUT)
        rows (partition w pxs)]
    (doseq [r rows]
      (println (str/join (map {\0 " " \1 "#"} r))))))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 100 (count INPUT))))
  (testing "main"
    (is (= nil (-main)))))
