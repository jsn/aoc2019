(ns d22
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1
["deal with increment 7
deal into new stack
deal into new stack"
[0 3 6 9 2 5 8 1 4 7]])

(def test2
["cut 6
deal with increment 7
deal into new stack"
[3 0 7 4 1 8 5 2 9 6]])

(def test3
["deal with increment 7
deal with increment 9
cut -2"
[6 3 0 7 4 1 8 5 2 9]])

(def test4
["deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1"
[9 2 5 8 1 4 7 0 3 6]])

(defn match-line [l]
  (condp re-matches l
    #"^deal into new stack$" [:new]
    #"^cut (.+)$" :>> #(vector :cut (bigint (% 1)))
    #"^deal with increment (.+)$" :>> #(vector :increment (bigint (% 1)))
    (throw (ex-info "no match" {:line l}))))

(defn parse [s] (->> s str/split-lines (map match-line)))

(defn op->fn [[tag arg]]
  (case tag
    :new #(- -1 %)
    :cut #(- % arg)
    :increment #(* arg %)
    (throw (ex-info "bad tag" {:tag tag}))))

(defn ops->fn* [ops] (->> ops reverse (map op->fn) (apply comp)))

(defn op->ab [[tag arg] [a b]]
  (case tag
    :new [(- a) (- -1 b)]
    :cut [a (- b arg)]
    :increment [(* a arg) (* b arg)]
    (throw (ex-info "bad tag" {:tag tag}))))

(defn ops->ab [ops n]
  (loop [[op & ops'] ops
         ab [1N 0N]]
    (let [ab' (mapv #(mod % n) (op->ab op ab))]
      (if (seq ops') (recur ops' ab')
        ab'))))

(defn ab->fn [[a b] n] #(mod (+ (* a %) b) n))

(defn ops->fn [ops n] (ab->fn (ops->ab ops n) n))

(defn project [f n]
  (let [r (range n)
        kv (zipmap (map #(mod (f %) n) r) r)]
    (mapv kv r)
  ))

;(-> (test3 0) parse (ops->fn2 10) (project 10))

(defn run1-10 [s] (let [f (-> s parse (ops->fn 10))] (project f 10)))

(defn one []
  (let [f (-> "22.in" slurp parse (ops->fn 10007))]
    (f 2019)))

(def ^:dynamic *BIGDECK*   119315717514047N)
(def ^:dynamic *MUCHTIMES* 101741582076661N)

(defn mod-big [x] (mod x *BIGDECK*))

(defn *ab [[a1 b1] [a2 b2]]
  (mapv mod-big [(* a1 a2) (+ (* a1 b2) b1)]))

(defn pow-ab [ab n]
  (if (= n 1) ab
    (let [ab2 (*ab ab ab)]
      (if (zero? (mod n 2))
        (recur ab2 (/ n 2))
        (*ab ab (pow-ab ab2 (quot n 2)))))))

(defn pow-mod [x n]
  (loop [x x
         n n
         ac 1]
    (if (= n 1) (mod-big (* x ac))
      (let [x2 (mod-big (* x x))
            ac (if (zero? (mod n 2)) ac (mod-big (* ac x)))]
        (recur x2 (quot n 2) ac)))))

(defn inv [x] (pow-mod x (- *BIGDECK* 2)))

(defn y->x [a b y]
  (let [x1 (mod-big (- y b))
        x (mod-big (* x1 (inv a)))]
    x))

'(binding [*BIGDECK* 11 *MUCHTIMES* 4]
  (let [ab (-> (test4 0) parse (ops->ab *BIGDECK*) (pow-ab *MUCHTIMES*))
        f' (-> (test4 0) parse (ops->fn *BIGDECK*))
        f (ab->fn ab *BIGDECK*)]
    (println (project f *BIGDECK*))
    (println (y->x (ab 0) (ab 1) 5))))

(defn two []
  (let [ab (-> "22.in" slurp parse (ops->ab *BIGDECK*) (pow-ab *MUCHTIMES*))
        f (ab->fn ab *BIGDECK*)]
    (y->x (ab 0) (ab 1) 2020)))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two))) ; 1487651599921N is too low
                        ; 14095717263710N is too low

(deftest everything
  (testing "a tests"
    (is (= (run1-10 (test1 0)) (test1 1)))
    (is (= (run1-10 (test2 0)) (test2 1)))
    (is (= (run1-10 (test3 0)) (test3 1)))
    (is (= (run1-10 (test4 0)) (test4 1))))
  (testing "regressions"
    (binding [*BIGDECK* 10 *MUCHTIMES* 5]
      (let [ab (-> (test4 0) parse (ops->ab *BIGDECK*) (pow-ab *MUCHTIMES*))
            f' (-> (test4 0) parse (ops->fn *BIGDECK*))
            f (ab->fn ab *BIGDECK*)]
        (is (= (project f 10) (project (comp f' f' f' f' f') 10)))))))
