(ns d14
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(def test1
"10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(def test2
"9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

(def test3
"157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

(def test4
"2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")

(def test5
"171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")

(defn parse [s]
  (let [lines (-> s
                  (str/replace #"\b([A-Z]+)\b" ":$1")
                  (str/replace #"[,=>]" "")
                  str/split-lines)
        vecs (map #(->> (str "[" % "]") read-string reverse) lines)]
    (into {}
          (map (fn[[k amount & deps]]
                 [k [amount (into {} (map vec (partition 2 deps)))]])
               vecs))))

(defn deps-for [tree k amount]
  (if (= k :ORE) [{k amount} {}]
    (let [[am deps] (tree k)
          mul (quot (+ amount am -1) am)
          deps (into {} (map #(vector (% 0) (* mul (% 1))) deps))
          left (- (* mul am) amount)]
      [deps (if (zero? left) {} {k left})])))

(deps-for (parse test1) :A 11)

(defn merge-need-have [need have]
  (loop [ks (keys need)
         need need
         have have]
    (if-not (seq ks) [need have]
      (let [[k & ks] ks
            v (need k)
            hv (have k)]
        (if-not hv (recur ks need have)
          (case (compare v hv)
            0 (recur ks (dissoc need k) (dissoc have k))
            -1 (recur ks (dissoc need k) (assoc have k (- hv v)))
            1 (recur ks (assoc need k (- v hv)) (dissoc have k))
            (throw (ex-info "bad compare" {}))))))))

(defn merge+
  ([a b] (merge+ a b +))
  ([a b op]
   (reduce #(assoc %1 %2 (op (get a %2 0) (b %2))) a (keys b))))

(defn run1 [tree need have]
  (let [ds (map #(apply deps-for tree %) need)
        need' (reduce merge+ {} (map first ds))
        have' (reduce merge+ have (map second ds))]
    (merge-need-have need' have')
  ))

(defn complete [[need have]]
  (and (= 1 (count need)) (= :ORE (first (keys need))) need))

(defn find-ore
  ([tree] (find-ore tree 1))
  ([tree amount]
   (let [f #(apply run1 tree %)
         init [{:FUEL amount} {}]]
     (some complete (iterate f init)))))

(defn one []
  (-> "14.in" slurp parse find-ore))

(def ALOT 1000000000000)

(defn bin-search-2 [tree n1 f1 n2 f2]
  (if (= f1 ALOT) n1
    (if (= f2 ALOT) n2
      (if (= (inc n1) n2) n1
        (let [n (quot (+ n1 n2) 2)
              f-n (:ORE (find-ore tree n))]
          (case (compare f-n ALOT)
            -1 (recur tree n f-n n2 f2)
            0 n
            1 (recur tree n1 f1 n f-n)))))))

(defn bin-search [tree]
  (loop [n1 1
         f1 (:ORE (find-ore tree n1))]
    (let [n2 (* 2 n1)
          f2 (:ORE (find-ore tree n2))]
      (if (> f2 ALOT)
        (if (> f1 ALOT)
          (throw (ex-info "huh" {}))
          (bin-search-2 tree n1 f1 n2 f2))
        (recur n2 f2)))))

(defn two [] (-> "14.in" slurp parse bin-search))

(defn -main [& args]
  (println "1." (one)) ; 432777 too high
  (println "2." (two)))

(deftest everything
  (testing "a-tests"
    (is (= (-> test1 parse find-ore vals first) 31))
    (is (= (-> test2 parse find-ore vals first) 165))
    (is (= (-> test3 parse find-ore vals first) 13312))
    (is (= (-> test4 parse find-ore vals first) 180697))
    (is (= (-> test5 parse find-ore vals first) 2210736)))

  (testing "b-tests"
    (is (= (bin-search (parse test3)) 82892753))
    (is (= (bin-search (parse test4)) 5586022))
    (is (= (bin-search (parse test5)) 460664)))

  (testing "main"
    (is (= nil (-main)))))
