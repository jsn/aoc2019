(ns d18
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"#########
#b.A.@.a#
#########")

(def test2
"#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(def test3
"########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")

(defn pic->world [pic]
  (let [a (str/split-lines pic)
        aref #(get (a %2) %1)
        w (count (first a))
        h (count a)
        as (for [x (range w) y (range h)] [[x y] (aref x y)])
        p (first (first (filter #(= \@ (last %)) as)))
        cells (assoc (into {} as) p \.)]
    {:w w :h h :p p :cells cells}))

(def ^:dynamic *WORLD* (-> "18.in" slurp pic->world))

(defn world->pic [{:keys [w h p cells]}]
  (let [cells' (assoc cells p \@)]
    (->> (for [y (range h) x (range w)] (cells' [x y]))
         (partition w)
         (map #(apply str %))
         (str/join \newline))))

(defn bfs [{:keys [done border next-f derive-state] :as opts :or {done {}}}]
  (let [done' (merge done border)
        gen-next (fn [[k v]]
                   (zipmap (remove done' (next-f k))
                           (repeat (derive-state k v))))
        border' (->> border
                     (map gen-next)
                     (apply merge))]
    (if (seq border')
      (recur (assoc opts :done done' :border border'))
      done')))

(defn neighbours [[x y]]
  (for [p [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
        :when (not (= \# (get-in *WORLD* [:cells p])))]
    p))

(defn key-tile? [tile] (<= (int \a) (int tile) (int \z)))

(defn trace-keys [border]
  (let [traces (bfs {:border border
                     :next-f neighbours
                     :derive-state cons})
        traces (into {} (map (fn [[k v]] [k (cons k v)]) traces))]
    (->> *WORLD*
         :cells
         (filter (fn [[p t]] (key-tile? t)))
         (map #(->> % first traces reverse)))))

(defn item-pos [p]
  (if (or (nil? p) (= p \@))
    (:p *WORLD*)
    (->> *WORLD* :cells (filter (fn [[k v]] (= v p))) first first)))

(defn path-len [src dst]
  (let [src (item-pos src)
        dst (item-pos dst)
        traces (trace-keys {src nil})]
    (->> traces
         (filter #(= (last %) dst))
         first
         count
         dec)))

(defn traces->tree [root cur dist traces]
  (let [traces' (group-by first traces)]
    (case (count traces')
      0 '()
      1 (let [cur' (first (keys traces'))
              cur-t (get-in *WORLD* [:cells cur'])
              traces'' (->> traces (map rest) (remove empty?))]
          (if (not= cur-t \.)
            (lazy-seq
              (cons [root (inc dist) cur-t]
                    (traces->tree cur-t nil 0 traces'')))
            (recur root cur' (inc dist) traces'')))
      (if cur
        (lazy-seq
          (cons [root dist cur]
                (traces->tree cur nil 0 traces)))
        (apply
          concat
          (map (fn [[k v]]
                 (traces->tree root k 1 (->> v (map rest) (remove empty?))))
               traces'))))))

(defn p->str [p]
  (if (vector? p)
    (str \" (first p) ":" (second p) \")
    (str \" p \")))

(defn tree->dot [tree]
  (str/join
    "\n"
    ["digraph X {"
     (str/join
       "\n"
       (for [[src dist dst] tree]
         (let [attrs (if (> dist 1) (str "[label=\"" dist "\"]") "")]
           (str "  " (p->str src) " -> " (p->str dst) " " attrs " ;"))))
     "}"]))

(time
  (let [p (:p *WORLD*)
        traces (trace-keys {p nil})
        tree (->> traces
                  (map rest)
                  (traces->tree \@ nil 0)
                  tree->dot)]
    (spit "18.dot" tree)))

(def *STEPS*
  [\@
   \x \j \m \o
   \l
   \t \y
   \i \p \q
   \d \u \h
   \w \c])


(defn one []
  (apply + (map (fn [[src dst]] (path-len src dst))(partition 2 1 *STEPS*))))

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  )
