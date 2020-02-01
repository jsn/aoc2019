(ns d18bis
  (:require [clojure.test :refer :all]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn str->points [s y] (map #(vector [%2 y] %1) (seq s) (range)))

(defn pic->world [pic]
  (let [lines (str/split-lines pic)
        h (count lines)
        w (count (first lines))
        as (mapcat str->points lines (range))
        cells (into {} as)
        pois (->> as
                  (remove (fn [[[x y] t]] (#{\. \#} t)))
                  (mapcat reverse)
                  (apply hash-map))]
    {:w w :h h :cells cells :pois pois}
    ))

(def ^:dynamic *WORLD* (-> "18.in" slurp pic->world))

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

;(let [p [1 10]]
;  (for [f [inc dec] i [0 1] :let [p' (update p i f)]] p'))

(defn neighbours [p]
  (for [f [inc dec] i [0 1]
        :let [p' (update p i f)]
        :when (not (= \# (get-in *WORLD* [:cells p])))]
    p'))

(defn key-tile? [tile]
  (and (char? tile) (<= (int \a) (int tile) (int \z))))

(defn door-tile? [tile]
  (and (char? tile) (<= (int \A) (int tile) (int \Z))))

(defn item-pos [p] (if (vector? p) p (get-in *WORLD* [:pois (or p \@)])))

(defn trace-keys [start]
  (let [traces (bfs {:border {(item-pos start) nil}
                     :next-f neighbours
                     :derive-state cons})
        traces (into {} (map (fn [[k v]] [k (cons k v)]) traces))]
    (->> *WORLD*
         :cells
         (filter (fn [[p t]] (key-tile? t)))
         (map #(-> % first traces reverse)))))

(def step-len*
 (memoize
  (fn [w src dst]
    (let [p (item-pos dst)
          traces (trace-keys src)]
      (->> traces
           (filter #(= (last %) p))
           first
           count
           dec)))))

(defn step-len [src dst] (step-len* *WORLD* src dst))

(defn path-len [steps]
  (apply + (map (fn [[src dst]] (step-len src dst))(partition 2 1 steps))))

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

(defn edges->map [edges]
  (->> edges
       (mapcat #(vector % (reverse %)))
       (reduce (fn [c [a dist b]] (assoc-in c [a b] dist)) {})))

(defn fat-start? []
  (let [[x y] (-> *WORLD* :pois (get \@))
        tiles (for [dx [-1 0 1] dy [-1 0 1]
                    :let [x (+ x dx) y (+ y dy)]
                    :let [t (get-in *WORLD* [:cells [x y]])]]
                t)]
    (not-any? #{\#} tiles)))

(defn drop-fat-start [traces]
  (cond->> traces
    (fat-start?) (map (partial drop 2))))

(defn world-tree []
  (->> (trace-keys nil)
       (map rest)
       drop-fat-start
       (traces->tree \@ nil 0)
       edges->map))

(def ^:dynamic *TREE* (world-tree))

(defn passable? [k has]
  (or (not (door-tile? k)) (-> k str .toLowerCase first char has)))

;(passable? \K #{\z \k})

(defn next-keys
  ([p has] (next-keys p has nil 0))
  ([p has from dist]
   (cond
     (and (key-tile? p) (not (has p))) (list [dist p])
     (not (passable? p has)) nil

     :else
     (mapcat (fn [[p' d]]
               (when (not= p' from)
                 (next-keys p' has p (+ dist d))))
             (*TREE* p)))))

(defn h
  ([p has] (if-let [[total longest] (h p has nil 0)] (- (* 2 total) longest) 0))
  ([p has from dist]
   (let [cs (for [[p' d] (*TREE* p)
                  :when (not= p' from)
                  :let [rv (h p' has p d)]
                  :when rv]
              rv)]
     (if (seq cs)
       [(apply + dist (map first cs)) (+ dist (apply max (map second cs)))]
       (when (and (key-tile? p) (not (has p)))
         [dist dist])))))

(defn build-path [camefrom start finish]
  (loop [rv nil
         c finish]
    (let [rv (cons c rv)]
      (if (= c start)
        (map first rv)
        (recur rv (camefrom c))))))

(defn a* [p-start]
  (let [start [p-start #{}]]
    (loop [fscore (priority-map start (h p-start #{}))
           gscore {start 0}
           camefrom {}]
      (let [[[p has] fs] (peek fscore)]
        (cond
          (nil? p) :failed
          (= fs (gscore [p has])) (build-path camefrom start [p has])

          :else
          (let
            [fscore (pop fscore)
             nexts (for [[d p'] (next-keys p has)
                         :let [g (+ (gscore [p has]) (step-len p p'))
                               has' (conj has p')]
                         :when (< g (get gscore [p' has'] Integer/MAX_VALUE))]
                     [[p' has'] g])
             d-cf (zipmap (map first nexts) (repeat [p has]))
             d-gs (into {} nexts)
             d-fs (into {} (map (fn [[[p' has'] g]]
                                  [[p' has'] (+ g (h p' has'))])
                                nexts))]
            (recur (merge fscore d-fs)
                   (merge gscore d-gs)
                   (merge camefrom d-cf))))))))

(defmacro with-input [input & body]
  `(binding [*WORLD* (->> ~input pic->world)]
     (binding [*TREE* (world-tree)]
       ~@body)))

(defn one [] (time (path-len (a* \@))))

(defn cut-and-cull [grid xl xr yl yr]
  (let [grid (mapv #(subvec % xl (inc xr)) (subvec grid yl (inc yr)))
        tiles (set (apply concat grid))
        ks (filter key-tile? tiles)
        bad-doors (zipmap (filter door-tile? tiles) (repeat \.))
        grid (mapv (fn [v] (mapv #(or (bad-doors %) %) v)) grid)]
    grid))

(defn patch-and-cut [pic]
  (let [world (pic->world pic)
        [x y] (get-in world [:pois \@])
        grid (-> (->> pic str/split-lines (mapv vec))
                 (assoc-in [y x] \#)
                 (assoc-in [(dec y) x] \#)
                 (assoc-in [(inc y) x] \#)
                 (assoc-in [y (dec x)] \#)
                 (assoc-in [y (inc x)] \#)

                 (assoc-in [(dec y) (dec x)] \@)
                 (assoc-in [(inc y) (dec x)] \@)
                 (assoc-in [(inc y) (inc x)] \@)
                 (assoc-in [(dec y) (inc x)] \@))]
    [(cut-and-cull grid 0 x 0 y)
     (cut-and-cull grid x (dec (:w world)) 0 y)
     (cut-and-cull grid 0 x y(dec (:h world)))
     (cut-and-cull grid x (dec (:w world)) y (dec (:h world)))]))

(defn grid->pic [grid]
  (map #(str/join "\n" (map (partial apply str) %)) grid))

(defn solve-one [pic]
  (with-input pic
    (let [path (a* \@)]
      ; (prn path)
      (path-len path))))

(defn two []
  (->> "18.in"
       slurp
       patch-and-cut
       grid->pic
       (map solve-one)
       (apply +)
       time))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "a tests"
    (with-input (slurp "t18-1.in")
      (is (= (path-len (a* \@)) 8)))
    (with-input (slurp "t18-2.in")
      (is (= (path-len (a* \@)) 136)))
    (with-input (slurp "t18-3.in")
      (is (= (path-len (a* \@)) 81)))))
