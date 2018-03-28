(ns mlearning.core
  (:require [clojure.data.csv :as csv]
            [vizard.core :refer :all]
            [vizard.lite :as lite]
            [clojure.data.generators :as rnd]
            [kixi.stats.core :as stats]
            [redux.core :as redux]
            [clojure.java.io :as io])
  (:gen-class))

(defn csv-data->maps [csv-data]
    (map zipmap
         (->> (first csv-data) ;; First row is the header
              (map keyword)
              repeat)
         (rest csv-data)))

(defn fix-data [k v]
  (if (clojure.string/blank? v)
      Double/NaN
      (Double/parseDouble v)))

(defn convert-strings-to-doubles [data]
  (for [rec data]
    (into {}
      (for [[k v] rec]
        (let [new-val (fix-data k v )]
          [k new-val])))))

;;housing["rooms_per_household"] = housing["total_rooms"]/housing["households"]
;;housing["bedrooms_per_room"] = housing["total_bedrooms"]/housing["total_rooms"]
;;housing["population_per_household"]=housing["population"]/housing["households"]
;
(defn create-combined-attributes [data]
  (for [rec data]
     (let [total-rooms (:total_rooms rec)
           households (:households rec)
           total-bedrooms (:total_bedrooms rec)
           population (:population rec) ]
       {:room-per-household (/ total-rooms households)
        :bedrooms-per-room (/ total-bedrooms total-rooms)
        :populatoin-per-household (/ population households)})))

(defn add-combined-attributes [data]
  (map merge data (create-combined-attributes data)))

(defn get-stats [data]
  (transduce identity
             (redux/fuse {:mean stats/mean :sd stats/standard-deviation}) data))

(defn ->std-value [v {:keys [mean sd]}]
  (/ (- v mean) sd))

(defn standardize [data]
  (let [stats (into {} (for [k (keys (first data))]
                        [k (get-stats (map k data))]))
       ; _ (prn "stats: " stats)
        ]
    (for [rec data]
      (reduce-kv (fn [init k v] (assoc init k (->std-value v (stats k)))) {} rec))))

(defn fix-empty-strings [data]
  (for [rec data]
    (into {}
      (for [[k v] rec]
          [k (if (clojure.string/blank? v) :missing-value v)]))))

(defn make-one-hot [xs]
  (let [kys (map keyword (distinct xs))
        v (into {} (for [k kys] [k 0]))]
    (map #(assoc %2 (keyword %1) 1) xs (repeat v) )))

(defn calc-median [xs]
  (let [
        clean-of-nans (filter #(not (Double/isNaN %)) xs)
        size (count clean-of-nans)
        sorted (sort clean-of-nans)
        a (nth sorted (int (Math/floor (/ size 2))))
        b (nth sorted (int (Math/ceil (/ size 2))))
        ]
    (if (= a b) a (/ (+ a b) 2))))

(defn calc-medians [data]
  (let [ks (keys (first data))
;        _ (prn "calc-medians: " (first data))
        medians (into {} (for [k ks] [k (calc-median (map k data))]))
        ]
      medians))

(defn fill-missing-values-with-median [data]
  (let [;ks (keys (first data))
       ; _ (prn "ignored: " ignored-fields)
        ;relevant-data (map #(apply (partial dissoc %) ignored-fields) data )
        medians (calc-medians data)
        ]
      (for [rec data]
        (reduce-kv (fn [init k v]
                    (assoc init k (if (Double/isNaN v) (medians k) v)) )
                   {}
                   rec))))

(defn histogram-enum-data [data]
  "uses values in data enums and counts occurences"
 (reduce #(update %1 %2 (fnil inc 0)) {} data) )

;(defn convert-enum-field-to-numbered-category [data field]
; (let [hist (histogram-enum-data (map field data))
;       categories (keys hist)
;       ]
;   (for [rec data])) )

(defn histogram-data [data num-bins]
  "uses real numeric values divides into bins and counts occurences"
  (let [mx (int (apply max data))
        mn (int (apply min data))
        df (- mx mn)
        step (quot df num-bins)
        bounds (range (- mn step) (+ step mx) step)
        bins (map vector bounds (rest bounds))
        ;_ (prn "mx: " mx " mn: " mn " df: " df " step: " step " bounds: " bounds " bins: " bins)
        counts (reduce
                #(update %1
                  (first
                   (filter
                     (fn [rng]
                      (and (< %2 (second rng))
                           (>= %2 (first rng)))) bins))
                  (fnil inc 0))
                {} data)
        ]
    counts))

(defn make-graphable-hist-data [data num-bins]
   (->> (histogram-data data num-bins)
        (map #( into {} [[:x  (quot (apply + (first %)) 2) ][:y (second %) ] ]) )
        (sort-by :x)))

(defn make-graphable-enum-hist-data [data]
   (->> (histogram-enum-data data)
        (map #(into {} [[:x (first %) ][:y (second %) ] ]) )
        (sort-by :x)))

(defn in-test-set? [id ratio]
  (let [h (-> id hash (bit-and 0xFF))]
    (< h (* ratio 256))))

(defn make-stratified-test-and-train [data test-percentage & {:keys [stratified-field seed] :or {stratified-field :nothing seed 42}}]
  (binding [rnd/*rnd* (java.util.Random. seed)]
   (let [stratified-field-data (map stratified-field data)
         ;_ (prn "field-data: " field-data)
         hist (histogram-enum-data stratified-field-data)
         total-count (count data)
         split-data (into {} (for [[k v] hist]
                         (let [pct (double (/ v total-count))
                               ;_ (prn "key: " k " pct: " pct " index: " (int (* pct total-count test-percentage)))
                               filtered-data (if (not= stratified-field :nothing) (filter #(= (% stratified-field) k) data) data)
                               shuffled-data (rnd/shuffle filtered-data)
                               [test-data train-data] (split-at (int (* pct total-count test-percentage)) shuffled-data)]
                           [k {:train-data train-data :test-data test-data}])))
         train-data (flatten (map (comp :train-data val) split-data))
         test-data (flatten (concat (map (comp :test-data val) split-data)))
         ]
     {:train-data train-data :test-data test-data})))


(defn calc-pearson-cc [X Y]
  (let [sumX (apply + X)
        sumY (apply + Y)
        sumX*X (reduce + (map * X X))
        sumY*Y (reduce + (map * Y Y))
        sumX*Y (reduce + (map * X Y))
        n (count X)
        numer  (- sumX*Y (/ (* sumX sumY) n))
        denom (Math/sqrt (*
                           (- sumX*X (/ (* sumX sumX) n))
                           (- sumY*Y (/ (* sumY sumY) n))))
        ]
    (/ numer denom)))

(defn make-pearson-cc-matrix [data ks]
  (let [matrix (into {} (for [k ks ] [k (into {} (for [in-k (filter #(not= k %) ks)] [in-k 0]))]))]
    (reduce-kv
      (fn [init k v]
        (assoc init
               k
               (reduce-kv
                 (fn [in-init in-k in-v]
                   (assoc in-init in-k (calc-pearson-cc (map k data) (map in-k data))))
                 {}
                 v)))
      {}
      matrix)))

