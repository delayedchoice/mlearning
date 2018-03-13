(ns mlearning.core
  (:require [clojure.data.csv :as csv]
            [vizard.core :refer :all]
            [vizard.lite :as lite]
            [clojure.data.generators :as rnd]
            [clojure.java.io :as io])
  (:gen-class))

(defn csv-data->maps [csv-data]
    (map zipmap
         (->> (first csv-data) ;; First row is the header
              (map keyword) ;; Drop if you want string keys instead
              repeat)
         (rest csv-data)))

(defn get-data  []
  (with-open [reader (io/reader "resources/housing.csv")]
          (->> (csv/read-csv reader)
               csv-data->maps
        ;       (take 50)
               (doall))))


(def quantifiable-housing-data (map #(dissoc % :ocean_proximity) (get-data)))

(defn convert-strings-to-doubles [data]
  (for [rec data]
    (into {}
      (for [[k v] rec] [k (if (clojure.string/blank? v) Double/NaN (Double/parseDouble v))]))))

(def numeric-housing-data (convert-strings-to-doubles quantifiable-housing-data))

(defn calc-median [xs]
  (let [clean-of-nans (filter #(not (Double/isNaN %)) xs)
        size (count clean-of-nans)
        sorted (sort clean-of-nans)
        a (get clean-of-nans (int (Math/floor (/ size 2))))
        b (get clean-of-nans (int (Math/ceil (/ size 2))))
        ]
    (if (= a b) a (/ (+ a b) 2))))



(defn fill-missing-values-with-median [data]
  (let [ks (keys (first data))
        medians (into {} (for [k ks] [k (calc-median (map k data))]))
        ]
      (for [rec data]
        (reduce-kv (fn [init k v]
                    (assoc init k (if (Double/isNaN v) (medians k) v)) )
                   {}
                   rec))))

(def clean-numeric-data (fill-missing-values-with-median numeric-housing-data))

(def housing-labels (map :median_house_value clean-numeric-data))

(def clean-numeric-input-data (map #(dissoc % :median_house_value) clean-numeric-data))

(defn histogram-enum-data [data]
  "uses values in data enums and counts occurences"
 (reduce #(update %1 %2 (fnil inc 0)) {} data) )

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


;building histogramsa -- need labels
;(def median-house-value-historgram
;  (p! (lite/lite {:mark "bar"
;                  :encoding {:x {:field "x"
;                                :axis {:title "Median House Value"}
;                                :type "ordinal"}
;                             :y {:aggregate "sum"
;                                 :field "y"
;                                 :type "quantitative"}
;                             :color {:field "col"
;                                     :type "nominal"}}}
;                 (make-graphable-hist-data housing-labels 50))))
;(def median-income-histogram
;  (p! (lite/lite {:mark "bar"
;                  :encoding {:x {:field "x"
;                                :axis {:title "Median Income"}
;                                 :type "ordinal"}
;                             :y {:aggregate "sum"
;                                 :field "y"
;                                 :type "quantitative"}
;                             :color {:field "col"
;                                     :type "nominal"}}}
;                 (make-graphable-hist-data
;                   ;had to scale, graphics got confused
;                   (map (comp #(* 100 %) :median_income) clean-numeric-input-data)
;                   50))))
;(def ocean-proximity-histogram
;  (p! (lite/lite {:mark "bar"
;                  :encoding {:x {:field "x"
;                                 :axis {:title "Ocean Proximity"}
;                                 :type "nominal"}
;                             :y {:aggregate "sum"
;                                 :field "y"
;                                 :type "quantitative"}
;                             :color {:field "col"
;                                     :type "nominal"}}}
;                 (make-graphable-enum-hist-data (map :ocean_proximity (get-data))))))
;

(defn in-test-set? [id ratio]
  (let [h (-> id hash (bit-and 0xFF))]
    (< h (* ratio 256))))

(defn add-id-field [data]
  (map #(assoc % :id (int (+ (* (:longitude %) 1000) (:latitude %)))) data))

(def data-with-id (add-id-field clean-numeric-input-data))

(defn add-income-cat [data]
  (map #(let [i (Math/ceil (/ (:median_income %) 1.5))
              v (if (> i 5) 5 (int i))]
          (assoc % :income-cat v)) data))

(def data-with-income-cat (add-income-cat data-with-id))

(defn make-stratified-test-and-train [data field test-percentage seed]
  (binding [rnd/*rnd* (java.util.Random. seed)]
   (let [hist (histogram-enum-data (map field data))
         total-count (count data)
         split-data (into {} (for [[k v] hist]
                         (let [pct (double (/ v total-count))
              ;                 _ (prn "key: " k " pct: " pct " index: " (int (* pct total-count test-percentage)))
                               filtered-data (filter #(= (% field) k) data)
                               shuffled-data (rnd/shuffle filtered-data)
                               [test-data train-data] (split-at (int (* pct total-count test-percentage)) shuffled-data)]
                           [k {:train-data train-data :test-data test-data}])))
         train-data (flatten (map (comp :train-data val) split-data))
         test-data (flatten (concat (map (comp :test-data val) split-data)))
         ]
     {:train-data train-data :test-data test-data})))

;(def stratified-data (make-stratified-test-and-train data-with-income-cat :income-cat 0.2 42))

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

(def  ks [:median_house_value
          :median_income
          :total_rooms
          :housing_median_age
          :households
          :total_bedrooms
          :population
          :longitude
          :latitude] )

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

;(def long-lat-scatter-plot
;  (let [long-lat-data (map #(into {} [[:median_house_value (:median_house_value %) ]
;                                      [:population (:population %) ]
;                                      [:x (:longitude %) ]
;                                      [:y (:latitude %) ]]) clean-numeric-data)]
;    (p! (lite/lite {:mark "point"
;                    :width 700
;                    :height 700
;                    :encoding {:x {:field "x"
;                                    :scale {:domain [-125 -113]}
;                                    :type "quantitative"}
;                               :y {:field "y"
;                                    :scale {:domain [32 44]}
;                                    :type "quantitative"}
;                               :size {:field "population"
;                                      :type "quantitative"}
;                               :color {:field "median_house_value"
;                                       :type "quantitative"
;                                       :scale {:scheme "inferno"}}}}
;                  long-lat-data))))



