(ns mlearning.housing-inc
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [vizard.core :refer :all]
            [vizard.lite :as lite]
            [incanter.core :as i]
            [incanter.io]
            [incanter.stats :as s]
            [incanter.charts :as c]
            [mlearning.core :as ml]))

(defn get-data  []
  (with-open [reader (io/reader "resources/housing.csv")]
          (->> (csv/read-csv reader)
               ml/csv-data->maps
              ; (take 50)
               (doall))))

(defonce data (get-data))

(defn add-income-cat [data]
  (map #(let [i (Math/ceil (/ (:median_income %) 1.5))
              v (if (> i 5) 5 (int i))]
          (assoc % :income-cat v)) data))

(defn add-id-field [data]
  (map #(assoc % :id (int (+ (* (:longitude %) 1000) (:latitude %)))) data));

(def quantifiable-housing-data (map #(dissoc % :ocean_proximity) data))

(def categorical-housing-data (map :ocean_proximity data))

(def numeric-housing-data (ml/convert-strings-to-doubles quantifiable-housing-data))

(def all-numeric-housing-data-no-nulls (ml/fill-missing-values-with-median numeric-housing-data))

(def all-numeric-data-with-id (add-id-field all-numeric-housing-data-no-nulls))

(def all-numeric-data-with-income-cat (add-income-cat all-numeric-data-with-id))

(def stratified-numeric-data (ml/make-stratified-test-and-train all-numeric-data-with-income-cat 0.2 :stratified-filed :income-cat :seed 42))

(def numeric-training-data (:train-data stratified-numeric-data))

(def train-housing-labels (map :median_house_value numeric-training-data ))

(def numeric-training-input-data (map #(dissoc % :median_house_value) numeric-training-data))

(defn prep-pipeline [data]
  (let [non-numeric (map #(select-keys % [:ocean_proximity]) data )
    ;    _ (prn "nm: " (drop 5000 non-numeric))
    ]
    (as->  (map #(dissoc % :ocean_proximity) data) $
       (ml/convert-strings-to-doubles $)
       (ml/fill-missing-values-with-median $)
       (add-id-field $)
       (add-income-cat $)
       (map #(apply merge %1 %2) $ non-numeric )
       (ml/make-stratified-test-and-train $ 0.2
                                          :stratified-filed :income-cat
                                          :seed 42)
       )))

(defn num-pipeline [data]
  (->  (map #(dissoc % :ocean_proximity) data)
        (ml/add-combined-attributes)
        (ml/standardize)
      ))

(defn cat-pipeline [data]
  (->  (map :ocean_proximity data)
       (ml/make-one-hot)
       ));

(defn combine-pipeline-outputs [num cat]
  (map merge num cat))

(defn ->vectors [data]
  (->> data
       (map #(into (sorted-map) %))
       (map vals )))

(def prepped-data (prep-pipeline (get-data)))
(def train-data (:train-data prepped-data))
(def num1 (num-pipeline train-data))
(def cat1 (cat-pipeline train-data))
(def ready-data-map (combine-pipeline-outputs num1 cat1))

(def outputs (map :median_income ready-data-map))
;(def calc-output (dge 1 16512 outputs))
(def inputs (map #(dissoc % :median_income) ready-data-map))
(def input-vec (->vectors inputs))
;(def calc-input (dge 18 16512 (apply concat input-vec)))

;(def solution (sv (view-sy (mm (trans calc-input) calc-input)) (mm (trans calc-input) calc-output)))
;(def model (s/linear-model inputs outputs :intercept false))

;
;
;
;
;(def  ks [:median_house_value
;          :median_income
;          :total_rooms
;          :housing_median_age
;          :households
;          :total_bedrooms
;          :population
;          :longitude
;          :latitude] )
;
(def family-data
  (incanter.io/read-dataset "resources/all_160_in_51.P35.csv"
                            :header true))
(def housing (i/sel family-data :cols [:HU100]))
(def families (i/sel family-data :cols [:P035001]))
(def families-lm (s/linear-model housing families :intercept false))

