(ns mlearning.housing
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [vizard.core :refer :all]
            [vizard.lite :as lite]
            [mlearning.core :as ml]))

(defn get-data  []
  (with-open [reader (io/reader "resources/housing.csv")]
          (->> (csv/read-csv reader)
               ml/csv-data->maps
        ;       (take 50)
               (doall))))

(defn add-income-cat [data]
  (map #(let [i (Math/ceil (/ (:median_income %) 1.5))
              v (if (> i 5) 5 (int i))]
          (assoc % :income-cat v)) data))

(defn add-id-field [data]
  (map #(assoc % :id (int (+ (* (:longitude %) 1000) (:latitude %)))) data));

(def quantifiable-housing-data (map #(dissoc % :ocean_proximity) (get-data)))

(defonce data (get-data))

(def numeric-housing-data (ml/convert-strings-to-doubles data :ignored-fields [:ocean_proximity]))

(def all-housing-data-no-nulls (ml/fill-missing-values-with-median numeric-housing-data :ignored-fields [:ocean_proximity]))

(def all-data-with-id (add-id-field all-housing-data-no-nulls))

(def all-data-with-income-cat (add-income-cat all-data-with-id))

(def stratified-data (ml/make-stratified-test-and-train all-data-with-income-cat :income-cat 0.2 42))

(def training-data (:train-data stratified-data))

(def train-housing-labels (map :median_house_value training-data ))

(def train-input-data (map #(dissoc % :median_house_value) training-data))
;
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





(def long-lat-scatter-plot
  (let [long-lat-data (map #(into {} [[:median_house_value (:median_house_value %) ]
                                      [:population (:population %) ]
                                      [:x (:longitude %) ]
                                      [:y (:latitude %) ]]) training-data)]
    (p! (lite/lite {:mark "point"
                    :width 700
                    :height 700
                    :encoding {:x {:field "x"
                                    :scale {:domain [-125 -113]}
                                    :type "quantitative"}
                               :y {:field "y"
                                    :scale {:domain [32 44]}
                                    :type "quantitative"}
                               :size {:field "population"
                                      :type "quantitative"}
                               :color {:field "median_house_value"
                                       :type "quantitative"
                                       :scale {:scheme "inferno"}}}}
                  long-lat-data))))

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
