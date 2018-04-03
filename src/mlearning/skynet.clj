(ns mlearning.skynet
  (:require [jutsu.ai.core :as ai]))

;;Example configuration of a simple multilayer feedforward neural network architecture
;;classifying the iris dataset.
(def iris-net-config
	[:layers 
		[[:dense [:n-in 4 :n-out 10 :activation :relu]]
	 	 [:dense [:n-in 10 :n-out 10 :activation :relu]]
	 	 [:output :negative-log-likelihood [:n-in 10 :n-out 3 :activation :softmax]]]])

(def iris-net
	(-> iris-net-config
		ai/network
		ai/initialize-net))

(defn iris-train []
  (let [iris-iterator (ai/classification-csv-iterator "iris.csv" 150 4 3)]
    (-> iris-net
	(ai/train-net! 200 iris-iterator)
      (ai/save-model "iris-model"))
      (.reset iris-iterator)
        (println (ai/evaluate iris-net iris-iterator))))

(iris-train)
