(ns day-one
  (:require [clojure.java.io :as io]))

(defn parse-input [file-name]
  (with-open [rdr (io/reader file-name)]
    (doall
      (map #(Integer/parseInt %)
           (line-seq rdr)))))

(defn mass->required-fuel [mass]
  (let [required-fuel (- (Math/floor (/ mass 3))
                         2)]
    (if (pos? required-fuel)
      required-fuel
      0)))

(defn part-one [input]
  (reduce +
          (map mass->required-fuel
               input)))

(defn part-two [input]
  (reduce +
          (mapcat (fn [mass]
                    (->> mass
                         mass->required-fuel
                         (iterate mass->required-fuel)
                         (take-while pos?)))
                  input)))