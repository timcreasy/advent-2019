(ns day-four)

(defn puzzle-input->range [input]
  (let [[_ low high] (re-matches #"([\d]+)-([\d]+)"
                                 input)]
    (range (Integer/parseInt low)
           (inc (Integer/parseInt high)))))

(defn valid-password? [pw]
  (let [s (str pw)]
    (and (= (count s) 6)
         (some (partial apply =)
               (partition 2 1 s))
         (apply <= (map #(Integer/parseInt %)
                        (map str s))))))

(defn part-one [input]
  (count (filter valid-password?
                 (puzzle-input->range input))))

(part-one "124075-580769")