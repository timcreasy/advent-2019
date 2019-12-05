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

(defn valid-password?' [pw]
  (let [s (str pw)
        digits (map str s)]
    (and (= (count s) 6)
         (some (partial apply =)
               (partition 2 1 s))
         (apply <= (map #(Integer/parseInt %)
                        digits))
         (some (fn [[k v]]
                 (= v 2))
               (frequencies digits)))))

(defn part-one [input]
  (count (filter valid-password?
                 (puzzle-input->range input))))

(defn part-two [input]
  (count (filter valid-password?'
                   (puzzle-input->range input))))

;(part-one "124075-580769")

;(part-two "124075-580769")