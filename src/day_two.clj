(ns day-two
  (:require [clojure.string :as str]))

(defn parse-input [file-name]
  (mapv #(Integer/parseInt %)
        (-> file-name
            slurp
            str/trim
            (str/split #","))))

(defn run-program [program noun verb]
  (let [s (assoc program
            1 noun
            2 verb)
        op-code->operation {1 +
                            2 *}]
    (get (loop [pos 0
                state s]
           (let [[op-code p1 p2 out-register]
                 (try
                   (subvec state pos (+ pos 4))
                   (catch IndexOutOfBoundsException e
                     (subvec state pos)))]
             (if-let [operation (op-code->operation op-code)]
               (recur (+ pos 4)
                      (assoc state
                        out-register (operation (get state p1)
                                                (get state p2))))
               state)))
         0)))

(defn part-one [input]
  (run-program input 12 2))

(defn part-two [input target-output]
  (let [{:keys [noun verb]}
        (first
          (filter #(= (:output %) target-output)
                  (for [noun (range 100)
                        verb (range 100)]
                    {:output (run-program input noun verb)
                     :noun noun
                     :verb verb})))]
    (+ (* 100 noun)
       verb)))
