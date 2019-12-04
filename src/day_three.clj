(ns day-three
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-instruction [instruction]
  (let [[_ direction-s length-s] (re-matches #"([RLUD])([\d]+)"
                                             instruction)]
    {:direction (get {"R" :right
                      "L" :left
                      "U" :up
                      "D" :down}
                     direction-s)
     :length (Integer/parseInt length-s)}))

(defn parse-input [file-name]
  (let [[path-one path-two] (-> file-name
                                slurp
                                str/split-lines)]
    {:path-one (map parse-instruction
                    (str/split path-one #","))
     :path-two (map parse-instruction
                    (str/split path-two #","))}))

(defn find-waypoints [start path]
  (reduce (fn [acc instruction]
            (let [last-point (last acc)
                  new-points (case (:direction instruction)
                               :left (for [v (take (:length instruction)
                                                   (rest (range)))]
                                       {:x (- (:x last-point) v)
                                        :y (:y last-point)})
                               :right (for [v (take (:length instruction)
                                                    (rest (range)))]
                                        {:x (+ (:x last-point) v)
                                         :y (:y last-point)})
                               :up (for [v (take (:length instruction)
                                                 (rest (range)))]
                                     {:x (:x last-point)
                                      :y (+ (:y last-point) v)})
                               :down (for [v (take (:length instruction)
                                                   (rest (range)))]
                                       {:x (:x last-point)
                                        :y (- (:y last-point) v)}))]
              (concat acc
                      new-points)))
          [start]
          path))

(defn manhattan-distance [a b]
  (+ (Math/abs (- (:x a)
                  (:x b)))
     (Math/abs (- (:y a)
                  (:y b)))))

(defn part-one [input]
  (let [start {:x 0 :y 0}
        intersections (set/intersection (set (find-waypoints start
                                                             (:path-one input)))
                                        (set (find-waypoints start
                                                             (:path-two input))))]
    (second (sort (map (partial manhattan-distance start)
                       intersections)))))

(defn part-two [input]
  (let [start {:x 0 :y 0}
        path-one-points (apply vector (find-waypoints start
                                                      (:path-one input)))
        path-two-points (apply vector (find-waypoints start
                                                      (:path-two input)))
        intersections (set/intersection (set path-one-points)
                                        (set path-two-points))]
    (second (sort (map (fn [intersection]
                         (+ (.indexOf path-one-points intersection)
                            (.indexOf path-two-points intersection)))
                       intersections)))))

(part-two (parse-input "/Users/timcreasy/Downloads/daythree.txt"))