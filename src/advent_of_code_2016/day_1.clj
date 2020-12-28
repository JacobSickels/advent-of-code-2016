(ns advent-of-code-2016.day-1
  (:require [advent-of-code-2016.core :as core]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn day-1 []
  (let [data (str/split (first (core/read-file "resources/2016-1.txt")) #", ")]
    (loop [points data
           [x y] [0 0]
           facing \N]
      (if (empty? points)
        (reduce + (map #(Math/abs %) [x y]))
        (let [direction (first points)
              turn (first direction)
              steps (Integer/parseInt (apply str (rest direction)))]
          (cond
            (= facing \N)
            (cond
              (= turn \L) (recur (rest points) [(- x steps) y] \W)
              (= turn \R) (recur (rest points) [(+ x steps) y] \E))
            (= facing \E)
            (cond
              (= turn \L) (recur (rest points) [x (+ y steps)] \N)
              (= turn \R) (recur (rest points) [x (- y steps)] \S))
            (= facing \S)
            (cond
              (= turn \L) (recur (rest points) [(+ x steps) y] \E)
              (= turn \R) (recur (rest points) [(- x steps) y] \W))
            (= facing \W)
            (cond
              (= turn \L) (recur (rest points) [x (- y steps)] \S)
              (= turn \R) (recur (rest points) [x (+ y steps)] \N))))))))


(defn points-between [[x1 y1] [x2 y2]]
  "Manhattan points between two points"
  (set
    (concat
      [[x1 y1]]
      [[x2 y2]]
      (map (fn [x] [x y1]) (apply range (sort [x1 x2])))
      (map (fn [y] [x2 y]) (apply range (sort [y1 y2]))))))

(defn day-1-2 []
  (let [
        ;data (str/split (first (core/read-file "resources/2016-1.txt")) #", ")
        data ["R8" "R4" "R4" "R8"]]
    (loop [points data
           visited #{}
           [x y] [0 0]
           facing \N]
      (println [x y])
      (if (empty? points)
        [x y]
        (if (contains? visited [x y])
          [x y]
          (let [direction (first points)
                turn (first direction)
                steps (Integer/parseInt (apply str (rest direction)))]
            (cond
              (= facing \N)
              (cond
                (= turn \L) (recur (rest points)
                                   (set/union visited (points-between [x y] [(- x steps) y]))
                                   [(- x steps) y]
                                   \W)
                (= turn \R) (recur (rest points)
                                   (set/union visited (points-between [x y] [(+ x steps) y]))
                                   [(+ x steps) y]
                                   \E))
              (= facing \E)
              (cond
                (= turn \L) (recur (rest points)
                                   (set/union visited (points-between [x y] [x (+ y steps)]))
                                   [x (+ y steps)]
                                   \N)
                (= turn \R) (recur (rest points)
                                   (set/union visited (points-between [x y] [x (- y steps)]))
                                   [x (- y steps)]
                                   \S))
              (= facing \S)
              (cond
                (= turn \L) (recur (rest points)
                                   (set/union visited (points-between [x y] [(+ x steps) y]))
                                   [(+ x steps) y]
                                   \E)
                (= turn \R) (recur (rest points)
                                   (set/union visited (points-between [x y] [(- x steps) y]))
                                   [(- x steps) y]
                                   \W))
              (= facing \W)
              (cond
                (= turn \L) (recur (rest points)
                                   (set/union visited (points-between [x y] [x (- y steps)]))
                                   [x (- y steps)]
                                   \S)
                (= turn \R) (recur (rest points)
                                   (set/union visited (points-between [x y] [x (+ y steps)]))
                                   [x (+ y steps)]
                                   \N)))))))))
