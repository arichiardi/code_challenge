(ns toehold.tree-test
  (:require [toehold.tree :as ttree :refer :all]
            [toehold.core :as c :refer :all]
            [clojure.test :as t :refer :all]
            [clojure.zip :as z]
            [clojure.set :as set])
  (:import [toehold.tree node]))

(defn v->nodes "Build a simple tree of nodes from a nested vector"
  [vectr]
  (node. (first vectr) (mapv v->nodes (rest vectr))))

(def x1 [17 [3] [14 [23] [11 [8]]]])
(def nodes (v->nodes x1))
(def z1 (content-zipper nodes))

(print-tree z1)
(comment
  "> 17"
  " > 3"
  " > 14"
  "  > 23"
  "  > 11"
  "   > 8"
  )

;; Handy zipper shortcuts for quicker testing:
(def z-l z/left)
(def z-r z/right)
(def z-d z/down)
(def con content)

(deftest basic-zipper-navigation-test
  (are [x y] [= x y]
    3 (-> z1 z-d con)
    8 (-> z1 z-d z-r z-d z-r z-d con)))

(def content-map #(map :content %))

(deftest node-path-test
  (are [x y] [= x y]
    '(17 14 11 8) (-> z1 z-d z-r z-d z-r z-d node-path content-map)))

(def n [[0 0 :x] [0 1 :o]])
(def n1 (let [moves (conj n [2 0 :o])]
          (->node moves
                  [(->node (conj moves [2 1 :o]) nil)])))

(def dups-mock (->node n [n1 n1])) ;; purposely duplicating

(deftest single-move-mirroring
  (is (= [0 2] (vert-mirror [0 0])))
  (is (= [0 1] (vert-mirror [0 1])))
  (is (= [0 0] (vert-mirror [0 2])))
  (is (= [1 2] (vert-mirror [1 0])))
  (is (= [1 1] (vert-mirror [1 1])))
  (is (= [1 0] (vert-mirror [1 2])))
  (is (= [2 2] (vert-mirror [2 0])))
  (is (= [2 1] (vert-mirror [2 1])))
  (is (= [2 0] (vert-mirror [2 2])))

  (is (= [2 0] (hor-mirror [0 0])))
  (is (= [2 1] (hor-mirror [0 1])))
  (is (= [2 2] (hor-mirror [0 2])))
  (is (= [1 0] (hor-mirror [1 0])))
  (is (= [1 1] (hor-mirror [1 1])))
  (is (= [1 2] (hor-mirror [1 2])))
  (is (= [0 0] (hor-mirror [2 0])))
  (is (= [0 1] (hor-mirror [2 1])))
  (is (= [0 2] (hor-mirror [2 2])))

  (is (= [0 0] (main-diag-mirror [0 0])))
  (is (= [1 0] (main-diag-mirror [0 1])))
  (is (= [2 0] (main-diag-mirror [0 2])))
  (is (= [0 1] (main-diag-mirror [1 0])))
  (is (= [1 1] (main-diag-mirror [1 1])))
  (is (= [2 1] (main-diag-mirror [1 2])))
  (is (= [0 2] (main-diag-mirror [2 0])))
  (is (= [1 2] (main-diag-mirror [2 1])))
  (is (= [2 2] (main-diag-mirror [2 2])))

  (is (= [2 2] (sec-diag-mirror [0 0])))
  (is (= [1 2] (sec-diag-mirror [0 1])))
  (is (= [0 2] (sec-diag-mirror [0 2])))
  (is (= [2 1] (sec-diag-mirror [1 0])))
  (is (= [1 1] (sec-diag-mirror [1 1])))
  (is (= [0 1] (sec-diag-mirror [1 2])))
  (is (= [2 0] (sec-diag-mirror [2 0])))
  (is (= [1 0] (sec-diag-mirror [2 1])))
  (is (= [0 0] (sec-diag-mirror [2 2]))))

(deftest moves-mirroring
  (let [fm1 [[0 0 :x] [0 1 :o] [1 0 :x] [1 2 :o] [1 1 :x] [2 1 :o] [0 2 :x] [2 0 :o] [2 2 :x]]
        fm2 [[0 0 :x] [0 1 :o] [1 1 :x] [1 0 :o] [0 2 :x] [2 0 :o] [1 2 :x] [2 1 :o] [2 2 :x]]
        fm-set (hash-set fm1 fm2)
        fm1-hor (mapv hor-mirror fm1)]
    (is (not (empty? (set/intersection fm-set (reflection-moves fm1-hor)))))
    (is (some fm-set (reflection-moves fm1-hor)))))
