(ns toehold.tree-test
  (:require [toehold.tree :as ttree :refer :all]
            [toehold.core :as c :refer :all]
            [clojure.test :as t :refer :all]
            [clojure.zip :as z])
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

(deftest dups
  (is (empty? (duplicates n)))
  (is (empty? (duplicates n1)))
  (is (= 2 (count (duplicates dups-mock)))))

(deftest mirrors
  ;; not all the cases here
  (is (= [0 2] (vert-mirror [0 0])))
  (is (= [0 1] (vert-mirror [0 1])))
  (is (= [0 0] (vert-mirror [0 2])))
  (is (= [2 0] (vert-mirror [2 2])))
  (is (= [1 1] (vert-mirror [1 1])))

  (is (= [2 0] (hor-mirror [0 0])))
  (is (= [1 0] (hor-mirror [1 0])))
  (is (= [0 0] (hor-mirror [2 0])))
  (is (= [1 1] (hor-mirror [1 1])))

  (is (= [1 1] (origin-mirror [1 1])))
  (is (= [2 2] (origin-mirror [2 2])))
  (is (= [0 2] (origin-mirror [2 0])))
  (is (= [1 0] (origin-mirror [0 1])))
  (is (= [1 2] (origin-mirror [2 1])))
  (is (= [2 0] (origin-mirror [0 2])))
  (is (= [0 1] (origin-mirror [1 0])))
  (is (= [2 1] (origin-mirror [1 2])))

  (is (= [1 1] (other-diag-mirror [1 1])))
  (is (= [0 1] (other-diag-mirror [1 2])))
  (is (= [1 2] (other-diag-mirror [0 1])))
  (is (= [2 1] (other-diag-mirror [1 0])))
  (is (= [1 0] (other-diag-mirror [2 1])))
  (is (= [0 0] (other-diag-mirror [2 2]))))
