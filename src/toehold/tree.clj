(ns toehold.tree
  (:require [clojure.zip :as z]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [toehold.core :as c :refer :all]
            [toehold.utils :as u :refer :all]))


(defrecord node [content children])

;; Contentful-node zipper largely borrowed from
;; http://grokbase.com/p/gg/clojure/12ag6cjnch/how-to-represent-trees-for-use-with-zippers
(defn content-zipper
  "vector-zip and seq-zip assume that branch nodes don't have content. This version is like vector-zip, but assumes all nodes can have content."
  [root]
  (z/zipper (comp coll? :children)
            :children
            (fn [nd children]
              (assoc nd :children children))
            root))

(defn branch?
  [node]
  (coll? (:children node)))

(def children :children)

(defn content [loc] (:content (first loc)))

(defn node-str
  "Return the attractively formatted contents of a node"
  [loc]
  (when-let [node (z/node loc)]
    (str "> " (:content node))))

(defn print-tree [loc]
  (when-not (z/end? loc)
    (do (pprint (str (string/join "" (repeat (count (z/path loc)) " "))
                     (node-str loc)))
        (recur (z/next loc)))))

(defn node-count [loc]
  (count (tree-seq branch? children loc)))

(defn leaf-count [loc]
  (count (remove :children (tree-seq branch? children loc))))

(defn node-path "Return the simple path of nodes up to and including this location, including the location"
  [loc]
  (conj (z/path loc) (z/node loc)))

(defn dump
  [root]
  (spit "dump.txt" (with-out-str (print-tree (content-zipper root)))))

(defn duplicates
  [root]
  (->> (tree-seq branch? children root)
       (map :content)
       frequencies
       (filter #(> (val %) 1))
       (map key)))

;; CHALLENGE 2: Write a function to build a tree of all possible games. Explain
;; why or why not it uses content-zipper (above).

;; AR - Aren't zipper for traversing only? The following solution is naive,
;; recursive and does not preserve the stack. The content of the node is
;; basically the vec of moves so far.
;; (node-count (build-tree)) => 549946 (quite a lot)
;; (count (duplicates (build-tree))) => 0

(defn helper
  [moves]
  (let [avl-moves (->> moves
                       c/board-from
                       c/available-moves
                       (map #(conj % (c/cur-player moves))))]
    (if-not (c/full-or-win? moves)
      (->node moves (map #(helper (conj moves %)) avl-moves))
      (->node moves nil))))

(defn build-tree
  []
  (helper []))

;; CHALLENGE 3: Is it possible to rewrite build-tree so that it's significantly
;; more efficient in time and/or space? If so, what strategies do you see for
;; that? Implement one.

;; AR - given that the game is basically symmetrical on both x, y and diagonal,
;; by trimming the unnecessary moves (for instance you can just calculate moves for
;; positions [0 0], [0 1] and [1 1]) you explore the whole solution space but
;; with a reduced branching factor.
;; With the following:
;; (node-count (build-tree*)) => 1233 (still not optimal)
;; (count (duplicates (build-tree*))) => 0

;; AR - I would still need to remove the configurations that are bound to finish in
;; the same way of other previously explored but I have run out of time...

(defn vert-mirror
  [move]
  (update move 1 #(-> % (- 1) - (+ 1))))

(defn hor-mirror
  [move]
  (update move 0 #(-> % (- 1) - (+ 1))))

(defn origin-mirror
  [move]
  [(second move)
   (first move)])

(defn other-diag-mirror
  [move]
  ;; AR - I was so rusty on this!
  (let [x (first move)
        y (second move)
        c (- y x)             ;; for m' = 1 perpendicular to y = -x + 2
        int-x (/ (- 2 c) 2)   ;; intersection coords
        int-y (+ (- int-x) 2)] ;; again from y = -x + 2
    [(- (long (* 2 int-x)) x)
     (- (long (* 2 int-y)) y)]))

(defn reflections
  "Return the set of reflections of a move"
  [move]
  (->> ((juxt hor-mirror vert-mirror origin-mirror other-diag-mirror) move)
       (remove #(= move %))
       (into #{})))

(defn trim-unnecessary-moves
  [moves]
  (into #{}
        (first (reduce (fn [[mvs refls] move]
                         (if-not (contains? refls move)
                           [(conj mvs move)
                            (set/union refls (reflections move))]
                           [mvs refls]))
                       [#{} #{}]
                       moves))))

;; Some sprinkle of AI
(defn compare-move
  [player moves m1 m2]
  (if (win? (conj moves (conj m1 player))) ;; can be improved
    true
    false))

(defn helper**
  [moves]
  (if-not (c/full-or-win? moves)
    (let [player (c/cur-player moves)
          all-moves (->> moves
                         c/board-from
                         c/available-moves
                         (sort (partial compare-move player moves))
                         (into #{}))
          trimmed-moves (->> (trim-unnecessary-moves all-moves)
                             (map #(conj % player)))]
      (->node moves (map #(let [next-moves (conj moves %)]
                            (helper** next-moves))
                         trimmed-moves)))
    (->node moves nil)))

(defn build-tree**
  []
  (helper** []))

;; (untested disclosure)
;; Another good idea would be to have a BitSet for storing at any given node
;; the index of the current move, indexing the vector of moves as below:
;; [0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]
;;   0     1     2     3     4     5     6     7     8

;; This brings very quick testing of abscence/presence of a move and reconstructing
;; the move sequence is trivial:
;; (keep-indexed
;;  (fn [index move]
;;    (if (.get ^BitSet bs index)
;;      move
;;      nil))
;;  [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]])

;; CHALLENGE 4: write code to answer some of the following questions:
;; 1. What percentage of 100000 random games have no win?

(defn nowin-count [root]
  (count (remove c/win? (map :content (tree-seq branch? children root)))))

(defn win% [root]
  (let [final-boards (map :content (remove :children (tree-seq branch? children root)))
        wins (count (filter c/win? final-boards))]
    (/ wins (float (count final-boards)))))

;; (win% (build-tree**)) => 0.8719512195121951
;; AR - the count is off, there must be some case not handled correctly

;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?

;; AR - The one who moves first, in this case X

;; 2. Under what conditions is player 2 (O) guaranteed a win?

;; AR - from childhood, no mathematical proof: if it occupies the center.

;; 2. Can X get a win if they blow 1st move?
