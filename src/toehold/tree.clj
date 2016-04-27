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

(defn leaves
  "Returns the leaves of the tree, warning, it can be big to display."
  [loc]
  (remove :children (tree-seq branch? children loc)))

(defn node-path "Return the simple path of nodes up to and including this location, including the location"
  [loc]
  (conj (z/path loc) (z/node loc)))

(defn dump
  [moves]
  (spit "dump.txt" (doall (string/join "\n" (map str moves)))))

(defn board-str
  [moves]
  (doall (string/join "\n" (board-from moves))))

(defn dump-boards
  [moves-set]
  (spit "dump.txt" (doall (string/join "\n\n" (map board-str moves-set)))))

(defn vert-mirror
  [move]
  (update move 1 #(-> % (- 1) - (+ 1))))

(defn hor-mirror
  [move]
  (update move 0 #(-> % (- 1) - (+ 1))))

(defn main-diag-mirror
  [move]
  (-> move
      (assoc 0 (second move))
      (assoc 1 (first move))))

(defn sec-diag-mirror
  [move]
  ;; AR - I was so rusty on this!
  (let [x (first move)
        y (second move)
        c (- y x)             ;; for m' = 1 perpendicular to y = -x + 2
        int-x (/ (- 2 c) 2)   ;; intersection coords
        int-y (+ (- int-x) 2)] ;; again from y = -x + 2
    (-> move
        (assoc 0 (- (long (* 2 int-x)) x))
        (assoc 1 (- (long (* 2 int-y)) y)))))

(defn reflection-move
  "Return the set of reflections of a single"
  [move]
  (->> ((juxt hor-mirror vert-mirror main-diag-mirror sec-diag-mirror) move)
       (remove #(= move %))
       (into #{})))

(defn reflection-moves
  "Return the reflections of the input moves (a set)"
  [moves]
  (into #{} ((juxt (partial mapv hor-mirror)
                   (partial mapv vert-mirror)
                   (partial mapv main-diag-mirror)
                   (partial mapv sec-diag-mirror)) moves)))

(defn rotation-moves
  "Return the rotations of the input moves (a set)"
  [moves]
  (let [rot-90 (->> moves (mapv main-diag-mirror) (mapv vert-mirror))
        rot-180 (->> moves (mapv hor-mirror) (mapv vert-mirror))
        rot-270 (->> moves (mapv main-diag-mirror) (mapv hor-mirror))]
    #{rot-90 rot-180 rot-270}))

(def roto-reflection-moves (memoize #(set/union (rotation-moves %)
                                                (reflection-moves %))))

;; CHALLENGE 2: Write a function to build a tree of all possible games. Explain
;; why or why not it uses content-zipper (above).

;; AR - The following solution is naive, recursive and does not preserve the
;; stack.

(defn next-moves
  "Given the current sequence of moves, return a seq of a seq of the
  next possible moves to perform."
  [moves]
  (let [player (c/cur-player moves)]
    (->> moves
         c/board-from
         c/available-moves
         (map #(conj % player))
         (map #(conj moves %)))))

(def next-moves-memo (memoize next-moves))

(defn setify-moves
  [moves]
  (into #{} moves))

(def setify-moves-memo setify-moves)

(defn setify-all-moves
  [moves-set]
  (into #{} (map setify-moves-memo moves-set)))

(defn helper
  [terminal-moves moves]
  (if (c/full-or-win? moves)
    (let [terminal-sets (setify-all-moves terminal-moves)]
      (if (and (not (contains? terminal-sets (setify-moves moves)))
               (empty? (set/intersection terminal-sets
                                         (setify-all-moves (roto-reflection-moves moves)))))
        (conj terminal-moves moves)
        terminal-moves))
    (reduce (fn [accs mvs]
              (helper accs mvs))
            terminal-moves
            (next-moves-memo moves))))

(defn build-tree
  []
  (helper #{} []))

;; (def tree (build-tree))
;; (count (filter #(and (c/full? %) (not (c/win? %))) tree)) ;;=> 3 draws
;; (count (filter c/win? tree)) ;;=> 135 wins

;; CHALLENGE 3: Is it possible to rewrite build-tree so that it's significantly
;; more efficient in time and/or space? If so, what strategies do you see for
;; that? Implement one.

;; AR - The solution above is of course not ideal, as the intermediate steps
;; are not taken into consideration and a lot of branching is repeated.








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

(defn nowin-count [terminal-moves]
  (count (remove c/win? terminal-moves)))

(defn win% [terminal-moves]
  (let [wins (count (filter c/win? terminal-moves))]
    (/ wins (float (count  final-boards)))))

;; (win% (build-tree**)) => 0.8719512195121951
;; (win% (build-tree)) => 0.8194130925507901

;; AR - the count is off, there must be some case not handled correctly

;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?

;; AR - The one who moves first, in this case X

;; 2. Under what conditions is player 2 (O) guaranteed a win?

;; AR - from childhood, no mathematical proof: if it occupies the center.

;; 2. Can X get a win if they blow 1st move?
