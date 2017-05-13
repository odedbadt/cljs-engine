(ns cljs-engine.graph
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn duplicate-and-transform-graph [graph transformation]
  (let [
    l (count graph)
    offset (inc (- (reduce max (keys graph)) (reduce min (keys graph))))
    to-offset-id (fn [x] (if x (+ x offset)))
    ]
  (into graph
      (for [[id [formula cyclic neighbors]] graph]
        [(to-offset-id id) [(str transformation formula) cyclic (vec (map to-offset-id neighbors))]])
    )))

(defn apply-word-rules [word [[from to] & rest-of-rules :as rule-book]]
  (if (and rule-book from (not (empty? word)))
    (if (string/starts-with? word from)
      (apply-word-rules (string/replace word from to) rule-book)
      (apply-word-rules word rest-of-rules)
    )
    word
  )
)

(defn flatten-equivalence-table [equivalence-table]
  (loop [equivalence-table equivalence-table]
    (let [
      mapped-entries (for [[k v] equivalence-table]
               [(contains? equivalence-table v) k (or (get equivalence-table v) v)])]
      (if (some first mapped-entries)
        (recur (into {} (for [[a b c] mapped-entries] [b c])))
        equivalence-table))))

(defn pair-induction [[a b]]
  (cond
    (and (not a) (not b)) [nil nil]
    (not a) [b b]
    (not b) [a a]
    :else [a b]))

#?(:clj
   (defn -exception [msg] (Exception. msg)))

#?(:cljs
   (defn -exception [msg] msg))

(defn transform-and-reduce-graph [graph-structure transformation rule-book]
  (let [
    new-graph-structure (duplicate-and-transform-graph graph-structure transformation)
    keyed-common-formula-groups (group-by
       (fn [id]
          (let [[formula cyclic neighbors] (get new-graph-structure id)]
            (apply-word-rules formula rule-book)))
       (sort > (keys new-graph-structure)))
    common-formula-groups (vals keyed-common-formula-groups)
    common-formula-pairs (map (comp vec (partial sort >)) (filter (comp (partial = 2) count) common-formula-groups))
    equivalence-table (into {}  common-formula-pairs)
    ]
    (when (some (partial < 2) (map count common-formula-groups))
      (throw (-exception "Group larger than 2")))
    [new-graph-structure equivalence-table]))

#?(:clj
   (defn -index-of [l x] (.indexOf l x)))

#?(:cljs
   (defn -index-of [l x] (.indexOf (apply array l) x)))

(defn common-cycle [[cyclic1 list1] [cyclic2 list2]]
  (if (and cyclic1 cyclic2 (= (count list1) (count list2)))
    (let [l (count list1)
          common-members (set/intersection (set list1)
                                           (set list2))
          first-common-member (first (filter identity common-members))
          offset1 (-index-of list1 first-common-member)
          offset2 (-index-of list2 first-common-member)
          candidate (map
           (fn [idx]
             (pair-induction [(get list1 (mod (+ idx offset1) l))
                              (get list2 (mod (+ idx offset2) l))]))
           (range l))
          ]
      (if (and
            first-common-member
            (every?
              (fn [[a b]]
                (or (= a b)
                  (not (contains? common-members a))
                  (not (contains? common-members b))))
                candidate))
          [true candidate]))))

(defn -get-member-or-index [coll idx] (or (coll idx) idx))

(defn -replace-with-equivalent [graph-structure equivalence-table]
  (into {} (for [[idx [formula cyclic neighbors]] graph-structure]
                 [idx
                 [formula cyclic (vec (map (partial -get-member-or-index equivalence-table) neighbors)) ]])))

(defn merge-vertices [[graph-structure equivalence-table :as graph] [src dst]]
  (if (or (= src dst)
          (not (and (contains? graph-structure src) (contains? graph-structure dst))))
    graph
    (let [
       [f1 cyclic1 neighbors1] (get graph-structure src)
       [f2 cyclic2 neighbors2] (get graph-structure dst)
       equivalnce-mapper (partial map (partial -get-member-or-index equivalence-table))
       [cyclic equivalent-neigbors-dups :as cc] (common-cycle [cyclic1 (vec (equivalnce-mapper neighbors1))]
                                                  [cyclic2 (vec (equivalnce-mapper neighbors2))])
       equivalent-neigbors (vec (filter (fn [[a b]] (and (not= a b) (or a b)))
                                        equivalent-neigbors-dups))
       extended-equivalence-table (merge equivalence-table (into {} (filter (partial apply not=)
                                                                             equivalent-neigbors))
                                                            [src dst])
       [new-graph-structure returned-equivalence-table] (reduce merge-vertices
                                                                [graph-structure extended-equivalence-table]
                                                                equivalent-neigbors)
       returned-graph-structure

         (assoc
           (dissoc (-replace-with-equivalent new-graph-structure returned-equivalence-table) src)
           dst
           [f2 cyclic (vec (map second equivalent-neigbors-dups))])
       ]
    (if (nil? cc)
      graph
      [returned-graph-structure (dissoc returned-equivalence-table nil)]))))

(defn transform-reduce-and-merge-graph [rule-book graph-structure transformation]
  (let [
    [new-graph-structure equivalence-table] (transform-and-reduce-graph graph-structure transformation rule-book)
    flat-equivalence-table (flatten-equivalence-table equivalence-table)
    [new-graph-structure _] (reduce merge-vertices
                                    [new-graph-structure equivalence-table]
                                    flat-equivalence-table)]
    new-graph-structure)
  )

(defn process-graph [transformations graph-structure rule-book]
  (reduce (partial transform-reduce-and-merge-graph rule-book) graph-structure transformations))