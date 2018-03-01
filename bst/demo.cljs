(ns bst.demo
  (:require [cljs.pprint :refer [pprint]]))

;; helper fns
(defn n-rand-ints
  "Return n random ints <= m"
  [n m]
  (for [x (range n)]
    (rand-int m)))

;; Define a base node data structure.
;; A record is like a map with predefined keys
(defrecord Node [value left right])

(defn insert
  "Insert new value into tree. Passing nil produces a new tree."
  [{:keys [value left right] :as node} value-to-insert]
  (cond
   (nil? node)                (Node. value-to-insert nil nil)
   (< value-to-insert value)  (Node. value (insert left value-to-insert) right)
   (> value-to-insert value)  (Node. value left (insert right value-to-insert))
   :else                      node))

(defn insert-list
  "Insert seq of items into tree."
  [tree lst]
  (reduce insert tree lst))

(defn in-order-walk
  "Return in order sequence of node values from tree"
  ([node] (in-order-walk node '() []))
  ([{:keys [value left right] :as node} [new-node & new-nodes :as nodes] output]
   (cond
     node           (recur left (conj nodes node) output)
     new-node       (recur (:right new-node) new-nodes (conj output (:value new-node)))
     :else          output)))

;; quick demo
(defn -main [& args]
  (let [random-list (n-rand-ints 50 10)
        example-tree (insert-list nil random-list)]
    (println "List: "  random-list)
    (println "Walk: "  (in-order-walk example-tree))
    (println "Tree:")
    (pprint example-tree)))
