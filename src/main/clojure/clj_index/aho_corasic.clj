(in-ns 'clj-index.core)
(require '[clojure.zip :as zip])
(use '[mutable.box])

;;===============================================================
;; Aho-Corasic matching algorithm
;;===============================================================

;;TODO: Storage to children that autopromotes itself to
;;hash map as it grows

(defn- get-child
  "Returns mutable child object for the given key, or null."
  [node key]
  (get-in @node [:children key]))

(defn- get-children
  "Returns key/value storage of all the node's children"
  [node]
  (get @node :children))

(defn- make-child
  "Creates mutable child object"
  []
  (box (array-map)))

(defn- get-or-add!
  "Searches for a child with the given key, if it does not
   exist, creates a new child and updates node's state."
  [node key]
  (let [child (get-child node key)]
    (if child
      child
      (let [child (make-child)]
        (set-value! node
                        (assoc-in @node [:children key] child))
        child))))

(defn- mark-word!
  "Marks given node as an ending of a word"
  [node]
  (set-value! node (assoc @node :stop true)))

(defn- word? [node]
  (get @node :stop))

;;TODO Use Flyweight pattern. Write a function that memoizes
;;every item from (seq word)?
(defn add-word! [node word]
  (when (seq word)
    (letfn [(recur-add [node word]
              (if (seq word)
                (recur (get-or-add! node (first word))
                       (rest word))
                node))]
      (mark-word!
       (recur-add node word)))
    node))

(defn get-skip-link [node]
  (get @node :skip))

(defn set-skip-link!
  "Sets skip link for the given node. Link also specifies
   linked node depth."
  [node skip-node length]
  (set-value! node
              (assoc @node :skip [skip-node length])))

(defn add-skip-links!
  "Walks the tree in breadth first fashion and adds skip link
   to its nodes."
  [root]
  )
