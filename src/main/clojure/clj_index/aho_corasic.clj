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

(defn- get-skip-link [node]
  (when node
    (get @node :skip)))

(defn- get-output-link [node]
  (when node
    (get @node :output)))

;;------------ Link -----------

(defprotocol Link
  (linked-node [this])
  (prefix-length [this]))

(extend-type nil
  Link
  (linked-node [_] nil)
  (prefix-length [_] 0))

(defrecord RecordLink [node prefix-length]
  Link
  (linked-node [_]
    node)
  (prefix-length [_]
    prefix-length))

(defn mk-link [node prefix-length]
  (->RecordLink node prefix-length))

(defn- set-skip-link!
  "Sets skip link for the given node. Link also specifies
   linked node depth."
  [node skip-node length]
  (set-value! node
              (assoc @node :skip (mk-link skip-node length))))

(defn- set-output-link!
  "Sets link to a node marked as final."
  [node output-node length]
  (set-value! node
              (assoc @node :output (mk-link output-node length))))

(defn- skip-seq
  "Builds sequence of outgoing skip links starting from the node."
  [node]
  (when-let [skip-link (get-skip-link node)]
    (cons skip-link
          (lazy-seq (skip-seq (linked-node skip-link))))))

(defn- find-skip-link
  "Follows parent's skip link chain searching for
   a node with a child with next-key.
   Returns the link or nil."
  [root parent-node next-key]
  (let [sseq (skip-seq parent-node)
        link (first
              (filter (fn [link]
                        (get-child (linked-node link) next-key))
                      sseq))]
    (if link
      link
      (when-let [root-child (get-child root next-key)]
        (mk-link root 0)))))


(defn match-prefix
  "Checks if the prefix exists in the tree and returns
   node corresponding to the last element in the prefix."
  [node prefix]
  (reduce (fn [node key]
            (when node ;TODO: should stop when node is null
              (get-child node key)))
          node prefix))

(defn add-links!
  "Walks the tree in breadth first fashion and adds skip link
   to its nodes."
  [root]
  ;;BF - needs queue
  (let [queue (java.util.LinkedList.) ;;mutable queue
        enqueue-children (fn [node]
                           (doseq [child (get-children node)]
                             (.add queue [node child])))]
    ;;Start from second level children as children of root don't have
    ;;skip links.
    (doseq [[_ child] (get-children root)]
      (enqueue-children child))
    (when-not (.isEmpty queue)
      (loop [[parent [key child]] (.removeFirst queue)]
        (when-let [link (find-skip-link root parent key)]
          (let [link-child (get-child (linked-node link) key)
                linked-length (inc (prefix-length link))]

            ;;Skip link
            (set-skip-link! child
                            link-child
                            linked-length)

            ;;Output link
            (cond
             ;;Linked node is a stop node
             (word? link-child) (set-output-link!
                                 child link-child linked-length)
             ;;Linked node has an output link
             (get-output-link link-child) (let [output-link (get-output-link link-child)]
                                            (set-output-link!
                                             child
                                             (linked-node output-link)
                                             (prefix-length (get-output-link link-child)))))))
        (enqueue-children child)
        (when-not (.isEmpty queue)
          (recur (.removeFirst queue)))))))

