(in-ns 'clj-index.core)
(require '[clojure.zip :as zip])
(use '[mutable.box])

;;===============================================================
;; Aho-Corasic matching algorithm
;;===============================================================

;;TODO: Storage to children that autopromotes itself to
;;hash map as it grows

(defn- get-children
  "Returns key/value storage of all the node's children"
  [node]
  (get (get-value node) :children))

(defn- get-child
  "Returns mutable child object for the given key, or null."
  [node key]
  (get (get-children node) key))

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
                    (assoc-in (get-value node) [:children key] child))
        child))))

(defn- mark-word!
  "Marks given node as an ending of a word"
  [node]
  (set-value! node (assoc (get-value node) :stop true)))

(defn- word? [node]
  (get (get-value node) :stop))

(defn- get-max-length
  "Returns length of the longest index sequence."
  [root]
  (:max-length (get-value root) 0))

(defn- set-max-length!
  "Must be called on root node with length of added word,
   it's needed to keep track of longest sequence in the dictionary."
  [root length]
  (if (< (get-max-length root) length)
    (set-value! root
                (assoc (get-value root) :max-length length))))

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
    (set-max-length! node (count word))))

(defn- get-skip-link [node]
  (get (get-value node) :skip))

(defn- get-output-link [node]
  (when node
    (get (get-value node) :output)))

;;------------ Link -----------

(defprotocol Link
  (linked-node [this])
  (prefix-length [this]))

(extend-type nil
  Link
  (linked-node [_] nil)
  (prefix-length [_] 0))

(deftype RecordLink [node prefix-length]
  Link
  (linked-node [_]
    node)
  (prefix-length [_]
    prefix-length)
  Object
  (toString [this]
    (str "{length:" prefix-length "}")))

(defn mk-link [node prefix-length]
  (->RecordLink node prefix-length))

(defn- set-skip-link!
  "Sets skip link for the given node. Link also specifies
   linked node depth."
  [node skip-node length]
  (set-value! node
              (assoc (get-value node) :skip (mk-link skip-node length))))

(defn- set-output-link!
  "Sets link to a node marked as final."
  [node output-node length]
  (set-value! node
              (assoc (get-value node) :output (mk-link output-node length))))

(defn- link-seq
  "Builds sequence out of linked nodes, where link is obtain by calling
   (get-child-fn node)."
  [get-child-fn node]
  (letfn [(inner [node]
            (when-let [link (get-child-fn node)]
              (cons link
                    (lazy-seq (inner (linked-node link))))))]
    (inner node)))

(defn- skip-seq
  "Builds sequence of outgoing skip links starting from the node."
  [node]
  (link-seq get-skip-link node))

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

(defn- output-seq [node]
  (link-seq get-output-link node))

(defn- match-seq*
  "Returns sequence of matches following output links from the node."
  [link index-from-root]
  (map (fn [link]
         (let [match-length (prefix-length link)]
           [(- index-from-root match-length -1) index-from-root]))
       (output-seq link)))

(defn- match-seq
  "Returns all possible matches at the given node."
  [node index-from-root]
  (if (word? node)
    (cons [0 index-from-root]
          (match-seq* node index-from-root))
    (match-seq* node index-from-root)))

(defn- follow-skip-link
  "For the given key returns link to a node reachable
   from the given node's skip links or from root."
  [root node key]
  (let [found-link
        (find-skip-link root node key)]
    (if found-link ;;non nil only if there path with key from linked node
      (mk-link (get-child (linked-node found-link) key)
               (inc (prefix-length found-link)))
      (mk-link root 0))))

(defrecord ACIndex [tree max-length]
  Matcher
  (match [this data]
    (let [context-length (get-max-length tree) ;will need it to report matches
          inner (fn inner [data ;moving reference to current item
                           data-idx ;index of the current item
                           node ;moving reference to current node in index trie
                           root     ;reference to the data item that would be child of the
                                        ;root in index trie in the sequence [root-idx, data-idx]
                           root-idx ;index of the first item in history within original data
                           ]
                  (when (seq data)
                    (let [next-item (first data)
                          ;;A. Find appropriate node for next-item
                          child (get-child node next-item)
                          skip-child (when-not child
                                       ;;it should walk from skip link to next node
                                       (follow-skip-link tree child next-item))
                          new-root-idx (if child
                                         root-idx
                                         (- data-idx (prefix-length skip-child) -1))
                          new-root (if child
                                     root
                                     (drop (- new-root-idx root-idx) root))
                          new-node (or child
                                       (linked-node skip-child))
                          matches (match-seq new-node
                                             (- data-idx new-root-idx))]
                      (lazy-seq
                       (if (seq matches)
                         (concat
                          (map(fn [ends] (map (partial + new-root-idx) ends))
                              matches)
                          (inner (rest data)
                                 (inc data-idx)
                                 new-node
                                 new-root
                                 new-root-idx))
                         (inner (rest data)
                                (inc data-idx)
                                new-node
                                new-root
                                new-root-idx))))))]
      (inner data 0 tree data 0))))

(defn ac-index [patterns]
  (let [tree (box)]
    (doseq [pattern patterns]
      (add-word! tree pattern))
    (add-links! tree)
    (->ACIndex tree (get-max-length tree))))
