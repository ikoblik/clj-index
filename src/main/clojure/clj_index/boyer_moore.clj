(in-ns 'clj-index.core)
(import '[java.util Collections])

(defn count-match
  "counts number of matching items from the beginning"
  [str1 str2]
  (or (last (take-while identity
		    (map (fn [c1 c2 i] (if (= c1 c2) i))
			 str1 str2 (iterate inc 1))))
      0))

(defn find-z
  "Returns an array of substring lengths that are propper prefixes of the string.
   Positions in the array correspond to the positions of the first characters
   of these substrings."
  [pattern]
  (when (seq pattern)
    (loop [k 1, zs (transient [0]), l 0, r 0, tail (rest pattern)]
      (cond
       ;;done
       (empty? tail) (persistent! zs)
       ;;outside of the envelope
       (> k r) (let [zk (count-match pattern tail)]
           (recur (inc k) (conj! zs zk) k (+ k zk -1) (rest tail)))
       ;;in the envelope
       :else
       (let [zkp (nth zs (- k l)) betta (- r k -1)]
         (cond
	  ;;zk-prime is less than betta, then zk=zkp
	  (< zkp betta) (recur (inc k) (conj! zs zkp) l r (rest tail))
	  ;;zk-prime is greater than betta, then zk=betta and l=k
	  (> zkp betta) (recur (inc k) (conj! zs betta) k r (rest tail))
	  ;;zkp stopped exactly at the border of zl new character at the end of betta may match
	  :else (let [zk (+ betta (count-match (drop betta pattern) (drop betta tail)))]
		  (recur (inc k) (conj! zs zk) k (+ k zk -1) (rest tail)))))))))

;;===============================================================
;; Matcher protocol
;;===============================================================

(defprotocol Matcher
  (match [this data]))

;;===============================================================
;; Boyer-Moore algorithm
;;===============================================================

(defn find-char-idx
  "Builds a list-multimap of characters and their positions in the pattern"
  [pattern]
  (persistent! (second
		(reduce
		 (fn [[i so-far] ch]
		   [(inc i) (assoc! so-far ch (conj (get so-far ch []) i))])
		 [0 (transient {})]
		 pattern))))

(defn find-reverse-N
  "N(j) is the length of the longest suffix of P[1..j] equal
   to the proper suffix of P"
  [pattern]
  (find-z (reverse pattern)))

(defn- find-L-map
  "Returns the result similar to find-L function but in map format."
  [rev-n-values]
  (when (seq rev-n-values)
    (let [n (count rev-n-values)]
      (persistent!
       (reduce
	(fn [l-val-map cc]
	  (let [n-value (nth rev-n-values cc)]
	    (if (not= 0 n-value)
	      (conj! l-val-map [(- n n-value) (- n cc 1)])
	      l-val-map)))
	(transient {})
	(range (- n 2)))))))

(defn find-L
  ([rev-n-values]
     "Implements good-suffix rule for Boyer-Moore algorithm,
      rev-n-values - is the z-values of the reversed pattern
      returns - L'(i) is the largest position less than n
                such that string P[i..n] is suffix of [1..L'(i)]
      see Gusfield p. 20"
     (when (seq rev-n-values) ;discard empty sequences
       (let [n (count rev-n-values)
	     l-val-map (find-L-map rev-n-values)]
	 (persistent! 
	  (reduce (fn [v i] (conj! v (or (l-val-map i) 0)))
		  (transient [])
		  (range n)))))))

(defn find-l
  "l(i) is the largest suffix of P[i..n] which is also prefix of P.
   NOTE: prefix and suffix may oferlap, i.e. for aaa = [0 2 1]"
  [rev-n-values]
  (when (seq rev-n-values)
    (let [n (count rev-n-values)]
      (into []
	    (reduce
	     (fn [so-far [i rev-n-value]]
	       (cons (min (- n i)
			  (let [last-elem (or (first so-far) 0)]
			    (if (= rev-n-value (- n i))
			      (max last-elem rev-n-value)
			      last-elem)))
		     so-far))
	     '()
	     (map list
		  (range (dec n) -1 -1)
		  (reverse rev-n-values)))))))

(defn- binary-search
  "Returns the searched item or if it's not in the seq its predecessor"
  [coll value default]
  (if (seq coll)
    (let [res (Collections/binarySearch coll value)
	  res (if (< res 0)
		(- -2 res) ;;return previous item's index
		res)	   ;;return found item's index
	  ]
      (get coll res default))
    default))

(defn- shift-L
  "Returns the shift for the pattern using good
   suffix rule due to a mismatch on position idx"
  [L-values n idx]
  (let [L-value (if (> n (inc idx))
		  (L-values (inc idx))
		  0)]
    (if (> L-value 0)
      (- n L-value 1)
      0)))

(defn- shift-bad-char [char-idx idx bad-char]
  (- idx (binary-search (char-idx bad-char) idx -1)))

(defn- max-shift
  "bm-index, idx, length, bad-char"
  [{char-idx :char-idx L :L n :length} idx bad-char]
  (max (shift-L L n idx)
       (shift-bad-char char-idx idx bad-char)))

(defn- match-shift
  "Gives max shift after a match,
   n - the pattern length, l the l-values"
  [n l]
  (if (> n 1)
    (- n (nth l 1))
    1))

(defn drop-indexed
  "Applies predicate to the items of the given collections
   until it returns false. Returns a tuple with
   index of the last check and the values at this postion."
  [pred & collections]
  (drop-while #(apply pred (drop 1 %))
	      (apply map vector
		     (iterate inc 0)
		     collections)))

(defrecord BMIndex [pattern length char-idx L l]
  Matcher
  (match [this data]
	 (let [n length ;;pattern length
	       rpattern (reverse pattern)
	       inner
	       (fn inner [data k];k - right position index+1
		 (lazy-seq
		  (let [chunk (take n data)]
		    (when (>= (count chunk) n)
		      (let [;;failed-on (index from the end), bad-char (the last char)
			    [failed-on _ bad-char] (first (drop-indexed = rpattern (reverse chunk)))
			    ;;compute shift
			    shift (if (nil? failed-on) ;;true means that there was a match
				    (match-shift n l) ;;keep suffix equal to prefix
				    (max-shift this (- n failed-on 1) bad-char))]
			(cons (if (nil? failed-on) (- k n))
			      (inner (drop shift data) (+ k shift))))))))]
	   (when (seq data)
	     (remove nil? (inner data (count L)))))))

(defn bm-index [pattern]
  (let [reverse-n (find-reverse-N pattern)]
    (BMIndex.
     pattern
     (count pattern)
     (find-char-idx pattern)
     (find-L reverse-n)
     (find-l reverse-n))))
