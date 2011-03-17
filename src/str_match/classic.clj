(ns str-match.classic)

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
  [chain]
  (when (seq chain)
    (loop [k 1, zs (transient [0]), l 0, r 0, tail (rest chain)]
      (cond
       ;;done
       (empty? tail) (persistent! zs)
       ;;outside of the envelope
       (> k r) (let [zk (count-match chain tail)]
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
	  :else (let [zk (+ betta (count-match (drop betta chain) (drop betta tail)))]
		  (recur (inc k) (conj! zs zk) k (+ k zk -1) (rest tail)))))))))


(defn find-L
  ([chain]
     "Implements good-suffix rule for Boyer-Moore algorithm,
      chain is a sequence of elements from a finite set"
     (find-L chain (reverse (find-z (reverse chain)))))
  ([chain n-values]
     "Implements good-suffix rule for Boyer-Moore algorithm,
      chain - is a sequence of elements from a finite set,
      n-values - is a reverse of z-values of the reversed chain
      returns - the L'(i) indices of substrings [1..L'(i)]
                such that their suffixes are equal to [i..n]
      see Gusfield p. 20"
     (when (seq chain) ;discard empty sequences
       (let [n (count chain)
	     n-2 (- n 2)]
	 (loop [l-val-map (transient {}) cc 0]
	   (if (> cc n-2)
	     (persistent! 
	      (reduce (fn [v i] (conj! v (or (l-val-map i) 0)))
		      (transient [])
		      (take n (iterate inc 0))))
	     (recur (conj! l-val-map [(- n (nth n-values cc)) cc]) (inc cc))))))))

(defn find-l
  [n-values]
  (when n-values
    (let [n (count (n-values))]
      (map =
       (map list (iterate inc 0) n-values)))))