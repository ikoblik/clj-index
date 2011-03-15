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
