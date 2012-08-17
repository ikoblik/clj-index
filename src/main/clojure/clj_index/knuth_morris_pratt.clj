(in-ns 'clj-index.core)

;;===============================================================
;; Knuth-Morris-Pratt algorithm
;;===============================================================

(defn find-sp*
  "Converts Z(i) values to sp(i)."
  [z-values]
  (when (seq z-values)
    (let [length (count z-values)]
      (persistent!
       (reduce (fn [so-far [z-value j]]
		 (assoc! so-far (+ j z-value -1) z-value))
	       (transient (into [] (repeat length 0)))
	       (map list
		    (reverse z-values)
		    (range (dec length) 0 -1)))))))

(defn find-sp
  "Returns sp(i) values for the Knuth-Morris-Pratt method."
  [pattern]
  (find-sp* (find-z pattern)))

(defrecord KMP-Index [pattern length sp-values]
  Matcher
  (match [this data]
	 (let [n-1 (dec length)
	       inner
	       (fn inner [data patt-idx data-idx]
		 ;;always shift data, adapt pattern
		 (lazy-seq
		  (when (seq data)
		    (let [[failed-on _ _] (first (drop-indexed = (drop patt-idx pattern) data))
			  patt-shift (nth sp-values (or failed-on n-1))
			  data-shift (if failed-on (max failed-on 1) (- length patt-shift))]
		      (cons (when (and (nil? failed-on)
                                       ;;make sure that there's enough remaining data for match
                                       (= length (count (take length data))))
                              data-idx)
			    (inner (drop data-shift data) patt-shift (+ data-idx data-shift)))))))]
	   (when (seq data)
	     (remove nil? (inner data 0 0))))))

(defn kmp-index [pattern]
  (KMP-Index. pattern
	     (count pattern)
	     (find-sp pattern)))