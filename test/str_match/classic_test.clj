(ns str-match.classic-test
  (:use [str-match.classic :as sut] :reload-all)
  (:use [clojure.test]))

(deftest find-z-empty-string-or-collection
  (is (= (find-z "") nil))
  (is (= (find-z nil) nil))
  (is (= (find-z []) nil)))

(deftest find-z-fails-with-invalid-argument
  (is (thrown? IllegalArgumentException (find-z 'symbol)))
  (is (thrown? IllegalArgumentException (find-z :keyword))))

(deftest find-z-single-character
  (is (= (find-z "aaaaaa") [0 5 4 3 2 1]))
  (is (= (find-z "a") [0])))

(deftest find-z-periodic-string
  (is (= (find-z "abcabcabcabc") [0 0 0 9 0 0 6 0 0 3 0 0]))
  (is (= (find-z "abababab") [0 0 6 0 4 0 2 0])))

(deftest find-z-zkp-eq-beta
  (testing "beta string can be extended"
    (is (= (find-z "ab!a ab!aba") [0 0 0 1 0 4 0 0 2 0 1])))
  (testing "beta string cannot be extended"
    (is (= (find-z "ab!a ab!ada") [0 0 0 1 0 4 0 0 1 0 1]))))
  
(deftest find-z-zkp-ne-beta
  (testing "zk-prime < beta"
    (is (= (find-z "ab!ad ab!ada") [0 0 0 1 0 0 5 0 0 1 0 1])))
  (testing "zk-prime > beta"
    (is (= (find-z "ab!ab ab!ada") [0 0 0 2 0 0 4 0 0 1 0 1]))))
  
(deftest find-z-non-periodic-string
  (is (= (find-z "abcabdabcabd") [0 0 0 2 0 0 6 0 0 2 0 0]))
  (is (= (find-z "abc$abcabc") [0 0 0 0 3 0 0 3 0 0])))
  
(deftest find-z-non-string
  (testing "similar to string [ab!a ab!aba ab!aca]"
    (is (= (find-z [1 2 nil 1 'space 1 2 nil 1 2 1 'space 1 2 nil 1 3 1]) 
      [0 0 0 1 0 4 0 0 2 0 1 0 4 0 0 1 0 1])))
  (testing "similar to string [ab!ad ab!ada]"
    (is (= (find-z [1 2 nil 1 3 'space 1 2 nil 1 3 1]) 
      [0 0 0 1 0 0 5 0 0 1 0 1])))
  (testing "similar to string [ab!ab ab!ada]"
    (is (= (find-z [1 2 nil 1 2 'space 1 2 nil 1 3 1]) 
      [0 0 0 2 0 0 4 0 0 1 0 1]))))

(deftest find-reverse-N-reverse-of-z
  (testing "Null cases"
    (are [x] (nil? x)
	 (find-reverse-N nil)
	 (find-reverse-N [])
	 (find-reverse-N "")))
  (testing "As z-value of reverse"
    (is (= (binding [find-z (fn [s] (when (= s [2 1]) :my-result))]
	     (find-reverse-N [1 2])) :my-result))))

(deftest find-L-map-all-cases
  (testing "Null cases"
    (are [x] (nil? x)
	 (@#'sut/find-L-map nil)
	 (@#'sut/find-L-map [])
	 (@#'sut/find-L-map "")))
  (testing "Usual cases"
    (is (= (@#'sut/find-L-map (find-reverse-N "cabdabdab")) {4 5, 7 2}))
    (is (= (@#'sut/find-L-map (find-reverse-N "ab:abab")) {5 4}))
    (is (= (@#'sut/find-L-map (find-reverse-N "dabababab")) {7 2, 5 4, 3 6}))))

(deftest find-L-all-cases
  (testing "Null cases"
    (are [x] (nil? x)
	 (find-L nil)
	 (find-L [])
	 (find-L "")))
  (testing "Usual cases"
    (is (= (find-L (find-reverse-N "cabdabdab")) [0 0 0 0 5 0 0 2 0]))
    (is (= (find-L (find-reverse-N "ab:abab")) [0 0, 0, 0 0, 4 0]))
    (is (= (find-L (find-reverse-N "dabababab")) [0 0 0 6, 0 4 0 2, 0]))))

(deftest find-l-all-cases
  (testing "Null cases"
    (are [x] (nil? x)
	 (find-l nil)
	 (find-l [])
	 (find-l "")))
  (testing "Usual cases"
    (is (= (find-l (find-reverse-N "abcde")) [0 0 0 0 0]))
    (is (= (find-l (find-reverse-N "aaaaa")) [4 4 3 2 1]))
    (is (= (find-l (find-reverse-N "abcabcabc")) [6 6 6 6 3 3 3 0 0]))
    (is (= (find-l (find-reverse-N "abba")) [1 1 1 1])))
  (testing "Source is not a vector"
    (is (= (find-l (list 0 5 0 3 0 1)) [5 5 3 3 1 1]))))

(deftest binary-search-all-cases
  (testing "Null or empty"
    (are [x] (= :non-nil x)
	 (@#'sut/binary-search nil 'a :non-nil)
	 (@#'sut/binary-search [] 'a :non-nil)))
  (testing "Usual cases"
    (is (= (@#'sut/binary-search ['a 'b 'c 'd 'e] 'a :nope) 'a))
    (is (= (@#'sut/binary-search ['a 'b 'c 'd 'e] 'd :nope) 'd))
    (is (= (@#'sut/binary-search ['a 'b 'd 'e] 'c :nope) 'b))
    (is (= (@#'sut/binary-search ['b 'c 'd 'e] 'a :nope) :nope))
    (is (= (@#'sut/binary-search ['a 'b 'c 'd 'e] 'f :nope) 'e))))

(deftest shift-L-all-cases
  (testing "Accuracy"
    (is (= (@#'sut/shift-L [0 0 0 3 0] 5 2) 1))
    (is (= (@#'sut/shift-L [0 0 0 5 0 0 2 0 0] 9 2) 3))
    (is (every? #{0} (map #(@#'sut/shift-L [0 0 0 3 0] 5 %) [0 1 3 4])))))

(deftest shift-bad-char-all-cases
  (testing "Accuracy"
    (is (= (@#'sut/shift-bad-char {\a [0 3 6], \b [1 4 7], \c [2 5 8]} 5 \b) 1))
    (is (= (@#'sut/shift-bad-char {\a [0 1 2]} 0 \e) 1))
    (is (= (@#'sut/shift-bad-char {\a [0 2] \b [1]} 1 \a) 1))
    (is (= (@#'sut/shift-bad-char {\a [0 1 2 3]} 3 \b) 4))))

;;     0 1 2 3 4 5 6 7 8
;;    "a b c a b c a b c"
;; :L [0 0 0 5 0 0 2 0 0],
;; :l [6 6 6 6 3 3 3 0 0]}
;; :char-idx {\a [0 3 6], \b [1 4 7], \c [2 5 8]},
(deftest max-shitft-all-cases
  (testing "Accuracy"
    (is (= (@#'sut/max-shift (bm-index "abcabcabc") 2 \b) 3))
    (is (= (@#'sut/max-shift (bm-index "abcabcabc") 5 \a) 6))
    (is (= (@#'sut/max-shift {:L [0 0 0 2 0] :char-idx {\b [0]} :length 5} 2 \b) 2))
    (is (= (@#'sut/max-shift {:L [0 0 0 2 0] :char-idx {} :length 5} 2 \b) 3))))

(deftest match-shift-all-cases
  (testing "Accuracy"
    (is (= (@#'sut/match-shift 3 [0 0 0]) 3))
    (is (= (@#'sut/match-shift 3 [0 1 0]) 2))
    (is (= (@#'sut/match-shift 1 [1]) 1))))

(deftest bm-match-all-cases
  (testing "Null cases"
    (are [x] (nil? x)
	 (match (bm-index "a") nil)
	 (match (bm-index "a") [])
	 (match (bm-index "a") "")))
  (testing "Accuracy"
    (is (= (match (bm-index "a") "aaaa") [0 1 2 3]))
    (is (= (match (bm-index "abc") "abc") [0]))
    (is (= (match (bm-index "abc") "abcabcabc") [0 3 6]))
    (is (= (match (bm-index "abc") "abc#abc#abc") [0 4 8]))))

;;
;; Knuth-Morris-Pratt method
;;

(deftest find-sp*-all-cases
  (testing "Null cases"
    (are [x] (nil? x)
	 (find-sp* nil)
	 (find-sp* [])))
  (testing "Normal cases"
    (is (= (find-sp* [0 2 0 0]) [0 0 2 0]))
    (is (= (find-sp* [0 0 3 0 0]) [0 0 0 0 3]))
    (is (= (find-sp* [1]) [0]))
    (is (= (find-sp* [0 5 0 3 0 0 0]) [0 0 0 0 0 5 0]))
    (is (= (find-sp* [1 2 0 4 0 0 0]) [0 0 2 0 0 0 4]))))

(deftest find-sp-delegates-to-sp*
  (testing "Delegation"
    (binding
	[find-sp* (fn [param]
		    (cond (= param (find-z "abcabczabc")) 'result
			  (= param nil) 'nil-result
			  :else nil))]
      (is (= (find-sp "abcabczabc") 'result))
      (is (= (find-sp []) 'nil-result)))))

#_(run-all-tests)