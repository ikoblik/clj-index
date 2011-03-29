(ns str-match.classic-test
  (:use [str-match.classic] :reload-all)
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
	 (find-L-map nil)
	 (find-L-map [])
	 (find-L-map "")))
  (testing "Usual cases"
    (is (= (find-L-map (find-reverse-N "cabdabdab")) {4 5, 7 2}))
    (is (= (find-L-map (find-reverse-N "ab:abab")) {5 4}))
    (is (= (find-L-map (find-reverse-N "dabababab")) {7 2, 5 4, 3 6}))))

(deftest find-L-all-cases
  (testing "Null cases"
    (are [x] (nil? x)
	 (find-L nil)
	 (find-L [])
	 (find-L "")))
  (testing "Usual cases"
    (is (= (find-L (find-reverse-N "cabdabdab")) [0 0 0 0 5 0 0 2 0]))
    (is (= (find-L (find-reverse-N "ab:abab")) [0 0, 0, 0 0, 5 0]))))

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

#_(run-all-tests)