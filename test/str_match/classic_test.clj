(ns str-match.classic-test
  (:use [str-match.classic] :reload-all)
  (:use [clojure.test]))

(deftest empty-string-or-collection
  (is (= (find-z "") nil))
  (is (= (find-z nil) nil))
  (is (= (find-z []) nil)))

(deftest fails-with-invalid-argument
  (is (thrown? IllegalArgumentException (find-z 'symbol)))
  (is (thrown? IllegalArgumentException (find-z :keyword))))

(deftest single-character
  (is (= (find-z "aaaaaa") [0 5 4 3 2 1]))
  (is (= (find-z "a") [0])))

(deftest periodic-string
  (is (= (find-z "abcabcabcabc") [0 0 0 9 0 0 6 0 0 3 0 0]))
  (is (= (find-z "abababab") [0 0 6 0 4 0 2 0])))

(deftest zkp-eq-beta
  (testing "beta string can be extended"
    (is (= (find-z "ab!a ab!aba") [0 0 0 1 0 4 0 0 2 0 1])))
  (testing "beta string cannot be extended"
    (is (= (find-z "ab!a ab!ada") [0 0 0 1 0 4 0 0 1 0 1]))))
  
(deftest zkp-ne-beta
  (testing "zk-prime < beta"
    (is (= (find-z "ab!ad ab!ada") [0 0 0 1 0 0 5 0 0 1 0 1])))
  (testing "zk-prime > beta"
    (is (= (find-z "ab!ab ab!ada") [0 0 0 2 0 0 4 0 0 1 0 1]))))
  
(deftest non-periodic-string
  (is (= (find-z "abcabdabcabd") [0 0 0 2 0 0 6 0 0 2 0 0]))
  (is (= (find-z "abc$abcabc") [0 0 0 0 3 0 0 3 0 0])))
  
(deftest non-string
  (testing "similar to string [ab!a ab!aba ab!aca]"
    (is (= (find-z [1 2 nil 1 'space 1 2 nil 1 2 1 'space 1 2 nil 1 3 1]) 
      [0 0 0 1 0 4 0 0 2 0 1 0 4 0 0 1 0 1])))
  (testing "similar to string [ab!ad ab!ada]"
    (is (= (find-z [1 2 nil 1 3 'space 1 2 nil 1 3 1]) 
      [0 0 0 1 0 0 5 0 0 1 0 1])))
  (testing "similar to string [ab!ab ab!ada]"
    (is (= (find-z [1 2 nil 1 2 'space 1 2 nil 1 3 1]) 
      [0 0 0 2 0 0 4 0 0 1 0 1]))))