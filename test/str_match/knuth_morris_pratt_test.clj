(ns str-match.knuth-morris-pratt-test
  (:use [str-match.core :as sut] :reload-all)
  (:use [clojure.test]))

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

(deftest kmp-match-all-cases
  (testing "Null cases"
    (are [x] (nil? x)
	 (match (kmp-index "a") nil)
	 (match (kmp-index "a") [])
	 (match (kmp-index "a") "")))
  (testing "Accuracy"
    (is (= (match (kmp-index "a") "aaaa") [0 1 2 3]))
    (is (= (match (kmp-index "aa") "aaaaaa") [0 1 2 3 4]))
    (is (= (match (kmp-index "abc") "abc") [0]))
    (is (= (match (kmp-index "abc") "abcabcabc") [0 3 6]))
    (is (= (match (kmp-index "abc") "abc#abc#abc") [0 4 8]))))

#_(run-all-tests)