(ns clj-index.aho-corasic-test
  (:use [clj-index.core :as sut] :reload-all)
  (:use [clojure.test]
        [mutable.box]))

;;===============================================================
;; Aho-Corasic unit tests
;;===============================================================

(deftest get-or-add
  (testing "Intialization"
    (let [b (box)]
      (is (= (@#'sut/make-child)
             (@#'sut/get-or-add! b \a))
          "New child")
      (is (= (@#'sut/make-child)
             (@#'sut/get-child b \a))
          "Original box updated"))))

(deftest get-children
  (let [b (doto (box)
            (@#'sut/get-or-add! \a)
            (@#'sut/get-or-add! \b)
            (@#'sut/get-or-add! \c))]
    (is (= #{\a \b \c}
           (into #{}
                 (keys (@#'sut/get-children b))))
        "All originally added keys")
    (doseq [[k v] (@#'sut/get-children b)]
      (is (= (@#'sut/make-child)
             v)
          (str "New child node for key " k)))))

(deftest word-marks
  (let [b (box)]
    (is (not (@#'sut/word? b)))
    (do (@#'sut/mark-word! b)
        (is (@#'sut/word? b)))))

(deftest skip-links
  (let [b1 (box)
        b2 (box)]
    (are [b] (nil? (@#'sut/get-skip-link b))
         b1
         b2)
    (do (@#'sut/set-skip-link! b1 b2 1)
        (is (= [b2 1]
               (@#'sut/get-skip-link b1))))))