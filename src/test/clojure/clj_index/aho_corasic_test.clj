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
    (do (@#'sut/set-skip-link! b1 b2 123)
        (are [skip-node length node]
             (and (= skip-node (@#'sut/skip-node node))
                  (= length (@#'sut/prefix-length node)))
             b2 123 (@#'sut/get-skip-link b1)))))

(deftest skip-link-seq
  (let [b1 (box)
        b2 (box)
        b3 (box)
        _ (do (@#'sut/set-skip-link! b1 b2 123)
              (@#'sut/set-skip-link! b2 b3 15))
        b1-seq (doall (@#'sut/skip-seq b1))
        b2-seq (doall (@#'sut/skip-seq b2))]
    (is (= 2 (count b1-seq)))
    (is (= 1 (count b2-seq)))
    (are [skip-node length node]
         (and (= skip-node (@#'sut/skip-node node))
              (= length (@#'sut/prefix-length node)))
         b2 123 (first b1-seq)
         b3 15 (second b1-seq)
         b3 15 (first b2-seq))))

(deftest match-prefix-test
  (let [tree (doto (box)
               (@#'sut/add-word! "abcde")
               (@#'sut/add-word! "abcef"))]
    (are [child-keys prefix]
         (= child-keys
            (into #{} (keys (@#'sut/get-children (sut/match-prefix tree prefix)))))
         #{\b} "a"
         #{\d \e} "abc"
         #{\f} "abce"
         #{} "abcef")
    (are [unknown-prefix]
         (nil? (sut/match-prefix tree unknown-prefix))
         "d"
         "abd"
         "abcf"
         "abcdef"
         "bcd")))

(defn get-tree [& words]
  (let [tree (box)]
    (doseq [word words]
      (sut/add-word! tree word))
    tree))

(deftest find-skip-link-test
  ;;a-b-c-d-e
  ;;      |
  ;;  b-c-d-f
  ;;      |
  ;;    c-d-g
  ;;      |
  ;;      d-h
  (let [tree (box)
        add-skip (fn [from to]
                   (@#'sut/set-skip-link!
                    (sut/match-prefix tree from)
                    (sut/match-prefix tree to)
                    (count to)))
        _ (do (doseq [word ["abcde" "bcdf" "cdg" "dh"]]
                (sut/add-word! tree word))
              (doseq [link [["abcd" "bcd"]
                            ["bcd" "cd"]
                            ["cd" "d"]]]
                (apply add-skip link)))]
    (testing "Following skip links from a node for the new key should end-up in
              a node marked as 'to' or nil."
        (are [from key to]
             (= (when to (sut/match-prefix tree to))
                (@#'sut/skip-node
                 (@#'sut/find-skip-link tree (sut/match-prefix tree from) key)))
             "abcd" \g "cd"
             "bcd" \h "d"
             "cd" \f nil))
    (testing "Root is a valid skip link."
      (is (= tree
             (@#'sut/skip-node
              (@#'sut/find-skip-link tree
                                     (sut/match-prefix tree "cd")
                                     \a)))))))

;;a-b-c-d-e
;;      |
;;  b-c-d-f
;;      |
;;    c-d-g
;;      |
;;      d-h
;;      |
;;      d-h-a-b-c
(deftest add-skip-links-test
  (let [tree (get-tree "abcde" "bcdf" "cdg" "dh" "dhabc")
        _ (sut/add-skip-links! tree)]
    (testing "Node in skip-link must equal expected node"
      (are [from-node expected prefix-length]
           (let [link (@#'sut/get-skip-link
                       (sut/match-prefix tree from-node))]
             (= (when expected (sut/match-prefix tree expected))
                (@#'sut/skip-node link))
             (= prefix-length (@#'sut/prefix-length link)))
           "dha" "a" 1
           "dhab" "ab" 2
           "dhabc" "abc" 3
           "bc" "c" 1
           "dh" nil 0
           "abcd" "bcd" 3))))

;;TODO: check invocations with null arguments
