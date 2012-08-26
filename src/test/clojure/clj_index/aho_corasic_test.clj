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
             (and (= skip-node (@#'sut/linked-node node))
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
         (and (= skip-node (@#'sut/linked-node node))
              (= length (@#'sut/prefix-length node)))
         b2 123 (first b1-seq)
         b3 15 (second b1-seq)
         b3 15 (first b2-seq))))

(deftest add-word-test
  (testing "Max length is mantained"
    (are [added-words max-length]
         (= max-length
            (let [tree (box)]
              (doseq [word added-words]
                (@#'sut/add-word! tree word))
              (@#'sut/get-max-length tree)))
         [] 0
         ["a"] 1
         ["ab" "a"] 2
         ["bcd" "afg" "fa" "dc"] 3
         ["a" "bc" "dfe"] 3)))

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

(defn get-linked-tree [& words]
  (doto (apply get-tree words)
    (sut/add-links!)))

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
                (@#'sut/linked-node
                 (@#'sut/find-skip-link tree (sut/match-prefix tree from) key)))
             "abcd" \g "cd"
             "bcd" \h "d"
             "cd" \f nil))
    (testing "Root is a valid skip link."
      (is (= tree
             (@#'sut/linked-node
              (@#'sut/find-skip-link tree
                                     (sut/match-prefix tree "cd")
                                     \a)))))
    (testing "No skip link to non-matching node that has no match at root"
      (is (nil?
           (@#'sut/find-skip-link tree
                                  (sut/match-prefix tree "cd")
                                  \f))))))

;;a-b-c-d-e
;;      |
;;  b-c-d-f
;;      |
;;    c-d-g
;;      |
;;      d-h
;;      |
;;      d-h-a-b-c
(deftest add-links-test
    (testing "Node in skip-link must equal expected node"
      (let [tree (get-linked-tree "abcde" "bcdf" "cdg" "dh" "dhabc" "cd" "aaa")]
        (are [from-node expected prefix-length]
             (let [link (@#'sut/get-skip-link
                         (sut/match-prefix tree from-node))]
               (= (when expected (sut/match-prefix tree expected))
                  (@#'sut/linked-node link))
               (= prefix-length (@#'sut/prefix-length link)))
             "dha" "a" 1
             "dhab" "ab" 2
             "dhabc" "abc" 3
             "bc" "c" 1
             "dh" nil 0
             "abcd" "bcd" 3
             "aa" "a" 1
             "aaa" "aa" 2
             )))
    (testing "Node in output link must equal expected node"
      (let [tree (get-linked-tree "abcde" "bcdf" "cdg" "dh" "dhabc" "cd" "aaa" "aa" "aaaa")]
        (are [from-node expected prefix-length]
             (let [link (@#'sut/get-output-link
                         (sut/match-prefix tree from-node))
                   expected-node (when expected (sut/match-prefix tree expected))
                   linked-node (@#'sut/linked-node link)
                   linked-length (@#'sut/prefix-length link)]
               (= expected-node linked-node)
               (= prefix-length linked-length))
             "abcd" "cd" 2
             "dh" nil 0
             "aaa" "aa" 2
             "aaaa" "aaa" 3
             ))))

;;TODO: check invocations with null arguments

(deftest match-seq-test
  (testing "Simple non-chained match"
      (let [tree (get-linked-tree "cattag" "catt")]
        (is (= [[0 3]] (@#'sut/match-seq (sut/match-prefix tree "catt")
                                         3)))
        (is (empty? (@#'sut/match-seq (sut/match-prefix tree "cat")
                                      2)))))
  (testing "Single chain of output links"
    (let [tree (get-linked-tree "cattag" "catt" "att" "tt" "t" "attag")]
      (is (= [[0 2] [1 2] [2 2]] (@#'sut/match-seq (sut/match-prefix tree "att")
                                                   2)))
      (is (= [[0 3] [1 3] [2 3] [3 3]] (@#'sut/match-seq (sut/match-prefix tree "catt")
                                                         3)))
      (is (= [[0 4]] (@#'sut/match-seq (sut/match-prefix tree "attag")
                                       4)))))
  (testing "Chain that starts with no match"
    (let [tree (get-linked-tree "cattag" "att" "tt" "t" "attag")]
      (is (= [[1 3] [2 3] [3 3]] (@#'sut/match-seq (sut/match-prefix tree "catt")
                                                   3))))))

(deftest ac-index-match-test
  (is (= [[0 0] [1 1] [2 2] [3 3]] (match (sut/ac-index ["a"])
                                          "aaaa")))
  (is (= [[0 3] [2 3] [5 6] [7 10] [13 16]] (match (sut/ac-index ["this" "is" "test"])
                                                   "this istest, test")))
  (is (= [[0 2] [1 3]] (match (sut/ac-index ["abc" "dab"])
                              "dabcdef")))
  (testing "Overlapping patterns"
    (let [ranges [(range 3 8)
                  (range 4 11)
                  (range 5 10)
                  (range 7 11)
                  (range 7 12)
                  (range 8 11)
                  (range 8 14)]
          expected (map (juxt first last) ranges)]
      (is (= (into #{} expected)
             (into #{} (match (sut/ac-index ranges)
                              (range 0 20))))))))