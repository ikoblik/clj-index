(ns mutable.box-test
  (:use [mutable.box] :reload-all)
  (:use [clojure.test]))

;;===============================================================
;; Box unit tests
;;===============================================================

(deftest ctor
  (is (not= nil (box)))
  (is (not= nil (box 'value))))

(deftest dereference
  (is (= nil @(box)))
  (is (= :value @(box :value)))
  (is (= nil (get-value (box))))
  (is (= :value (get-value (box :value)))))

(deftest set-value
  (let [box1 (box)]
    (is (and (= 2 (set-value! box1 2))
             (= 2 @box1)))
    (is (and (= 3 (set-value! box1 3))
             (= 3 @box1)))
    (is (and (nil? (set-value! box1 nil))
             (nil? @box1)))))

(deftest equality
  (testing "Equality of boxes and their hash codes"
    (are [x y] (and (= x y)
                    (= (hash x) (hash y)))
         (box :v1) (box :v1)
         (box) (box)))
  (testing "Inequality of boxes and their hash codes"
    (are [x y] (and (not= x y)
                    (not= (hash x) (hash y)))
         (box) (box 1)
         (box 1) (box)
         (box :v1) (box 1))))

(deftest to-string
  (are [x y] (= x y)
       (str [1 2 3]) (str (box [1 2 3]))
       "My string" (str (box "My string"))
       (str nil) (str (box))))


