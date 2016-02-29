(ns clj-index.core-test
  (:require [clj-index.core :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]))

(def search-pair-gen
  (gen/fmap (fn [[a b c]] [b (str/join [a b c])])
            ;;also fails with gen/string-alphanumeric
            (gen/tuple gen/string gen/string gen/string)))

(defspec bm-test 1000
  (prop/for-all [[i-str s-str] search-pair-gen]
                (let [res (match (bm-index i-str) s-str)
                      i-res (.indexOf s-str i-str)]
                  (if (= 0 i-res)
                    (or (empty? res) (= 0 (first res)))
                    (= (first res) i-res)))))
