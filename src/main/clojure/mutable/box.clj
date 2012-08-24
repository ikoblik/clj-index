(ns mutable.box
  (:import [mutable Box]))

(defn box
  ([]
     (Box.))
  ([value]
     (Box. value)))

(defn get-value [box]
  (when box
    @box))

(defn set-value! [box value]
  (when box
    (.setValue box value)))
