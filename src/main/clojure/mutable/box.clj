(ns mutable.box
  (:import [mutable Box]))

(defn box
  ([]
     (Box.))
  ([value]
     (Box. value)))

(defn get-value [box]
  @box)

(defn set-value! [box value]
  (.setValue box value))
