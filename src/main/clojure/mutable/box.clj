(ns mutable.box
  (:import [mutable Box]))

(defn box
  (^Box []
        (Box.))
  (^Box [value]
        (Box. value)))

(defn get-value [^Box box]
  (when box
    @box))

(defn set-value! [^Box box value]
  (when box
    (.setValue box value)))
