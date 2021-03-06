(ns commonmark-clj.node
  (:require [clojure.zip :as zip]))

(defn create-node [type]
  {:type type
   :children []
   :string-value nil
   :closed? false})

(defn node-zip [root]
  (zip/zipper
    (fn [_] true)
    (fn [x] (:children x))
    (fn [x children]
      (assoc x :children children))
    root))



