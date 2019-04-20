(ns commonmark-clj.core)
(require '[clojure.string :refer [split-lines starts-with? blank?]])

(defn create-node [type]
  {:type type
   :parent nil
   :children []
   :string-value nil})

(defn append-child [parent child]
  (assoc parent :children (conj (:children parent) child)))

(defn is-paragraph [line]
  true)

(defn parse-line [document line]
  (println "handle line")
  (cond
    (is-paragraph line) (append-child document {:type :paragraph :parent document :string-value line})
    :else document))

(defn empty-lines? [line]
  (not (blank? line)))

(defn parse
  "Parses the contents into AST"
  [contents]
  (let [document (create-node :document)
        lines (->> contents (split-lines) (filter empty-lines?))]
    (println "lines" lines)
    (reduce parse-line document lines)))


(defn -main []
  (println "AST: " (parse "hello")))



