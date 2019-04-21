(ns commonmark-clj.core)
(require '[clojure.string :refer [split-lines starts-with? blank?]])
(require '[commonmark_clj.node :as node])
(require '[clojure.zip :as zip])

(defn is-paragraph [_line]
  true)

(defn last-node [document]
  (last
    (take-while (complement zip/end?)
      (iterate zip/next document))))

(defn append-paragraph [document line]
  (if (= (:type (zip/node (last-node document))) :p)
    (let [prev-p (last-node document)
          node (zip/node (last-node document))]
      (zip/replace prev-p (assoc node :string-value (str (:string-value node) "\n" line))))
    (zip/append-child document {:type :p :string-value line})))

(defn parse-line [document line]
  (cond
    (is-paragraph line) (append-paragraph document line)
    :else document))

(defn empty-lines? [line]
  (not (blank? line)))

(defn parse
  "Parses the contents into AST."
  [contents]
  (let [document-zipper (node/node-zip (node/create-node :document))
        lines (->> contents (split-lines) (filter empty-lines?))]
    (println "lines" lines)
    (zip/root (reduce parse-line document-zipper lines))))

(defn -main []
  (println "AST: " (parse "hello\nworld!")))



