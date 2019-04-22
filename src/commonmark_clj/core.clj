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

(defn root-loc [document]
  (node/node-zip (zip/root document)))

(defn append-paragraph [document line]
  (if (= (:type (zip/node (last-node document))) :p)
    (let [prev-p (last-node document)
          node (zip/node (last-node document))]
      (zip/replace prev-p (assoc node :string-value (str (:string-value node) "\n" line))))
    (zip/append-child (root-loc document) {:type :p :string-value line})))

(defn is-h1 [line]
  (starts-with? line "# "))

(defn append-header-h1 [document line]
  (zip/append-child (root-loc document) {:type :h1 :string-value line}))

(defn parse-line [document line]
  (cond
    (is-h1 line) (append-header-h1 document line)
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

(defn render-element [acc el]
  (cond
    (= (:type el) :p) (str acc "<p>" (:string-value el) "</p>")
    (= (:type el) :h1) (str acc "<h1>" (:string-value el) "</h1>")
    :else acc))

(defn render-html [ast]
  (let []
    (reduce render-element "" (:children ast))))


(defn -main []
  (println "AST: " (parse "hello\nworld!\n# HEADER!\n"))
  (println "HTML: " (-> "hello\nworld!\n# HEADER!\n" parse render-html)))



