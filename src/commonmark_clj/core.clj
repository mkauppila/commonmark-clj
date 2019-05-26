(ns commonmark-clj.core)
(require '[clojure.string :refer [split-lines starts-with? blank? trim]])
(require '[commonmark-clj.node :as node])
(require '[clojure.zip :as zip])
(require '[clojure.data.json :as json])

(defn is-paragraph [_line]
  true)

(defn prev-node [document]
  (last
    (take-while (complement zip/end?)
      (iterate zip/next document))))

(defn root-loc [document]
  (node/node-zip (zip/root document)))

(def not-nil? (complement nil?))

(defn matches-pattern [line pattern]
  (not-nil? (re-matches pattern line)))

(defn append-paragraph [document line]
  (let [pn (zip/node (prev-node document))]
    (if (and
          (not-nil? pn)
          (= (:type pn) :p)
          (= (:closed? pn) false))
      (let [prev-p (prev-node document)]
       (zip/replace prev-p
                    (assoc pn :string-value (str (:string-value pn) "\n" line))))
      (zip/append-child (root-loc document) {:type :p :string-value line :closed? false}))))

(defn is-h1 [line]
  (starts-with? line "# "))

(defn is-h2 [line]
  (starts-with? line "# "))

(defn is-setext-h2 [line pn]
  ;(println "is setext header")
  (and (= line "---") (= (:type pn) :p)))
  ;(matches-pattern line #" {0,3}-*"))

(defn append-header-h1 [document line]
  (zip/append-child (root-loc document) {:type :h1 :string-value line}))

(defn append-header-h2 [document line]
  (zip/append-child (root-loc document) {:type :h2 :string-value line}))

(defn append-setext-header-h2 [document line]
  (let [pn (zip/node (prev-node document))
        prev-p (prev-node document)]
    (zip/replace prev-p (-> pn
                            (assoc :type :h2)))))
                            ;(assoc :string-value (str (:string-value pn) "\n"))))))

(defn is-semantic-break [line]
  (matches-pattern line #" {0,3}(((-|\*|\_) *){3,})"))

(defn append-semantic-break [document line]
  (zip/append-child (root-loc document) {:type :hr :string-value line}))

(defn is-code-block [line]
  (matches-pattern line #" {4}.*"))

(defn append-code-block [document line]
  (if (= (:type (zip/node (prev-node document))) :p)
    (let [prev-p (prev-node document)
          node (zip/node (prev-node document))]
      (zip/replace prev-p (-> node
                              (assoc :string-value (str (:string-value node) "\n" (trim line))))))
    (zip/append-child (root-loc document) {:type :code :string-value (trim line)})))

(defn is-empty-line [line]
  (= line ""))

(defn close-previous-node [document line]
  (let [node (zip/node (prev-node document))]
    (zip/replace (prev-node document) (-> node (assoc :closed? true)))))

(defn parse-line [document line]
  (let [pn (zip/node (prev-node document))]
    (cond
      (is-h1 line) (append-header-h1 document line)
      (is-h2 line) (append-header-h2 document line)
      (is-setext-h2 line pn) (append-setext-header-h2 document line)
      (is-semantic-break line) (append-semantic-break document line)
      (is-code-block line) (append-code-block document line)
      ; new stuff here
      (is-empty-line line) (close-previous-node document line)
      (is-paragraph line) (append-paragraph document line)
      :else document)))

(defn parse
  "Parses the contents into AST."
  [contents]
  (let [document-zipper (node/node-zip (node/create-node :document))
        lines (->> contents (split-lines))]
    (zip/root (reduce parse-line document-zipper lines))))

(defn render-element [acc el]
  (cond
    (= (:type el) :p) (str acc "<p>" (:string-value el) "</p>\n")
    (= (:type el) :h1) (str acc "<h1>" (:string-value el) "</h1>")
    (= (:type el) :h2) (str acc "<h2>" (:string-value el) "</h2>\n")
    (= (:type el) :hr) (str acc "<hr />\n")
    (= (:type el) :code) (str acc "<pre><code>" (:string-value el) "\n</code></pre>\n")
    :else acc))

(defn render-html [ast]
  (reduce render-element "" (:children ast)))

(defn execute-test [{markdown :markdown html :html count :example}]
  (if (= (-> markdown parse render-html) html)
    (println "Test number " count " passed")
    (println "Fail! Test number " count ".\n"
             "Original commonmark:\n "
             markdown
             "Rendered html:\n"
             "'" (-> markdown parse render-html) "'"
             "\nOriginal html:\n"
             "'" html "'"
             "\n")))

(defn -main []
  (println "Run some tests")
  (doseq [example (-> "./spec.json" slurp (json/read-str :key-fn keyword))]
    (execute-test example)))


