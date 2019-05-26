(ns commonmark-clj.core)
(require '[clojure.string :refer [split-lines starts-with? blank? trim replace]])
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
  (matches-pattern line #" {0,3}# +\w+ *"))

(defn is-h2 [line]
  (matches-pattern line #" {0,3}## +\w+ *"))

(defn is-h3 [line]
  (matches-pattern line #" {0,3}### +\w+ *"))

(defn is-h4 [line]
  (matches-pattern line #" {0,3}#### +\w+ *"))

(defn is-h5 [line]
  (matches-pattern line #" {0,3}##### +\w+ *"))

(defn is-h6 [line]
  (matches-pattern line #" {0,3}###### +\w+ *"))

(defn is-setext-h2 [line pn]
  (and (= line "---") (= (:type pn) :p)))

(defn append-header-h1 [document line]
  (zip/append-child
    (root-loc document)
    {:type :h1
     :string-value (trim (replace line #"{0,3}#" ""))}))

(defn append-header-h2 [document line]
  (zip/append-child
    (root-loc document)
    {:type :h2
     :string-value (trim (replace line #"{0,3}##" ""))}))

(defn append-header-h3 [document line]
  (zip/append-child (root-loc document) {:type :h3 :string-value (trim (replace line #"{0,3}###" ""))}))

(defn append-header-h4 [document line]
  (zip/append-child (root-loc document) {:type :h4 :string-value (trim (replace line #"{0,3}####" ""))}))

(defn append-header-h5 [document line]
  (zip/append-child (root-loc document) {:type :h5 :string-value (trim (replace line #"{0,3}#####" ""))}))

(defn append-header-h6 [document line]
  (zip/append-child (root-loc document) {:type :h6 :string-value (trim (replace line #"{0,3}######" ""))}))

(defn append-setext-header-h2 [document line]
  (let [pn (zip/node (prev-node document))
        prev-p (prev-node document)]
    (zip/replace prev-p (-> pn (assoc :type :h2)))))

(defn is-semantic-break [line]
  ; bug *-- is not a valid semantic break?
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
      (is-h3 line) (append-header-h3 document line)
      (is-h4 line) (append-header-h4 document line)
      (is-h5 line) (append-header-h5 document line)
      (is-h6 line) (append-header-h6 document line)
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
    (= (:type el) :h1) (str acc "<h1>" (:string-value el) "</h1>\n")
    (= (:type el) :h2) (str acc "<h2>" (:string-value el) "</h2>\n")
    (= (:type el) :h3) (str acc "<h3>" (:string-value el) "</h3>\n")
    (= (:type el) :h4) (str acc "<h4>" (:string-value el) "</h4>\n")
    (= (:type el) :h5) (str acc "<h5>" (:string-value el) "</h5>\n")
    (= (:type el) :h6) (str acc "<h6>" (:string-value el) "</h6>\n")
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


