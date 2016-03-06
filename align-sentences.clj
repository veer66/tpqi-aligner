(require ['clojure.string :as 'str])
(require ['clojure.main :as 'main])
(import java.io.BufferedWriter)
(import java.io.FileWriter)

(defn sec1 [lines]
  (str/split (str/join " "
                       (remove empty? lines))
             #"\t"))

(main/load-script "thai-sections-info.clj")
(main/load-script "english-sections-info.clj")
(main/load-script "doc-parser.clj")
(main/load-script "aligner.clj")

(defn list-paths [prefix]
  (lazy-seq (.list (java.io.File. prefix))))

(defn common-paths [eng-prefix tha-prefix]
  (let [eng-paths (into (list) (sort (list-paths eng-prefix)))
        tha-paths (into (list) (sort (list-paths tha-prefix)))]
    (loop [eng-paths eng-paths tha-paths tha-paths lst []]
      (if (or (empty? eng-paths) (empty? tha-paths))
        lst
        (let [eng-path (first eng-paths)
              tha-path (first tha-paths)
              cmp (compare eng-path tha-path)]
          (cond
            (= cmp 0) (recur (rest eng-paths)
                             (rest tha-paths)
                             (cons eng-path lst))
            (> cmp 0) (recur (rest eng-paths)
                             tha-paths
                             lst)
            (< cmp 0) (recur eng-paths
                             (rest tha-paths)
                             lst)))))))

(defn align-all []
  (mapcat (fn [path] (align-from-paths (str "eng/txt/" path)
                                       (str "tha/txt/" path)))
          (common-paths "eng/txt" "tha/txt")))

(with-open [w (BufferedWriter. (FileWriter. "pairs.txt"))]
  (clojure.pprint/write (remove (fn [p] (and (empty? (first p))
                                             (empty? (second p))))
                                (map (fn [p] (into (list) p))
                                     (mapcat (fn [p] p) (align-all))))
                        :stream w))

