(defn exec-if-len-eq [eng tha exec-fn]
  (if (not= (count eng) (count tha))
    nil
    (exec-fn eng tha)))

(defn match-if-len-eq [eng tha]
  (exec-if-len-eq eng tha (fn [eng tha] (map vector eng tha))))

(defn parse-item [line]
  (re-matches #"(^\d+\.\d*).+$" line))

(defn match-by-item [eng tha]
  (let [eng (sort-by second eng)
        tha (sort-by second tha)]
    (loop [eng eng tha tha lst []]
      (if (or (empty? eng) (empty? tha))
        lst
        (let [e (first eng)
              t (first tha)
              item-e (second e)
              item-t (second t)
              cmp (compare item-e item-t)]
          (cond
            (= cmp 0) (recur (rest eng)
                             (rest tha)
                             (conj lst [(first e) (first t)]))
            (< cmp 0) (recur (rest eng) tha lst)
            (> cmp 0) (recur eng (rest tha) lst)))))))

(defn line-checker [line]
  (or (empty? line)
      (re-matches #"^Occupational Standards and Professional Qualifications.*" line)))

(defn remove-lines [lines]
  (remove line-checker lines))

(defn align [eng-doc tha-doc]

  (defn align-title [r]
    (let [eng (remove empty? (nth eng-doc r))
          tha (str/join " " (nth tha-doc r))
          tha (str/split tha #":")
          tha (map str/trim tha)]
      (match-if-len-eq eng tha)))

  (defn simple-match [r]
    (let [eng (nth eng-doc r)
          tha (nth tha-doc r)]
      ;; (println "@@@" (count eng) (count tha))
      (match-if-len-eq eng tha)))

  (defn align-elements [r]
    (let [eng (nth eng-doc r)
          eng (remove nil? (map parse-item eng))
          tha (nth tha-doc r)
          tha (remove nil? (map parse-item tha))]
      (match-by-item eng tha)))

  (defn align-range [r]
    (let [eng (remove-lines (nth eng-doc r))
          tha (remove-lines (nth tha-doc r))]
      (match-if-len-eq eng tha)))
  (concat (list (align-title 0))
          (map simple-match (range 1 9))
          (list (align-elements 9))
          (list (align-range 10))))

(defn align-from-paths [eng-path tha-path]
  (let [eng-doc (with-open [r (clojure.java.io/reader
                               eng-path)]
                  (parse-doc (line-seq r) sections-english))
        tha-doc (with-open [r (clojure.java.io/reader
                               tha-path)]
                  (parse-doc (line-seq r) sections-thai))
        tha-doc (concat (take 10 tha-doc)
                        (list (nth tha-doc 13)))]
    (align eng-doc tha-doc)))
