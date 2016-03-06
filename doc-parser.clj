(defn count-prefix-match [a b]
  (loop [a (reverse (into () a))
         b (reverse (into () b))
         match-count 0]
    (if (or (empty? a) (empty? b))
      match-count
      (if (= (first a) (first b))
        (recur (rest a) (rest b) (inc match-count))
        match-count))))

(defn add-line-to-doc-lst [line doc-lst]
  (cons (cons line (first doc-lst))
        (rest doc-lst)))

(defn add-new-part [doc-lst]
  (cons (list) doc-lst))

(defn split-doc-internal [lines headers]
  (loop [lines lines headers headers doc-lst (list (list))]
    (if (empty? lines)
      doc-lst
      (let [line (first lines)
            line (str/replace line  #"\x{feff}" " ")
            line (str/trim line)]
        (if (empty? headers)
          (recur (rest lines) nil (add-line-to-doc-lst line doc-lst))
          (let [header (first headers)]
            (if (> (count-prefix-match line header) 5)
              (recur (rest lines)
                     (rest headers)
                     (add-line-to-doc-lst line
                                          (add-new-part doc-lst)))
              (recur (rest lines)
                     headers
                     (add-line-to-doc-lst line doc-lst)))))))))

(defn split-doc [lines headers]
  (remove empty?
          (reverse (map reverse
                        (split-doc-internal lines
                                            headers)))))

(defn parse-doc [lines sections-info]
  (map (fn [proc sec] (if (nil? proc)
                        sec
                        (proc sec)))
       (map second sections-info)
       (split-doc lines (map first sections-info))))
