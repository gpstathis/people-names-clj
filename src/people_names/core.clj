(ns people-names.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn top-n
  "Sorts map m by descending key values and returns the top n entries"
  [m n]
  (take n (sort-by val > m)))

(defn print-results
    [m topN]
    (let [full-freq (get m :full-names)
          fname-freq (get m :first-names)
          lname-freq (get m :last-names)
          fname-unique (get m :fname-unique)
          lname-unique (get m :lname-unique)
          modifiedN (count fname-unique)]
      (do
        (println (format "Unique Full Names:  %5s" (count full-freq)))
        (println (format "Unique First Names: %5s" (count fname-freq)))
        (println (format "Unique Last Names:  %5s" (count lname-freq)))
        (println)
        (println (str "Top " topN " First Names:"))
        (doseq [fname (top-n fname-freq topN)]
          (println (format "%-10s %s" (fname 0) (fname 1))))
        (println)
        (println (str "Top " topN " Last Names:"))
        (doseq [lname (top-n lname-freq topN)]
          (println (format "%-10s %s" (lname 0) (lname 1))))
        (println)
        (println (str "First " modifiedN " Unique Names:"))
        (loop [i 0]
          (when (< i modifiedN)
            (println (format "%-15s %s" (if (< i (- modifiedN 1))
                                            (get lname-unique (+ i 1))
                                            (get lname-unique 0))
                                        (get fname-unique i)))
            (recur (inc i))))
        (println))))

(defn inc-freq
  "Increments the value associated with the given key k in map m. Returns a new
  map. Used to update the name frequency counts."
  [m k]
  (update-in m [k] (fnil inc 0)))

(defn process-line
  "Parses an input line from the file. Lines mathing the regex will return a
  vector such as ['last name' 'first name'], otherwise nil"
  [line]
  (let [line-parts (re-find (re-matcher #"(\w+), (\w+) -- .*" line))]
    (if line-parts
      [(line-parts 1) (line-parts 2)])))

(defn update-unique-names
  "Collects first modifiedN unique first and last names"
  [m lName fName]
    (let [fname-unique (into [] (get m :fname-unique))
          lname-unique (into [] (get m :lname-unique))
          modifiedN (get m :modifiedN)]
      (if (and  (< (count fname-unique) modifiedN)
                (not (contains? fname-unique fName))
                (not (contains? lname-unique lName)))
        (update
          (update m
            :lname-unique conj lName)
            :fname-unique conj fName)
        (identity m))))

(defn update-counts
  "Counts reducer function"
  [m [lName fName]]
    (update
      (update
        (update
          (update-unique-names m lName fName)
          :first-names inc-freq fName)
          :last-names inc-freq lName)
          :full-names inc-freq (format "%s, %s" fName lName)))

(defn process-file
  "Parses the input file and passes the extracted data to a counts reducer function"
  [path modifiedN]
  (with-open [rdr (io/reader path)]
    (let [name-parts (remove nil? (map process-line (line-seq rdr)))
          name-data (reduce update-counts {:first-names {}
                                           :last-names {}
                                           :full-names {}
                                           :fname-unique []
                                           :lname-unique []
                                           :modifiedN modifiedN}
                                           name-parts)]
      name-data)))

(defn -main
  [& args]
  (let [topN 10
        modifiedN 25
        data-counts (process-file "resources/people-names.txt" modifiedN)]
    (print-results data-counts topN)))
