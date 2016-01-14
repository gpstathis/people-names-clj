(ns people-names.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

;; Holds full name frequency counts
(def full-freq {})
;; Holds first name frequency counts
(def fname-freq {})
;; Holds last name frequency counts
(def lname-freq {})
;; Holds first N unique first names
(def fname-unique [])
;; Holds first N unique last names
(def lname-unique [])

(defn top-n
  "Sorts map m by descending key values and returns the top n entries"
  [m n]
  (take n (sort-by val > m)))

(defn print-results
  [n uniqueCount]
  (println (format "Unique Full Names:  %5s" (count full-freq)))
  (println (format "Unique First Names: %5s" (count fname-freq)))
  (println (format "Unique Last Names:  %5s" (count lname-freq)))
  (println)
  (println (str "Top " n " First Names:"))
  (doseq [fname (top-n fname-freq n)]
    (println (format "%-10s %s" (fname 0) (fname 1))))
  (println)
  (println (str "Top " n " Last Names:"))
  (doseq [lname (top-n lname-freq n)]
    (println (format "%-10s %s" (lname 0) (lname 1))))
  (println)
  (println (str "First " uniqueCount " Unique Names:"))
  (loop [i 0]
    (when (< i uniqueCount)
      (println (format "%-15s %s" (if (< i (- uniqueCount 1))
                                      (get lname-unique (+ i 1))
                                      (get lname-unique 0))
                                  (get fname-unique i)))
      (recur (inc i))))
  (println))

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

(defn process-file
  "Reads the input file and updates the frequency maps"
  [path uniqueCount]
  (with-open [rdr (io/reader path)]
    (doseq [line (line-seq rdr)]
      (let [name-parts (process-line line)]
        (if name-parts
          (let [fName (name-parts 1) lName (name-parts 0)]
            (def full-freq (inc-freq full-freq (format "%s, %s" fName lName)))
            (def fname-freq (inc-freq fname-freq fName))
            (def lname-freq (inc-freq lname-freq lName))
            (if (and  (< (count fname-unique) uniqueCount)
                      (not (contains? fname-unique fName))
                      (not (contains? lname-unique lName)))
              (do
                (def fname-unique (conj fname-unique fName))
                (def lname-unique (conj lname-unique lName))))))))))

(defn -main
  [& args]
  (process-file "resources/people-names-sample.txt" 25)
  (print-results 10 25))
