(ns rowreduce2latex.matrix
  (:require [seesaw.table :as table])
  (:use [seesaw core]))

(defn letter->row-index [text]
  "Returns the row index that corresponds to the letter in text"
  (-> text
      (.toUpperCase)
      (.charAt 0)
      (int)
      (- 65)))

(defn table->matrix [table]
  "Converts a table into a matrix"
  (->> (table/value-at table
                       (range 0 (table/row-count table)))
       (map sort)
       (map (partial map second))
       (map (partial into []))
       (into [])))

(defn set-matrix! [table matrix]
  "Sets the table elements to match the matrix"
  (doseq [[row index] (map list matrix 
                           (range 0 (table/row-count table)))]
    (table/update-at! table index row)))

(defn matrix-table []
  "Returns a map whose keys are:
    :matrix a matrix given by the user
    :table  a table whose values are given by matrix."
  (let [matrix (read-string (input "Input Matrix"))]
    {:matrix matrix
     :table (table :model [:columns (for [n (range 1 (inc (count (first matrix))))]
                                      (keyword (str "x_" n)))
                           :rows matrix])}))



(defn scalar! 
  "Returns a pair whose first value is the text given by the user,
   and second is the text converted into a scalar."
  ([]
   (scalar! "Enter Scalar"))
  ([message]
   (let [scalar-text (input message)]
     [scalar-text (read-string scalar-text)])))

(defn row! 
  "Returns a pair whose first value is the text given by the user,
   and second is the text converted into a row index"
  ([]
   (row! "Enter row"))
  ([message]
    (let [row-text (input message)]
      [row-text (letter->row-index row-text)])))


(defn swap-rows! [table]
  "Swaps two rows specified by user in table, and returns a transition object
   representing this swap."
  (let [[row1 row1-index] (row! "Enter First Row Letter")
        [row2 row2-index] (row! "Enter Second Row Letter")
        row1* (table/value-at table row1-index)
        row2* (table/value-at table row2-index)]
    (table/update-at! table row1-index row2*)
    (table/update-at! table row2-index row1*)
    [(str row1 "," row2)
     (str row2 "," row1)]))

(defn mult-row! [table]
  "Multiplies a row specified by the user in table, and returns a transition
   object representing this operation."
  (let [[row index] (row! "Enter Row Letter")
        [_ scalar] (scalar! "Enter Scalar")]
    (table/update-at! table index 
                      (into {} (for [[key val] (table/value-at table index)]
                                 [key (* scalar val)])))
    [row (str "(" scalar ")" row)]))

(defn add-rows! [table]
  "Adds two rows specified by the user in table, and returns a transition
   object representing this operation."
  (let [[row1 index1] (row! "Enter First Row Letter")
        [row2 index2] (row! "Enter Second Row Letter")
        [_ row2-scalar] (scalar! "Enter Second Row Scalar")
        row1* (table/value-at table index1)
        row2* (table/value-at table index2)]
    (table/update-at! table index1
                      (into {} (for [[key val] row1*]
                                 [key (+ val (* row2-scalar (key row2*)))])))
    [row1 (str row1 " + (" row2-scalar ")" row2)]))
