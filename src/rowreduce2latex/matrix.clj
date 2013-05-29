(ns rowreduce2latex.matrix
  (:require [clojure.string :as str]
            [seesaw.table :as table])
  (:use [rowreduce2latex templates]
        [seesaw core]))

(defn letter->row-index [text]
  "Returns the row index that corresponds to the letter in text"
  (-> text
      (.toUpperCase)
      (.charAt 0)
      (int)
      (- 65)))

(defn parse-scalar [s]
  "Parses the string (if possible) into one of the accept scalar types:
   integers, decimals, or ratios."
  (cond (re-find #"^\d+$" s) (bigint s)
        (re-find #"^(\d+)?\.\d+$" s) (bigdec s)
        (re-find #"^\d+/\d+$" s) (read-string s)
        :else               (throw (Exception. "Not a valid scalar."))))

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

(defn string->matrix [matrix-text]
  "Converts a string encoding a matrix into nested vectors of scalars."
  (let [rows (filter not-empty (str/split-lines matrix-text))]
    (vec (for [row rows]
           (vec (for [cell (filter not-empty (str/split row #"\s+"))]
                  (parse-scalar cell)))))))


(defn matrix-table []
  "Returns a map whose keys are:
    :matrix a matrix given by the user
    :table  a table whose values are given by matrix."
  (let [textbox (text :multi-line? true)
        matrix-text (show! (dialog :option-type :ok-cancel
                                   :size [450 :by 400]
                                   :content 
                                     (border-panel 
                                       :north "Enter matrix. Separate rows by new lines and columns by spaces."
                                       :center (scrollable textbox))
                                   :success-fn 
                                     (fn [_] (value textbox))))
        matrix (string->matrix matrix-text)] 
    {:matrix matrix
     :table (table :model [:columns (for [n (range 1 (inc (count (first matrix))))]
                                      (keyword (str "x_" n)))
                           :rows matrix])}))



(defn scalar! 
  "Returns a pair whose first value is the text given by the user,
   and second is the text converted into a scalar."
  [& {:keys [message initial]
      :or
      {:message "Enter Scalar"
       :initial ""}}]
   (let [scalar-text (input message :value initial)]
     [scalar-text (read-string scalar-text)]))

(defn row! 
  "Returns a pair whose first value is the text given by the user,
   and second is the text converted into a row index"
  [& {:keys [message initial]
      :or
      {:message "Enter Row Letter"
       :initial ""}}]
    (let [row-text (input message :value initial)]
      [row-text (letter->row-index row-text)]))


(defn swap-rows! [table]
  "Swaps two rows specified by user in table, and returns a transition object
   representing this swap."
  (let [[row1 row1-index] (row! :message "Enter First Row Letter")
        [row2 row2-index] (row! :message "Enter Second Row Letter")
        row1* (table/value-at table row1-index)
        row2* (table/value-at table row2-index)
        row1-str (row->string row1)
        row2-str (row->string row2)]
    (table/update-at! table row1-index row2*)
    (table/update-at! table row2-index row1*)
    [(str row1-str "," row2-str)
     (str row2-str "," row1-str)]))

(defn mult-row! [table]
  "Multiplies a row specified by the user in table, and returns a transition
   object representing this operation."
  (let [[row index] (row! :message "Enter Row Letter")
        [_ scalar] (scalar! :message "Enter Scalar" :initial "1")]
    (table/update-at! table index 
                      (into {} (for [[key val] (table/value-at table index)]
                                 [key (* scalar val)])))
    [(row->string row) (row->string row scalar)]))

(defn add-rows! [table]
  "Adds two rows specified by the user in table, and returns a transition
   object representing this operation."
  (let [[row1 index1] (row! :message "Enter First Row Letter")
        [_ row1-scalar] (scalar! :message "Enter First Row Scalar"
                                 :initial "1")
        [row2 index2] (row! :message "Enter Second Row Letter")
        [_ row2-scalar] (scalar! :message "Enter Second Row Scalar" 
                                 :initial "1")
        row1* (table/value-at table index1)
        row2* (table/value-at table index2)
        row1-str (row->string row1)]
    (table/update-at! table index1
                      (into {} (for [[key val] row1*]
                                 [key (+ (* row1-scalar val) 
                                         (* row2-scalar (key row2*)))])))
    [row1-str (format "%s + %s"
                      (row->string row1 row1-scalar)
                      (row->string row2 row2-scalar))]))
