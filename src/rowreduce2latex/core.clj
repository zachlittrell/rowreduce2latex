(ns rowreduce2latex.core
  (:require [seesaw.table :as table]
            [clojure.string :as str])
  (:use [seesaw core]))

(defn actions->latex-code [actions]
  (loop [code ""
         actions actions]
    (if (empty? actions)
      code
      (let [{:keys [transition matrix]} (peek actions)]
        (recur 
          (str (if-let [[prev next] transition]
                 (str "$$\\mattrans{"prev"}{"next"}$$\n\n")
                 "")
               "$$\\begin{bmatrix}"
               (str/join "" (for [row matrix]
                              (str (str/join "&" row) "\\\\")))
               "\\end{bmatrix}$$\n\n"
               code)
          (pop actions))))))


(defn str->row-index [text]
  (-> text
     (.charAt 0)
     (int)
     (- 65)))

(defn make-matrix-grid [actions]
  (let [matrix (read-string (input "Input matrix"))]
    (swap! actions conj {:matrix matrix})
    (table :model [:columns (for [n (range 1 (inc (count (first matrix))))]
                              (keyword (str "x_" n)))
                   :rows matrix])))

(defn set-matrix! [matrix-grid matrix]
  (doseq [[row index] (map list matrix 
                           (range 0 (table/row-count matrix-grid)))]
    (println row matrix)
    (table/update-at! matrix-grid
                      index
                      row)))


(defn get-matrix [matrix-grid]
  (->> (table/value-at matrix-grid
                   (range 0 (table/row-count matrix-grid)))
       (map sort)
       (map (partial map second))
       (map (partial into []))
       (into [])))

                     

(defn make-operations-panel [actions matrix-grid]
  (flow-panel
    :items [(action :name "Multiply Row"
                    :handler (fn [_]
                               (let [row-text (input "Row?")
                                     row (str->row-index row-text)
                                     scalar (read-string (input "Scalar?"))]
                                 (table/update-at! 
                                   matrix-grid
                                   row
                                   (into {} 
                                         (for [[key val] (table/value-at 
                                                            matrix-grid
                                                            row)]
                                            [key (* scalar val)])))
                                 (swap! actions conj
                                        {:transition
                                         [row-text
                                          (str "(" scalar ")" row-text)]
                                         :matrix (get-matrix matrix-grid)}))))
            (action :name "Add Row"
                    :handler (fn [_]
                               (let [row1-text (input "Row 1?")
                                     row2-text (input "Row 2?")
                                     row1 (str->row-index row1-text)
                                     row2 (str->row-index row2-text)
                                     row2-scalar (read-string (input "Row 2 Scalar"))
                                     row1* (table/value-at matrix-grid row1)
                                     row2* (table/value-at matrix-grid row2)]
                                 (table/update-at!
                                   matrix-grid
                                   row1
                                   (into {}
                                         (for [[key val] row1*]
                                           [key (+ val (* row2-scalar 
                                                          (key row2*)))])))
                                 (swap! actions conj
                                        {:transition
                                         [row1-text
                                          (str row1-text " + (" row2-scalar ")" row2-text)]
                                         :matrix (get-matrix matrix-grid)}))))
            (action :name "Swap Rows"
                    :handler (fn [_]
                               (let [row1-text (input "Row 1?")
                                     row2-text (input "Row 2?")
                                     row1 (str->row-index row1-text)
                                     row2 (str->row-index row2-text)
                                     row1* (table/value-at matrix-grid row1)
                                     row2* (table/value-at matrix-grid row2)]
                                 (table/update-at! matrix-grid
                                                   row1 row2*)
                                 (table/update-at! matrix-grid
                                                   row2 row1*)
                                 (swap! actions conj 
                                        {:transition
                                         [(str row1-text "," row2-text)
                                          (str row2-text "," row1-text)]
                                         :matrix (get-matrix matrix-grid)}))))
            (action :name "Undo"
                    :handler (fn [_]
                               (let [actions* @actions]
                                 (when (< 1 (count actions*))
                                   (swap! actions pop)
                                   (set-matrix! matrix-grid (:matrix (peek (pop actions*))))))))
            (action :name "Output"
                    :handler (fn [_]
                               (show! (frame :content (text :text (actions->latex-code @actions)
                                                     :multi-line? true)
                                      :on-close :dispose))))
            ]))

(defn make-gui []
  (let [actions (atom [])
        matrix-grid (make-matrix-grid actions)
        operations-panel (make-operations-panel actions matrix-grid)]
  (frame :title "Latex Matrix Ops"
         :on-close :exit
         :size [500 :by 500]
         :content (border-panel
                    :center (scrollable matrix-grid)
                    :north operations-panel))))


(defn -main [& args]
  (-> (make-gui)
      (show!)))
