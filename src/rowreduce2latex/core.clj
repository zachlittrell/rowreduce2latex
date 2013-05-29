(ns rowreduce2latex.core
  (:require [clojure.string :as str])
  (:use [rowreduce2latex history matrix templates]
        [seesaw core]))

(defn action->latex-code [action]
  "Converts an action into latex code"
  (str (if-let [transition (:transition action)]
         (str "\n\n" (matrix-transition->string transition) "\n\n")
         "")
       (matrix->string (:matrix action))))

(defn history->latex-code [history]
  "Converts all the actions in history into latex code"
  (loop [code ""
         history history]
    (let [code* (str (action->latex-code (current history)) code)]
      (if (beginning? history)
        code*
        (recur code* (undo history))))))

(defn matrix-op->button [name op! table *history*]
  "Returns a button that performs op! on the table, and adds the resulting
   action to *history*."
  (action :name name
          :handler (fn [_]
                     (let [desc (op! table)]
                       (swap! *history* add
                              {:matrix (table->matrix table)
                               :transition desc})))))


(defn make-operations-panel [*history* table]
  "Returns the panel of buttons for creating the matrix."
  (flow-panel
    :items [(matrix-op->button "Multiply Row"
                               mult-row!
                               table
                               *history*)
            (matrix-op->button "Add Row"
                               add-rows!
                               table
                               *history*)
            (matrix-op->button "Swap Rows"
                               swap-rows!
                               table
                               *history*)
            (action :name "Undo"
                    :handler (fn [_]
                               (let [history* @*history*
                                     undone (undo history*)]
                                   (swap! *history* (constantly undone))
                                   (set-matrix! table 
                                                (:matrix (current undone))))))
            (action :name "Redo"
                    :handler (fn [_]
                               (let [history* @*history*
                                     redone (redo history*)]
                                 (swap! *history* (constantly redone))
                                 (set-matrix! table
                                              (:matrix (current redone))))))
            (action :name "Output"
                    :handler (fn [_]
                               (show! (frame :content (text :text (history->latex-code @*history*)
                                                     :multi-line? true)
                                      :on-close :dispose))))
          ]))

(defn make-gui []
  "Returns the gui for the program"
  (let [{:keys [matrix table]} (matrix-table)
        *history* (atom (history {:matrix matrix}))
        operations-panel (make-operations-panel *history* table)]
  (frame :title "Latex Matrix Ops"
         :on-close :exit
         :size [500 :by 500]
         :content (border-panel
                    :center (scrollable table)
                    :north operations-panel))))


(defn -main [& args]
  (-> (make-gui)
      (show!)))
