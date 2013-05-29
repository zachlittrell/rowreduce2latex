(ns rowreduce2latex.history
  (:require [clojure.zip :as zip]))

(defn history [start] 
  "Returns a history object whose beginning is start."
  (zip/seq-zip (list start :end)))

(defn add [history next] 
  "Adds next as the next event in history."
  (-> history
      (zip/next)
      (zip/next)
      (zip/replace (list next :end))))

(defn beginning? [history]
  "Returns true iff the current history is the same as where it started."
  (not (-> history (zip/prev) (zip/prev))))

(defn undo [history] 
  "Returns the history where current is an event back. Cannot undo
   past the beginning."
  (or (-> history
         (zip/prev)
         (zip/prev))
      history))

(defn redo [history] 
  "Returns the history where current is an event forward. Cannot
   redo beyond the last added event."
  (let [redone (-> history (zip/next) (zip/next))]
    (if (= :end (zip/node redone))
      history
      redone)))

(defn current [history] 
  "Returns the current event in history, which is not necessarily
   the last added event."
  (-> history
      (zip/next)
      (zip/node)))
