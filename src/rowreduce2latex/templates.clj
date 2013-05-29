(ns rowreduce2latex.templates
  (:require [clojure.string :as str]))

(defprotocol Latexable
  (latex [o]))

(extend-protocol Latexable
  clojure.lang.Ratio
  (latex [r] (format "\\frac{%s}{%s}"
                     (.numerator r)
                     (.denominator r)))
  Object
  (latex [o] (str o)))

(defn matrix-transition->string [[prev next]]
  "Returns the transition embedded in a string."
  (str "\\substack{"prev"\\\\\\Rightarrow\\\\"next"}"))

(defn matrix->string [matrix]
  "Returns the matrix embedded in a string."
  (str "\\begin{bmatrix}"
       (str/join "\\\\" (for [row matrix]
                          (str/join "&" (map latex row))))
       "\\end{bmatrix}"))


