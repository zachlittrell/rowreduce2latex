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

(defn row->string
  "Returns the row-letter into a general format. Can optionally pass
   a scalar for the row."
  ([row-letter]
   (row->string row-letter 1))
  ([row-letter scalar]
    (if (== scalar 1)
      (.toUpperCase row-letter)
      (format "(%s)%s"
              (latex scalar)
              (.toUpperCase row-letter)))))

(defn matrix-transition->string [[prev next]]
  "Returns the transition embedded in a string."
  (str "$\\substack{"prev"\\\\\\Rightarrow\\\\"next"}$"))

(defn matrix->string [matrix]
  "Returns the matrix embedded in a string."
  (str "$\\begin{bmatrix}"
       (str/join "\\\\" (for [row matrix]
                          (str/join "&" (map latex row))))
       "\\end{bmatrix}$"))


