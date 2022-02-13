(ns juxt.core
  (:require [juxt.interpreter :refer :all])
  (:gen-class))

(defn -main
  "REPL for ParenDB"
  [& args]
  (mainloop))
