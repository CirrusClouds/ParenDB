(ns juxt.parser
  (:require [juxt.combinator :refer :all]
            [clojure.string :as str])
  (:gen-class))

(comment "Specific parsers")

(defn operator-parse []
  (fn [state]
    ((pblock (choice-parse [(many-parse [(pchar "I") (pchar "N") (pchar "S")
                                         (pchar "E") (pchar "R") (pchar "T")])
                            (many-parse [(pchar "D") (pchar "O")
                                         (pchar "N") (pchar "E")])
                            (many-parse [(pchar "M") (pchar "A")
                                         (pchar "K") (pchar "E")])
                            (many-parse [(pchar "R") (pchar "E") (pchar "M")
                                         (pchar "O") (pchar "V") (pchar "E")])
                            (many-parse [(pchar "R") (pchar "E") (pchar "T") (pchar "R")
                                         (pchar "I") (pchar "E") (pchar "V") (pchar "E")])
                            ])) state)))

(defn symparse []
  (fn [state]
    ((pblock (oneormoreparse (choice-parse [(pchar "a") (pchar "b") (pchar "c") (pchar "d") (pchar "e") (pchar "f") (pchar "g") (pchar "h") (pchar "i") (pchar "j") (pchar "k") (pchar "l") (pchar "m") (pchar "n") (pchar "o") (pchar "l") (pchar "m") (pchar "n") (pchar "o") (pchar "p") (pchar "q") (pchar "r") (pchar "s") (pchar "t") (pchar "u") (pchar "v") (pchar "w") (pchar "0") (pchar "1") (pchar "2") (pchar "3") (pchar "4")]))) state)))

(defn sexpr-parse []
  (fn [state]
    ((pblock (many-parse [(parse-no-result (pchar "("))
                          (operator-parse)
                          (oneormoreparse
                           (many-parse [(parse-no-result (pchar " "))
                                        (choice-parse [(sexpr-parse) (symparse)])]))
                          (parse-no-result (pchar ")"))])) state)))

(defn expr-parse []
  (fn [state]
    (let [result ((sexpr-parse) state)]
      (if (= (:status result) "ok")
        {:status (:status result)
         :remaining (:remaining result)
         :result (first (:result result))
         :position (:position result)}
        (throw (Exception.
                (format "Error, couldn't parse at position %s: %s"
                        (:position result)
                        (:remaining result))))))))

(defn make-var [symlist]
  (apply str symlist))

(defn traverse-ast [ast]
  (cond
    (empty? ast) []
    (= (first ast) ["I" "N" "S" "E" "R" "T"]) {:operator 'INSERT
                                               :args (reverse (traverse-ast (rest ast)))}
    (= (first ast) ["D" "O" "N" "E"]) {:operator 'DONE
                                       :args (reverse (traverse-ast (rest ast)))}
    (= (first ast) ["M" "A" "K" "E"]) {:operator 'MAKE
                                       :args (reverse (traverse-ast (rest ast)))}
    (= (first ast) ["R" "E" "M" "O" "V" "E"]) {:operator 'REMOVE
                                               :args (reverse (traverse-ast (rest ast)))}
    (= (first ast) ["R" "E" "T" "R" "I" "E" "V" "E"]) {:operator 'RETRIEVE
                                                       :args (reverse (traverse-ast (rest ast)))}
    (vector? (first (first ast))) (conj (traverse-ast (rest ast))
                                        (traverse-ast (first ast))) 
    :else (conj (traverse-ast (rest ast))
                (make-var (first ast)))))
