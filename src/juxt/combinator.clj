(ns juxt.combinator
  (:require [clojure.string :as str])
  (:gen-class))

(comment "Generic parser combinators")

(defn make-state [input]
  {:status "ok" :remaining (str/split input #"") :result [] :position 0})

(defn pchar [char]
  (fn [state]
    (cond
      (= char (first (:remaining state))) {:status "ok"
                                           :remaining (rest (:remaining state))
                                           :result (conj (:result state) char)
                                           :position (+ (:position state) 1)}
      :else {:status "fail"
             :remaining (:remaining state)
             :result (:result state)
             :position (:position state)})))

(defn many-parse [parsers]
  (fn [state]
    (reduce (fn [state1 parser1]
                    (cond
                      (= (:status state1) "fail") state1
                      :else (parser1 state1))) state parsers)))

(defn pblock [parser]
  (fn [state]
    (let [empty-result-state {:status (:status state)
                              :result []
                              :remaining (:remaining state)
                              :position (:position state)}
          result (parser empty-result-state)]
      {:status (:status result)
       :remaining (:remaining result)
       :result (conj (:result state) (:result result))
       :position (:position result)})))

(defn null-parse []
  (fn [state]
    state))

(defn choice-parse [parsers]
  (fn [state]
    (if (empty? parsers)
      {:status "fail"
       :remaining (:remaining state)
       :result (:result state)
       :position (:position state)}
      (let [result ((first parsers) state)]
        (if (= (:status result) "ok")
          result
          ((choice-parse (rest parsers)) state))))))

(defn optionparse [parser]
  (fn [state]
    (let [result (parser state)]
      (if (= (:status result) "fail")
        state
        result))))

(defn oneormoreparse [parser]
  (fn aux ([state]
          (let [result (parser state)]
            (if (= "ok" (:status result))
              (aux result 1)
              result)))
    ([state acc]
     (let [result (parser state)]
       (if (= "ok" (:status result))
         (aux result (+ 1 acc))
         {:status "ok"
          :remaining (:remaining state)
          :result (:result state)
          :position (:position state)})))))

(defn parse-no-result [parser]
  (fn [state]
    (let [result (parser state)]
      (cond
        (= (:status result) "fail") result
        :else {:status "ok"
               :remaining (:remaining result)
               :result (:result state)
               :position (:position result)}))))
