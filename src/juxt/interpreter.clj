(ns juxt.interpreter
  (:require [juxt.parser :refer :all]
            [juxt.combinator :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:gen-class))

(defn handle-remove [arglist env]
  "USEAGE: (REMOVE <table> <field1>)"
  (let [table (get env (first arglist))
        tabledata (:data table)
        filterdata (filter (fn [datum]
                             (not= (first datum) (second arglist))) tabledata)]
    (if (= (count tabledata) (count filterdata))
      (do (println "ID not present in table")
          env)
      (assoc env (first arglist)
             {:len (:len table)
              :data filterdata}))))

(defn handle-insert [arglist env]
  "USEAGE: (INSERT <table> <field1> <field2>..)"
  (if (contains? env (keyword (first arglist)))
    (let [tabledata (get env (keyword (first arglist)))]
      (cond
        (= (Integer/parseInt (:len tabledata)) (count (rest arglist)))
        (let [filterdata (filter (fn [datum]
                                   (not= (first datum) (second arglist))) (:data tabledata))
              new-env (assoc env (keyword (first arglist))
                             {:len (:len tabledata)
                              :data (conj filterdata (into [] (rest arglist)))})]
          (println (format "Inserted/updated %s! in %s" (rest arglist) (keyword (first arglist))))
          new-env)
        :else (do (println "Wrong number of fields for this table!")
                  env)))
    (do
      (println "Table does not exist, can't insert data")
      env)))

(defn handle-done [env]
  "USEAGE: (DONE 0)"
  (let [json-data (json/write-str env)]
    (spit "resources/parendb.json" json-data)
    (java.lang.System/exit 0)))

(defn handle-make [arglist env]
  "USEAGE: (MAKE <tablename> <fieldnum>" 
  (if (contains? env (keyword (first arglist)))
    (do
      (println "Table exists already, can't make this!")
      env)
    (let [new-env (assoc env (keyword (first arglist)) {:len (second arglist)
                                                        :data []})]
      (println (format "Created table %s" (keyword (first arglist))))
      new-env)))


(defn handle-retrieve [arglist env]
  "Note to self: nth doesn't work on vectors. USEAGE: (RETRIEVE <table> <index> <name>)"
  (let [table1 (get env (keyword (first arglist)))
        filterdata (filter (fn [datum]
                             (= (nth datum (Integer/parseInt (second arglist)))
                                (nth arglist 2))) (:data table1))]
    (println filterdata)
    env))

(defn evaldb [expr env]
  (cond
    (= (:operator expr) 'INSERT) (handle-insert (:args expr) env)
    (= (:operator expr) 'DONE) (handle-done env)
    (= (:operator expr) 'MAKE) (handle-make (:args expr) env)
    (= (:operator expr) 'REMOVE) (handle-remove (:args expr) env)
    (= (:operator expr) 'RETRIEVE) (handle-retrieve (:args expr) env)
    :else (do
            (println "Failure to interpret expression")
            env)))

(defn interp [input initenv] (evaldb
                 (traverse-ast (:result ((expr-parse)
                                         (make-state input))))
                 initenv))

(defn repl [env]
  (print "EXEC> ")
  (flush)
  (let [input (str/trim (read-line))
        new-env (interp input env)]
    (println new-env)
    (repl new-env)))

(defn mainloop
  "REPL for ParenDB"
  [& args]
  (println "ParenDB v0.1")
  (newline)
  (repl (json/read-json (slurp "resources/parendb.json"))))
