(ns juxt.core
  (:require [clojure.string :as str]
            [clojure.data.json :as json])
  (:gen-class))


(defrecord FlightData [id model origin destination event timestamp fueldelta])


(defn flight-id-lookup [id env]
  (first (filter (fn [rec]
                   (= id (:id rec)))
                 env)))


(defn handle-remove [xs env]
  (cond
    (= (count xs) 1) (filter (fn [rec]
                               (not= (:id rec) (first xs))) env)
    :else (do
            (println "1 argument (flight ID) only necessray for deletion")
            env)))


(defn update-flight [rec env]
  (let [new-env (handle-remove [(:id rec)] env)]
    (println (format "Updated %s!" (:id rec)))
    (conj new-env rec)))


(defn handle-insert [xs env]
  (try
    (let [flight-record (apply ->FlightData xs)]
      (if (nil? (flight-id-lookup (:id flight-record) env))
        (do
          (println (format "Inserted %s!" (:id flight-record)))
          (conj env flight-record))
        (update-flight flight-record env)))
    (catch Exception e
      (do
        (println e)
        env))))


(defn field-from-string [field rec]
  (case field
    "ID" (:id rec)
    "MODEL" (:model rec)
    "ORIGIN" (:origin rec)
    "DEST" (:dest rec)
    "EVENT" (:event rec)
    "TIME" (:timestamp rec)
    "FUELDELTA" (:fueldelta rec)))


(defn conditional-retrieve [xs env]
  (let [results
        (reduce (fn [data fetch-tuple]
                  (filter (fn [datum]
                            (= (second fetch-tuple)
                               (field-from-string (first fetch-tuple) datum))) data))
                env xs)]
    (println results)
    env))


(defn handle-retrieve [xs env]
  (comment "Retrievals must be of the form RETRIEVE ID F222 ORIGIN LONDON- an even number")
  (cond
    (= (mod (count xs) 2) 0) (conditional-retrieve (partition 2 xs) env)
    :else (do
            (println "Invalid retrieve statement, wrong number of arguments")
            env)))


(defn save-flight-list [env]
  (comment "Turns record list into a list of lists and saves as json")
  (let [assoc-list (map (fn [rec]
                          (map (fn [key]
                                 (get rec key)) (keys rec))) env)
        json-data (json/write-str assoc-list)]
    (spit "resources/flightdata.json" json-data)
    (java.lang.System/exit 0)))


(defn handle-input [xs env]
  (comment "NOTE: Inserts also double as updates as per specifications! The data passed is the same!")
  (case (first xs)
    "INSERT" (handle-insert (rest xs) env)
    "UPDATE" (handle-insert (rest xs) env)
    "RETRIEVE" (handle-retrieve (rest xs) env)
    "REMOVE" (handle-remove (rest xs) env)
    "DONE" (save-flight-list env)
    :else (do
            (println "Invalid command, couldn't parse.")
            env)))


(defn repl [env]
  (comment "REPL persists data using env. Commands can range from 'RETRIEVE'
    to 'UPDATE ID F222 FUELDELTA 333'")
  (print "EXEC> ")
  (flush)
  (let [input (str/trim (read-line))
        input-list (str/split input #" ")
        new-env (handle-input input-list env)]
    (repl new-env)))


(defn make-flight-env [json-data]
  (map (fn [datum] (apply ->FlightData datum)) json-data))


(defn -main
  "REPL for Flight Control Tower"
  [& args]
  (println "FLIGHT CONTROL V0.1")
  (newline)
  (repl (make-flight-env (json/read-json (slurp "resources/flightdata.json")))))
