(ns juxt.core-test
  (:require [clojure.test :refer :all]
            [juxt.core :refer :all]))

(def test-flight-list [#juxt.core.FlightData{:id "F222", :model "747", :origin "SOME-PLACE", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"}])

(deftest flight-id-lookup-test
  (testing "flight-id-lookup"
    (is (= (flight-id-lookup "F223" test-flight-list) nil)))
  (testing "flight-id-lookup-2"
    (is (= (flight-id-lookup "F222" test-flight-list) #juxt.core.FlightData{:id "F222", :model "747", :origin "SOME-PLACE", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"}))))

(deftest field-from-string-test
  (testing "field from a string"
    (is (= (field-from-string "ID" #juxt.core.FlightData{:id "F222", :model "747", :origin "SOME-PLACE", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"})
           "F222"))))

(deftest remove-flight-test
  (testing "removing a flight"
    (is (= (handle-remove ["F222"] test-flight-list)
           []))))

(deftest update-flight-test
  (testing "update a flight directly"
    (is (= (update-flight #juxt.core.FlightData{:id "F222", :model "747", :origin "HELLO", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"} test-flight-list) [#juxt.core.FlightData{:id "F222", :model "747", :origin "HELLO", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"}]))))

(deftest flight-insert-test
  (testing "flight-insertion"
    (is (= (handle-insert ["F223", "747", "DUBLIN", "LONDON", "Re-Fuel", "2021-03-29T10:00:00", "200"]
                          test-flight-list)
           [#juxt.core.FlightData{:id "F222", :model "747", :origin "SOME-PLACE", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"}
            #juxt.core.FlightData{:id "F223", :model "747", :origin "DUBLIN", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"},]))))

(deftest insert-override-update-test
  (testing "flight-insertion overrided by update"
    (is (= (handle-insert ["F222", "747", "HELLO", "LONDON", "Re-Fuel", "2021-03-29T10:00:00", "200"]
                          test-flight-list)
           [#juxt.core.FlightData{:id "F222", :model "747", :origin "HELLO", :destination "LONDON", :event "Re-Fuel", :timestamp "2021-03-29T10:00:00", :fueldelta "200"}]))))
