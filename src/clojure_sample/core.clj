(ns clojure-sample.core)

(defn say-hello [user]
  (print user))

(defn print-coords [coords]
  (let [[lat lon] coords]
    (println (str "Latitude: " lat " - " "Longitude: " lon))))