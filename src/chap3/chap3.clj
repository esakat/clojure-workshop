(ns chap3.chap3)

(def mapjet-booking
  {
   :id             8773
   :customer-name  "Alice Smith"
   :category-notes "Vegetarian on Sundays"
   :flights        [
                    {
                     :from {:lat 48.9615 :lon 2.4372 :name "Paris Le Bourget Airport"},
                     :to   {:lat 37.742 :lon -25.6976 :name "Ponta Delgada Airport"}},
                    {
                     :from {:lat 37.742 :lon -25.6976 :name "Ponta Delgada Airport"},
                     :to   {:lat 48.9615 :lon 2.4372 :name "Paris Le Bourget Airport"}}
                    ]
   })


(defn print-mapjet-flight [flight]
  (let [{{lat1 :lat lon1 :lon} :from
         {lat2 :lat lon2 :lon} :to} flight]
    (println (str "Flying from: Lat " lat1 " Lon " lon1 " Flying to: Lat " lat2 " Lon " lon2))))

(defn print-mapjet-booking [booking]
  (let [{:keys [customer-name flights]} booking]
    (println (str customer-name " booked " (count flights) " flights."))
    (let [[flight1 flight2 flight3] flights]
      (when flight1 (print-mapjet-flight flight1)) flights
      (when flight2 (print-mapjet-flight flight2))
      (when flight3 (print-mapjet-flight flight3)))))

(defn print-flight
  [[[lat1 lon1] [lat2 lon2]]]
  (println (str "Flying from: Lat " lat1 " Lon " lon1 " Flying to: Lat " lat2 " Lon " lon2)))

(defn print-mapjet-flight
  [{{lat1 :lat lon1 :lon} :from, {lat2 :lat lon2 :lon} :to}]
  (println (str "Flying from: Lat " lat1 " Lon " lon1 " Flying to: Lat " lat2 " Lon " lon2)))

(defn overloading
  ([] "No argument")
  ([a] (str "One argument: " a))
  ([a b] (str "Two arguments: a: " a " b: " b)))

(defn welcome
  ([player] (println (str "Welcome to the Parenthmazes (single-player-mode), " player "!")))
  ([player & friends]
   (println (str "Welcome to the Parenthmazes (multi-player mode), " player "!"))
   (println (str "Sending " (count friends) " friend requests(s) to the following players:" (clojure.string/join "," friends)))))

;; Exercise 3.03: Multi-arity and Destructuring with Parenthmazes
(def weapon-damage {:fists 10.0 :staff 35.0 :sword 100.0 :cast-iron-saucepan 150.0})

;(defn strike
;  ([enemy] (strike enemy :fists))
;  ([enemy weapon]
;   (let [damage (weapon weapon-damage)]
;     (update enemy :health - damage))))
(defn strike
  "With one argument, strike a target with a default :fists `weapon`. With two argument, strike a target with `weapon`.

   Strike will heal a target that belongs to the gnomes camp."
  ([target] (strike target :fists))
  ([{:keys [camp armor], :or {armor 0}, :as target} weapon]
   (let [points (weapon weapon-damage)]
     (if (= :gnomes camp)
       (update target :health + points)
       (let [damage (* points (- 1 armor))]
         (update target :health - damage))))))

;(def enemy {:name "Zulkaz", :health 250, :camp :trolls})
(def enemy {:name "Zulkaz", :health 250, :armor 0.8, :camp :trolls})
(def ally {:name "Carla", :health 80, :camp :gnomes})

; Exercise 3.04
(def weapon-fn-map
  {
   :fists              (fn [health] (if (< health 100) (- health 10) health))
   :staff              (partial + 35)
   :sword              #(- % 100)
   :cast-iron-saucepan #(- % 100 (rand-int 50))
   :sweet-potato       identity
   })

(defn strike
  "With one argument, strike a target with a default :fists `weapon`. With two argument, strike a target with `weapon` and return the target entity"
  ([target] (strike target :fists))
  ([target weapon]
   (let [weapon-fn (weapon weapon-fn-map)]
     (update target :health weapon-fn))))

(def enemy {:name "Arnold", :health 250})

(defn mighty-strike
  "Strike a `target` with all weapons!"
  [target]
  (let [weapon-fn (apply comp (vals weapon-fn-map))]
    (update target :health weapon-fn)))


;; Multimethods
(defmulti strike (fn [m] (get m :weapon)))

(defmethod strike :sword
  [{{:keys [:health]} :target}]
  (- health 100))

(defmethod strike :cast-iron-saucepan
  [{{:keys [:health]} :target}]
  (- health 100 (rand-int 50)))

(defmethod strike :default [{{:keys [:health]} :target}] health)


(defmulti strike (fn
                   [{{:keys [:health]} :target weapon :weapon}]
                   (if (< health 50) :finisher weapon)))

;; Exercise 3.05
(def player {:name "Lea" :health 200 :position {:x 10 :y 10 :facing :north}})

(defmulti move (comp :facing :position))
(defmethod move :north
  [entity]
  (update-in entity [:position :y] inc))

(defmethod move :south
  [entity]
  (update-in entity [:position :y] dec))

(defmethod move :west
  [entity]
  (update-in entity [:position :x] inc))

(defmethod move :east
  [entity]
  (update-in entity [:position :x] dec))

(defmethod move :default [entity] entity)


;; Activity 3.01
;; https://github.com/PacktWorkshops/The-Clojure-Workshop/blob/master/Chapter03/Activity3.01/repl.clj

;; defining the walking-speed and driving-speed constants.
(def walking-speed 4)
(def driving-speed 70)

;; Create two other constants representing two locations with the coordinates
(def paris {:lat 48.856483 :lon 2.352413})
(def bordeaux {:lat 44.834999  :lon -0.575490})
(def london {:lat 51.507351, :lon -0.127758})
(def manchester {:lat 53.480759, :lon -2.242631})

(def vehicle-cost-fns
  {
   :sporche (partial * 0.12 1.3)
   :tayato (partial * 0.07 1.3)
   :sleta (partial * 0.2 0.1)
   })

;; Create the distance function
(defn distance
  "Returns a rough estimate of the distance between two coordinate points, in kilometers. Works better with smaller distance"
  [{lat1 :lat lon1 :lon} {lat2 :lat lon2 :lon}]
  (let [deglen 110.25
        x (- lat2 lat1)
        y (* (Math/cos lat2) (- lon2 lon1))]
    (* deglen (Math/sqrt (+ (* y y) (* x x))))))

;; Create a multimethod called itinerary.
(defmulti itinerary
          "Calculate the distance of travel between two location, and the cost and duration based on the type of transport"
          :transport)

(defmethod itinerary :walking
  [{:keys [:from :to]}]
  (let [walking-distance (distance from to)
        duration (/ (distance from to) walking-speed)]
    {:cost 0 :distance walking-distance :duration duration}))

(defmethod itinerary :driving
  [{:keys [:from :to :vehicle]}]
  (let [driving-distance (distance from to)
        cost ((vehicle vehicle-cost-fns) driving-distance)
        duration (/ driving-distance driving-speed)]
    {:cost cost :distance driving-speed :duration duration}))
