(def gemstone-db
  {:ruby
   {:name  "Ruby"
    :stock 120
    :sales [1990 3644 6376 4918 7882 6747 7495 8573 5097 1712]
    :properties
           {:dispersion       0.018
            :hardness         9.0
            :refractive-index [1.77 1.78]
            :color            "Red"}}
   :diamond
   {:name  "Diamond"
    :stock 10
    :sales [8295 329 5960 6118 4189 3436 9833 8870 9700 7182 7061 1579]
    :properties
           {:dispersion       0.044
            :hardness         10
            :refractive-index [2.417 2.419]
            :color            "Typically yellow, brown or gray to colorless"}}
   :moissanite
   {:name  "Moissanite"
    :stock 45
    :sales [7761 3220]
    :properties
           {:dispersion       0.104
            :hardness         9.5
            :refractive-index [2.65 2.69]
            :color            "Colorless, green, yellow"}}})

(defn durability
  [db gemstone]
  (get-in db [gemstone :properties :hardness]))

(defn change-color
  [db gemstone new-color]
  (assoc-in db [gemstone :properties :color] new-color))

(defn sell
  [db gemstone client-id]
  (let [clients-updated-db (update-in db [gemstone :sales] conj client-id)]
    (update-in clients-updated-db [gemstone :stock] dec)))


(def memory-db (atom {}))
(defn read-db [] @memory-db)
(defn write-db [new-db] (reset! memory-db new-db))

(defn create-table
  [table-name]
  (let [db (read-db)]
    (write-db (assoc db (keyword table-name) {:data [], :indexes {}}))))

(defn drop-table
  [table-name]
  (let [db (read-db)]
    (write-db (dissoc db (keyword table-name)))))


(defn insert
  [table record id-key]
  (let [db (read-db)
        new-db (update-in db [(keyword table) :data] conj record)
        index (- (count (get-in new-db [(keyword table) :data])) 1)]
    (write-db
      (update-in new-db [(keyword table) :indexes id-key] assoc (id-key record) index))))


(defn select-*
  [table-name]
  (get-in (read-db) [table-name :data]))

;; 9
(defn select-*-where
  [table-name field field-value]
  (let [db (read-db)
        index (get-in db [table-name :indexes field field-value])
        data (get-in db [table-name :data])]
    (get data index)))