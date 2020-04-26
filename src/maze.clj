(ns maze)

(defn inverse-direction [direction] (get {:N :S :E :W :S :N :W :E} direction))

(defn directions
  [[row col]]
  {:N [(dec row) col]
   :E [row (inc col)]
   :S [(inc row) col]
   :W [row (dec col)]})

(defn valid-cell? [cells position] (some? (get cells position)))

(defn valid-directions [cells position] (filter #(valid-cell? cells (val %)) (directions position)))

(defn unvisited? [cells position] (empty? (get cells position)))

(def visited? (complement unvisited?))

(defn get-unvisited-neighbors
  [cells position]
  (some->> (valid-directions cells position)
           (filter #(unvisited? cells (val %)))
           seq))

(defn get-next-direction [cells position] (rand-nth (get-unvisited-neighbors cells position)))

(defn carve-path [position [direction next-position] cells]
  (-> cells
      (update position #(conj % direction))
      (update next-position #(conj % (inverse-direction direction)))))

(defn walk
  [position cells]
  (if-let [next-direction (get-next-direction cells position)]
    (let [cells' (carve-path position next-direction cells)]
      (recur (second next-direction) cells'))
    cells))

(defn cell-pos->maze-pos [[x y]] [(inc (* 2 x)) (inc (* 2 y))])

(defn cell->floor-tiles
  [[cell-pos connections]]
  (let [center (cell-pos->maze-pos cell-pos)]
    (-> (partial get (directions center))
        (map connections)
        (conj center))))

(defn generate-floor-tiles
  [maze-cells]
  (->> maze-cells
       (filter #(not-empty (val %)))
       (mapcat cell->floor-tiles)
       (into #{})))

(defn wall-or-floor [floor-tiles coord] (if (contains? floor-tiles coord) ". " "# "))

(defn generate-maze-coords [rows cols] (for [x (range (inc (* 2 rows))) y (range (inc (* 2 cols)))] [x y]))

(defn cells->maze
  [rows cols cells]
  (let [floor-tiles (generate-floor-tiles cells)]
    (->> (generate-maze-coords rows cols)
         (map (partial wall-or-floor floor-tiles))
         (partition (inc (* 2 cols)))
         (map #(apply str %))
         ((partial interpose "\n"))
         (apply str))))

(defn generate-cells [rows cols] (->> (for [x (range rows) y (range cols)] (hash-map [x y] #{})) (apply merge)))

(defn generate-maze
  [rows cols]
  (let [cells (generate-cells rows cols)
        initial-position (rand-nth (keys cells))]
    (->> (walk initial-position cells)
         (cells->maze rows cols))))

(defn -main []
  (println (generate-maze 5 5)))