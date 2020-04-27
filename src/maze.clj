(ns maze)


;; Directions
;;

(defn inverse-direction [direction] (get {:N :S :E :W :S :N :W :E} direction))

(defn directions
  [[row col]]
  {:N [(dec row) col]
   :E [row (inc col)]
   :S [(inc row) col]
   :W [row (dec col)]})


;; Maze display
;;

(defn cell-pos->maze-pos
  [[x y]]
  [(inc (* 2 x)) (inc (* 2 y))])

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

(defn wall-or-floor
  [floor-tiles coord]
  (if (contains? floor-tiles coord)
    "\033[1;34m. "
    "\033[1;32m# "))

(defn generate-maze-coords
  [rows cols]
  (for [x (range (inc (* 2 rows))) y (range (inc (* 2 cols)))]
    [x y]))

(defn cells->maze
  [rows cols cells]
  (let [floor-tiles (generate-floor-tiles cells)]
    (->> (generate-maze-coords rows cols)
         (map (partial wall-or-floor floor-tiles))
         (partition (inc (* 2 cols)))
         (map #(apply str %))
         ((partial interpose "\n"))
         (apply str))))


;; Maze generation
;;

(defn valid-cell?
  [cells position]
  (some? (get cells position)))

(defn valid-directions
  [cells position]
  (filter #(valid-cell? cells (val %)) (directions position)))

(defn unvisited?
  [cells position]
  (empty? (get cells position)))

(defn maybe-get-unvisited-neighbors
  [cells position]
  (some->> (valid-directions cells position)
           (filter #(unvisited? cells (val %)))
           seq))

(defn get-visited-cells
  [cells]
  (filter #(not-empty (val %)) cells))

(defn has-unvisited-neighbor?
  [cells position]
  (some? (maybe-get-unvisited-neighbors cells position)))

(defn maybe-get-next-direction
  [cells position]
  (rand-nth (maybe-get-unvisited-neighbors cells position)))

(defn carve-path
  [position [direction next-position] cells]
  (-> cells
      (update position #(conj % direction))
      (update next-position #(conj % (inverse-direction direction)))))

(defn hunt
  [cells]
  (some->> (get-visited-cells cells)
           keys
           (filter (partial has-unvisited-neighbor? cells))
           seq
           rand-nth))

(defn walk
  [position cells]
  (if-let [next-direction (maybe-get-next-direction cells position)]
    (->> cells
         (carve-path position next-direction)
         (recur (second next-direction)))
    (if-let [next-position (hunt cells)]
      (recur next-position cells)
      cells)))

(defn generate-cells
  [rows cols]
  (->> (for [x (range rows) y (range cols)] (hash-map [x y] #{}))
       (apply merge)))

(defn generate-maze
  [rows cols]
  (let [cells (generate-cells rows cols)
        initial-position (rand-nth (keys cells))]
    (->> (walk initial-position cells)
         (cells->maze rows cols))))

(defn -main
  [rows cols]
  (->> [rows cols]
       (map clojure.edn/read-string)
       (apply generate-maze)
       time
       println))
