(ns bot)

(def arena
  [[{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "poison", :display "-", :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:_id "me"}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "poison", :display "-", :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "poison", :display "-", :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "poison", :display "-", :transparent true}
    {:type "open", :display nil, :transparent true}]

   [{:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "food", :display "+", :transparent false}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}
    {:type "open", :display nil, :transparent true}]])

(defn position
  [pred coll]
  (first (keep-indexed (fn [idx x]
                         (when (pred x)
                           idx))
                       coll)))

(defn get-player-coords
  "Returns a tuple of a given players coords
  TODO: There's most likely a better way to accomplish this"
  [_id arena]
  (:coords (reduce (fn [memo row]
                     (if (:coords memo)
                       memo
                       (let [idx (position #(= (:_id %) _id) row)
                             row-number (:row memo)]
                         (if idx
                           {:row (+ 1 row-number)
                            :coords [row-number idx]}
                           {:row (+ 1 row-number)}))))
                   {:row 0} arena)))

(defn get-adjacent
  ([pos]
   (get-adjacent pos 1))
  ([[x y] dist]
   (for [w (range (- x dist) (inc (+ dist x))) ; make inclusive
         h (range (- y dist) (inc (+ dist y))) ; make inclusive
         :when (not (and (= w x) (= h y)))]
     [w h])))

(defn get-arena-dimensions
  "returns the dimensions of a given arena (NOTE: Not 0 based)"
  [arena]
  (let [x (count arena)
        y ((comp count first) arena)]
    [x y]))

(defn wrap-coords
  "wraps the coords around to the other side of the arena"
  [[c-x c-y] [d-x d-y]]
  (let [x (cond
            (< c-x 0) (if (> (Math/abs c-x) d-x)
                        (- d-x (mod (Math/abs c-x) d-x))
                        (+ d-x c-x))
            (> c-x d-x) (mod c-x d-x)
            :else c-x)
        y (cond
            (< c-y 0) (if (> (Math/abs c-y) d-y)
                        (- d-y (mod (Math/abs c-y) d-y))
                        (+ d-y c-y))
            (> c-y d-y) (mod c-y d-y)
            :else c-y)]
    [x y]))

(defn check-next-move
  [bot-id arena dist]
  (let [pos (get-player-coords bot-id arena)
        adj (map #(wrap-coords % (get-arena-dimensions arena)) (get-adjacent pos dist))]
    (reduce (fn [m a]
              (let [{:keys [display type transparent]} (get-in arena a)]
                (update-in m [type] conj a)))
            {}
            adj)))

(defn next-move
  [bot-id arena]
  (let [moves (check-next-move bot-id arena 3)]
    (shuffle (or (get moves "food")
                 (get moves "open")))))

(defn- vec-swap
  [[x y]]
  [y x])

(defn- swap-xy
  [pos]
  (mapv vec-swap pos))

(defn- calc-xy
  [steep? pts]
  (let [new-pts (if steep?
                  (swap-xy pts)
                  pts)]
    (if (> (ffirst new-pts) (first (second new-pts)))
      (vec-swap new-pts)
      new-pts)))

(defn- steep?
  [[[x1 y1] [x2 y2]]]
  (> (Math/abs (- y1 y2))
     (Math/abs (- x1 x2))))

(defn draw-line
  "Alternate fn using Bresenham's"
  [start-pts]
  (let [steep? (steep? start-pts)
        [[x1 y1] [x2 y2]] (calc-xy steep? start-pts)
        delta-x (- x2 x1)
        delta-y (Math/abs (- y1 y2))
        y-step (if (< y1 y2) 1 -1)]
    (loop [pt [x1 y1]
           error (Math/floor (/ delta-x 2))
           res []]
      (let [[x y] (if steep? pt (vec-swap pt))]
        (if (>= x x2)
          (conj res [x2 y2])
          (if (< error delta-y)
            (recur [(inc x) (+ y y-step)]
                   (+ error (- delta-x delta-y))
                   (conj res pt))
            (recur [(inc x) y]
                   (- error delta-y)
                   (conj res pt))))))))

(defn good-move
  [bot-id arena]
  (let [my-coords (get-player-coords bot-id arena)
        moves (next-move bot-id arena)
        ]
    (prn moves)
    (loop [a-move (first moves)]
      (let [a-line (draw-line [my-coords a-move])
            move-line (map #(get-in arena %) a-line)
            use-move? ((set (map :type move-line)) "poison")]
        (if-not use-move?
          a-move
          (recur (rest moves)))))))

(defn run
  [{:keys [arena state bot-id energy spawn-bot?] :as step-details}]
  {:commands [{:cmd "MOVE"
               :metadata {:direction (rand-nth [0 1 2 3 4])}}
              {:cmd "SET_STATE"
               :metadata {:foo "bar"}}]})
