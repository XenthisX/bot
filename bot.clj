(def arena [
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
{:type "open", :display nil, :transparent true}]]
)

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
[[x y]]
  (for [w (range (dec x) (+ 2 x))
         h (range (dec y) (+ 2 y))
         :when (not (= w x) (= h y))]
         [w h]))

(defn check-next-move
[arena bot_id]
(let [pos (get-player-coords bot_id arena)
      adj (get-adjacent pos)]
      (reduce (fn [m a]
      (let [{:keys [display type transparent]} (get-in arena a)]
      (update-in m [type] conj a)))
      {}
      adj)))

(defn run
  [{:keys [arena state bot_id energy spawn-bot?] :as step-details}]
  {:commands [{:cmd "MOVE"
               :metadata {:direction (rand-nth [0 1 2 3 4])}}
              {:cmd "SET_STATE"
               :metadata {:foo "bar"}}]})