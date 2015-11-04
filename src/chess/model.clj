(ns chess.model)

(defrecord Tile [color ^{volatile-mutable true} figure coord])

(defprotocol Moves
  (move [this x y]))

(deftype Figure [color type coord]
  Moves)

(defn tiles [coord]
  {:black (new Tile :black :empty coord)
   :white (new Tile :white :empty coord)})

(def turn (atom :white))

(defn switch-turn [] (if (= @turn :white)
                       (reset! turn :black)
                       (reset! turn :white)))

(def board
  (atom
   (repeatedly 8 (fn []
                   (let [n (switch-turn)]
                     (vec (repeatedly 8 (fn [] (switch-turn))))
                     )))))

(defn prepare-board []
  (map
   (fn [x] (map (fn [y]
                 (reset! board (update-in @board [x y]
                                          ((get-in @board [x y]) (tiles (str x y))))))
               x)) @board))

(prepare-board)

(map #(println (map (fn [n] (:color n)) %)) @board)

(map #(println %) board-base)
