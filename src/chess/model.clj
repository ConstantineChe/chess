(ns chess.model)

(deftype Tile [color ^{volatile-mutable true} figure coord])

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
   (vec (repeatedly 8 (fn []
                     (let [n (switch-turn)]
                       (vec (repeatedly 8 (fn [] (switch-turn))))
                       ))))))

(reset! board    (vec (repeatedly 8 (fn []
                     (let [n (switch-turn)]
                       (vec (repeatedly 8 (fn [] (switch-turn))))
                       )))))


(def coords
  (vec (map (fn [x] (vec (map (fn [y] (str x y))(range 1 9) ))) ["a" "b" "c" "d" "e" "f" "g" "h"])))

(print coords)

(get-in coords [1 2])

(defn prepare-board []
  (map
   (fn [x] (vec (map (fn [y]
                   (reset! board (update-in @board [x y]
                                            (fn [n] ((get-in @board [x y])
                                                    (tiles (get-in coords [x y])))))))
                 (range 8)))) (range 8)))

(map (fn [x] (map (fn [y] (println (get-in coords [x y] ))) (range 8))) (range 8))

(range 8)

(prepare-board)


(map #(println (map (fn [n] (.color n)) %)) @board)

(map #(println %) @board)

(print @board)

(.coord ((get-in @board [1 2]) (tiles (str 1 2))))

((fn [n] (get-in @board [1 2])) 1)

(update-in [[:a1 :a2 :a3] [:b1 :b2 :b3] [:c1 :c2 :c3]] [1 2] (fn [n] (get-in @board [1 2])))
