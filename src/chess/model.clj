(ns chess.model)

(defprotocol PTile
  (setFigure [this fig])
  (getFigure [this]))

(defprotocol Moves
  (move [this destination])
  (setOptions [this options]))


(deftype Tile [color ^{:volatile-mutable true}figure coord]
  PTile

  (setFigure [this fig]
    (set! figure fig))

  (getFigure [this]
    figure))

(deftype Figure [color type ^{:volatile-mutable true}coord ^{:volatile-mutable true}moves]
  Moves

  (move [this destination]
    (set! coord destination))

  (setOptions [this options]
    (set! moves options)))

(defn tiles [coord]
  {:black (new Tile :black nil coord)
   :white (new Tile :white nil coord)})

(if-let [x (getFigure (get-in @board [1 2]))]
  x)

(defmulti move-options (fn [fig] (.type fig)))

(defmethod move-options :pawn [fig destination]
  (if-let (= :empty (get-in @board (update-in 1 (.coord fig) inc)))
    (.setOptions fig (vec (get-in @board (update-in 1 (.coord fig) inc))))))

(defmulti directions (fn [type] type)
  )

(defmethod directions :four [pattern]
  )

(defmethod directions :full [pattern]
  )

(defmethod directions :pawn
  )

(defn move [from to]
  (let [fig (.figure (get-in @board from))]
    (do (swap! board (.coord fig) assoc-in (.setFigure tile nil))
        (.move fig to)
        (swap! board (.coord fig) assoc-in (.setFigure tile fig))
        switch-turn)))

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

(reset! board (vec (repeatedly 8 (fn []
                     (let [n (switch-turn)]
                       (vec (repeatedly 8 (fn [] (switch-turn))))
                       )))))


(def coords
  (vec (map (fn [x] (vec (map (fn [y] (str x y))
                             ["a" "b" "c" "d" "e" "f" "g" "h"]))) (reverse (range 1 9)) )))

(print coords)

(get-in coords [1 2])

(defn prepare-board []
  (map
   (fn [x] (vec (map (fn [y]
                      (swap! board assoc-in [x y]
                             ((get-in @board [x y])
                              (tiles (get-in coords [x y])))))
                 (range 8)))) (range 8)))

(map (fn [x] (map (fn [y] (println (get-in coords [x y] ))) (range 8))) (range 8))

(range 8)

(prepare-board)

(map #(println (map (fn [n] (str (.coord n) " " (.color n))) %)) @board)

(map #(println %) @board)

(print @board)

(.coord ((get-in @board [1 2]) (tiles (str 1 2))))

((fn [n] (get-in @board [1 2])) 1)

(update-in [[:a1 :a2 :a3] [:b1 :b2 :b3] [:c1 :c2 :c3]] [1 2] (fn [n] (get-in @board [1 2])))
