(ns chess.model)

(defprotocol PTile
  (setFigure [this fig])
  (getFigure [this]))

(defprotocol Moves
  (move [this destination])
  (setOptions [this options])
  (getCoord [this])
  (getColor [this])
  (ToString [this]))


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

  (getColor [this]
    color)

  (setOptions [this options]
    (set! moves options))

  (getCoord [this]
    coord)

  (ToString [this] type))

(defn tiles [coord]
  {:black (new Tile :black false coord)
   :white (new Tile :white false coord)})

(.setFigure (get-in @board [1 2]) :pawn)

(.getColor (if-let [x (.getFigure (get-in @board [1 2]))]
    x 1))

(defmulti move-options (fn [fig] (.type fig)))

(defmethod move-options :pawn [fig]
  (let [options []]
    (do (if-let [test (get-in @board (map + (.getCoord fig) [0 1]))]
          false
          (conj options (.getCoord test)))
        (map (fn [x]
               (let [test (get-in @board (vec (map + (.getCoord fig) [1 x])))]
                 (if (and (.getFigure test)
                      (= ((.getColor fig) colors :black)
                             (.getColor (.getFigure test))))
                   (conj options (.coord test))))) [1 -1]))))


(.setFigure (get-in @board [1 2]) (Figure. :white :pawn [1 2] []))
(.setFigure (get-in @board [1 3]) (Figure. :white :pawn [1 3] []))
(.setFigure (get-in @board [2 3]) (Figure. :black :pawn [2 3] []))
(.setFigure (get-in @board [2 1]) (Figure. :black :pawn [2 3] []))

(move-options (.getFigure (get-in @board [1 2])))

(map #(println (map (fn [n] (str (if (.getFigure n)
                                  (.type (.getFigure n))
                                  (.getFigure n))
                                " " (.color n))) %)) @board)

(map #(println %) @board)

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
    (do (swap! board (.coord fig) assoc-in (.setFigure tile false))
        (.move fig to)
        (swap! board (.coord fig) assoc-in (.setFigure tile fig))
        switch-turn)))

(def colors {:black :white})

(:white colors :black)

(def turn (atom :white))

(defn switch-turn [] (reset! turn (@turn colors :black)))

(def board
  (atom
   (vec (repeatedly 8 (fn []
                     (let [n (switch-turn)]
                       (vec (repeatedly 8 (fn [] (switch-turn))))
                       ))))))

(defn clean-board []
  (reset! board (vec (repeatedly 8 (fn []
                                     (let [n (switch-turn)]
                                       (vec (repeatedly 8 (fn [] (switch-turn))))))))))



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
                              (tiles [x y]))))
                 (range 8)))) (range 8)))

(map (fn [x] (map (fn [y] (println (get-in coords [x y] ))) (range 8))) (range 8))

(range 8)

(clean-board)
(prepare-board)

(map #(println (map (fn [n] (str (.coord n) " " (.color n))) %)) @board)

(map #(println %) @board)

(print @board)

(.coord ((get-in @board [1 2]) (tiles (str 1 2))))

((fn [n] (get-in @board [1 2])) 1)

(update-in [[:a1 :a2 :a3] [:b1 :b2 :b3] [:c1 :c2 :c3]] [1 2] (fn [n] (get-in @board [1 2])))
