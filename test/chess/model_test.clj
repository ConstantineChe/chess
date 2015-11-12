(ns chess.model-test
  (:require [chess.model :refer :all]
            [clojure.test :refer :all])
  (:import [chess.model Tile Figure]))

(map #(println (map (fn [n] (str (.coord n) " " (.color n))) %)) @board)



(deftest pawn-move
  (testing "pawn moves"
    (.setFigure (get-in @board [1 2]) (Figure. :white :pawn [1 2] []))
    (.setFigure (get-in @board [1 3]) :pawn)
    (.setFigure (get-in @board [2 3]) (Figure. :black :pawn [2 3] []))
    (is (= (move-options (.getFigure (get-in @board [1 2]))) [[2 3]]))))

(run-tests)
