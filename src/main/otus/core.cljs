(ns otus.core
  (:require
   [reagent.core :as reagent]
   [reagent.dom :as rd]))


(defn neighbors-of [[x y]]
  (for [X (range (dec x) (+ x 2))
        Y (range (dec y) (+ y 2))
        :when (not= [X Y] [x y])]
    [X Y]))


(defn count-active-neighbors [cell grid]
  (->> cell
       (neighbors-of)
       set
       (filter grid)
       count))


(defn update-cell [cell grid]
  (let [n (count-active-neighbors cell grid)]
    (cond (= 3 n) cell
          (not= 2 n) nil
          (grid cell) cell)))


(defn evolve [grid]
  (->> (mapcat neighbors-of grid)
       (map #(update-cell % grid))
       (remove nil?)
       (set)))




(def rows 40)
(def cols 40)

(def grid*
  (reagent/atom #{}))


(defn app []
  (let [grid @grid*]
    [:div
     [:div.board {:style {:grid-template-rows    (str "repeat(" rows ", 20px)")
                          :grid-template-columns (str "repeat(" cols ", 20px)")}}
      (for [row (range 0 rows)
            col (range 0 cols)
            :let [alive? (contains? grid [row col])]]
        ^{:key [row col]}
        [:div.cell {:class    (when alive? "cell_alive")
                    :on-click #(swap! grid* conj [row col])}])]]))


(defonce game-loop*
  (atom nil))


;; start

(defn start []
  (->> (js/setInterval #(swap! grid* evolve) 100)
       (reset! game-loop*)))


;; stop
(defn stop []
  (js/clearInterval @game-loop*)
  (reset! game-loop* nil))




(defn glider-gun []
  (->>
   ["........................O"
    "......................O.O"
    "............OO......OO............OO"
    "...........O...O....OO............OO"
    "OO........O.....O...OO"
    "OO........O...O.OO....O.O"
    "..........O.....O.......O"
    "...........O...O"
    "............OO"]
   (map-indexed
    (fn [x item]
      (for [y (range (count item))
            :when (= (nth item y) "O")]
        [x y])))
   (apply concat)
   set
   (reset! grid*)))


(defn mount []
  (glider-gun)
  (rd/render [app] (js/document.getElementById "root"))
  (start))
