(ns otus.core-test
  (:require
   [cljs.test :refer [deftest testing is]]
   [otus.core :as sut]))

;; Any live cell with two or three live neighbours survives.
;; Any dead cell with three live neighbours becomes a live cell.
;; All others live cells die in the next generation. Similarly, all other dead cells stay dead.

(def cell [0 0])


(defn take-neighbors [n]
  (->> (sut/neighbors-of cell)
       (take n)
       set))


(deftest game-of-life-rules
  (testing "получаем всех соседей клетки"
    (is (= #{[-1 -1] [0 -1] [1 -1]
             [-1 0]  #_cell [1 0]
             [-1 1] [0 1] [1 1]}
           (take-neighbors 8))))

  (testing "считаем количество живых клеток-соседей"
    (doall
     (for [n [0 1 2 3 4]]
       (let [grid (take-neighbors n)]
         (is (= n (sut/count-active-neighbors cell grid)))))))

  (testing "клетка умирает если соседей больше или меньше чем надо"
    (let [grid (take-neighbors 1)]
      (is (nil? (sut/update-cell cell grid)))))

  (testing "клетка выживает если соседей 2 или 3"
    (let [grid (conj (take-neighbors 2) cell)]
      (is (= cell (sut/update-cell cell grid))))

    (let [grid (conj (take-neighbors 3) cell)]
      (is (= cell (sut/update-cell cell grid)))))

  (testing "пораждаем новые поколения клеток"
    (let [oscillator [#{[0 1] [1 1] [2 1]}
                      #{[1 0] [1 1] [1 2]}]]
      (is (= (sut/evolve (first oscillator))
             (second oscillator))))))
