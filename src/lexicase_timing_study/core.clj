(ns lexicase-timing-study.core
  (:require [criterium.core :as bench])
  (:gen-class))

(defn make-rand-ind
  [len max-error]
  {:errors (repeatedly len #(rand-int max-error))})

(defn make-uniform-pop
  [pop-size len max-error]
  (repeat pop-size (make-rand-ind len max-error)))

(defn make-rand-pop
  [pop-size len max-error]
  (repeatedly pop-size #(make-rand-ind len max-error)))

(defn make-ratio-pop
  [ratios len max-error]
  (shuffle (mapcat #(repeat % (make-rand-ind len max-error)) ratios)))

(defn make-num-distinct-pop
  [pop-size num-distinct len max-error]
  (let [group-size (quot pop-size num-distinct)]
    (shuffle 
     (apply concat 
            (repeatedly num-distinct #(repeat group-size (make-rand-ind len max-error)))))))

;;; Copied directly from Propeller
(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop]
  (loop [survivors (map rand-nth (vals (group-by :errors pop)))
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

;;; Same as above, but without the grouping
(defn lexicase-selection-no-grouping
  "Selects an individual from the population using lexicase selection."
  [pop]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn lexicase-single-pass
  [pop]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (recur
       (reduce
        (fn [candidates individual]
          (let [best-error (nth (:errors (first candidates)) (first cases))
                this-error (nth (:errors individual) (first cases))]
            (if (< this-error best-error)
              (list individual)
              (if (= this-error best-error)
                (conj candidates individual)
                candidates))))
        (list (first survivors))
        (rest survivors))
       (rest cases)))))

(defn check-pop
  [pop pop-size len]
  (assert (= (count pop) pop-size))
  (assert (every? #(= (count (:errors %)) len) pop)))

(defn -main
  [& _]
  (doseq
   [[pop pop-descriptor num-values]
    [[(make-rand-pop 1000 100 20) "random_population" 1000]
     [(make-uniform-pop 1000 100 20) "uniform_population" 1]
     [(make-ratio-pop [500 500] 100 20) "50/50_population" 2]
     [(make-ratio-pop [800 200] 100 20) "80/20_population" 2]
     [(shuffle (concat (make-uniform-pop 500 100 20)
                       (make-rand-pop 500 100 20))) "50/rand-50_population" 501]
     [(shuffle (concat (make-uniform-pop 800 100 20)
                       (make-rand-pop 200 100 20))) "80/rand-20_population" 801]
     [(make-num-distinct-pop 1000 10 100 20) "10_random_values_population" 10]
     [(make-num-distinct-pop 1000 20 100 20) "20_random_values_population" 20]
     [(make-num-distinct-pop 1000 25 100 20) "25_random_values_population" 25]
     [(make-num-distinct-pop 1000 40 100 20) "40_random_values_population" 40]
     [(make-num-distinct-pop 1000 50 100 20) "50_random_values_population" 50]
     [(make-num-distinct-pop 1000 100 100 20) "100_random_values_population" 100]
     [(make-num-distinct-pop 1000 200 100 20) "200_random_values_population" 200]
     [(make-num-distinct-pop 1000 500 100 20) "500_random_values_population" 500]
     [(make-num-distinct-pop 1000 1000 100 20) "1000_random_values_population" 1000]]
    [selector selector-descriptor]
    [[lexicase-selection "lexicase"]
     [lexicase-selection-no-grouping "lexicase w/o grouping"]
     [lexicase-single-pass "lexicase single pass"]]]
    (check-pop pop 1000 100)
    (println (str selector-descriptor "\t" pop-descriptor "\t" num-values))
    (bench/bench
     (selector pop)
     :verbose)))
