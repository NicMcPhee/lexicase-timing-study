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

(defn check-pop
  [pop pop-size len]
  (assert (= (count pop) pop-size))
  (assert (every? #(= (count (:errors %)) len) pop)))

(defn -main
  [& _]
  (let [rand-pop (make-rand-pop 1000 100 20)]
    (check-pop rand-pop 1000 100)
    (println "Grouping on random population")
    (println (str "Population has " (count (distinct rand-pop)) " distinct errors"))
    (bench/bench 
      (lexicase-selection rand-pop)
      :verbose)
    (println "No grouping on random population")
    (bench/bench
     (lexicase-selection-no-grouping rand-pop)
     :verbose))
  (let [uniform-pop (make-uniform-pop 1000 100 20)]
    (check-pop uniform-pop 1000 100)
    (println "Grouping on uniform population")
    (println (str "Population has " (count (distinct uniform-pop)) " distinct errors"))
    (bench/bench
     (lexicase-selection uniform-pop)
     :verbose)
    (println "No grouping on uniform population")
    (bench/bench
     (lexicase-selection-no-grouping uniform-pop)
     :verbose))
  (let [fifty-fifty (make-ratio-pop [500 500] 100 20)]
    (check-pop fifty-fifty 1000 100)
    (println "Grouping on 50/50 population")
    (println (str "Population has " (count (distinct fifty-fifty)) " distinct errors"))
    (bench/bench
     (lexicase-selection fifty-fifty)
     :verbose)
    (println "No grouping on 50/50 population")
    (bench/bench
     (lexicase-selection-no-grouping fifty-fifty)
     :verbose))
  (let [eighty-twenty (make-ratio-pop [800 200] 100 20)]
    (check-pop eighty-twenty 1000 100)
    (println "Grouping on 80/20 population")
    (println (str "Population has " (count (distinct eighty-twenty)) " distinct errors"))
    (bench/bench
     (lexicase-selection eighty-twenty)
     :verbose)
    (println "No grouping on 80/20 population")
    (bench/bench
     (lexicase-selection-no-grouping eighty-twenty)
     :verbose))
  (let [fifty-fifty-rand (shuffle (concat (make-uniform-pop 500 100 20)
                                          (make-rand-pop 500 100 20)))]
    (check-pop fifty-fifty-rand 1000 100)
    (println "Grouping on 50/50-rand population")
    (println (str "Population has " (count (distinct fifty-fifty-rand)) " distinct errors"))
    (bench/bench
     (lexicase-selection fifty-fifty-rand)
     :verbose)
    (println "No grouping on 50/50-rand population")
    (bench/bench
     (lexicase-selection-no-grouping fifty-fifty-rand)
     :verbose))
  (let [eighty-twenty-rand (shuffle (concat (make-uniform-pop 800 100 20)
                                            (make-rand-pop 200 100 20)))]
    (check-pop eighty-twenty-rand 1000 100)
    (println "Grouping on 80/20-rand population")
    (println (str "Population has " (count (distinct eighty-twenty-rand)) " distinct errors"))
    (bench/bench
     (lexicase-selection eighty-twenty-rand)
     :verbose)
    (println "No grouping on 80/20-rand population")
    (bench/bench
     (lexicase-selection-no-grouping eighty-twenty-rand)
     :verbose))
  (let [group-10 (make-num-distinct-pop 1000 10 100 20)]
    (check-pop group-10 1000 100)
    (println "Grouping on 10 random errors")
    (println (str "Population has " (count (distinct group-10)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-10)
     :verbose)
    (println "No grouping on 10 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-10)
     :verbose))
  (let [group-20 (make-num-distinct-pop 1000 20 100 20)]
    (check-pop group-20 1000 100)
    (println "Grouping on 20 random errors")
    (println (str "Population has " (count (distinct group-20)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-20)
     :verbose)
    (println "No grouping on 20 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-20)
     :verbose))
  (let [group-25 (make-num-distinct-pop 1000 25 100 20)]
    (check-pop group-25 1000 100)
    (println "Grouping on 25 random errors")
    (println (str "Population has " (count (distinct group-25)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-25)
     :verbose)
    (println "No grouping on 25 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-25)
     :verbose))
  (let [group-40 (make-num-distinct-pop 1000 40 100 20)]
    (check-pop group-40 1000 100)
    (println "Grouping on 40 random errors")
    (println (str "Population has " (count (distinct group-40)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-40)
     :verbose)
    (println "No grouping on 40 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-40)
     :verbose))
  (let [group-50 (make-num-distinct-pop 1000 50 100 20)]
    (check-pop group-50 1000 100)
    (println "Grouping on 50 random errors")
    (println (str "Population has " (count (distinct group-50)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-50)
     :verbose)
    (println "No grouping on 50 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-50)
     :verbose))
  (let [group-100 (make-num-distinct-pop 1000 100 100 20)]
    (check-pop group-100 1000 100)
    (println "Grouping on 100 random errors")
    (println (str "Population has " (count (distinct group-100)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-100)
     :verbose)
    (println "No grouping on 100 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-100)
     :verbose))
  (let [group-200 (make-num-distinct-pop 1000 200 100 20)]
    (check-pop group-200 1000 100)
    (println "Grouping on 200 random errors")
    (println (str "Population has " (count (distinct group-200)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-200)
     :verbose)
    (println "No grouping on 200 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-200)
     :verbose))
  (let [group-500 (make-num-distinct-pop 1000 500 100 20)]
    (check-pop group-500 1000 100)
    (println "Grouping on 500 random errors")
    (println (str "Population has " (count (distinct group-500)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-500)
     :verbose)
    (println "No grouping on 500 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-500)
     :verbose))
  (let [group-1000 (make-num-distinct-pop 1000 1000 100 20)]
    (check-pop group-1000 1000 100)
    (println "Grouping on 1000 random errors")
    (println (str "Population has " (count (distinct group-1000)) " distinct errors"))
    (bench/bench
     (lexicase-selection group-1000)
     :verbose)
    (println "No grouping on 1000 random errors")
    (bench/bench
     (lexicase-selection-no-grouping group-1000)
     :verbose))
  )