;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.applications.socialanalytics-test
  (:require [conexp.fca.contexts :refer [random-context
                                         random-contexts
                                         make-context-from-matrix
                                         objects
                                         attributes]]
            [conexp.fca.applications.socialanalytics :refer :all]
            [clojure.test :refer [deftest is are]]
            [conexp.base :refer [with-testing-data close?]]))

;Average-shortest-path

(deftest test-context-graph-average-shortest-path
  (let [ctx (make-context-from-matrix 4 3
                          [1 0 1
                           0 1 0
                           1 0 0
                           1 1 0])
        ctx1 (make-context-from-matrix 3 3
                                [0 0 0
                                 0 1 0
                                 1 0 1])]
    (is (= (context-graph-average-shortest-path ctx) (/ 50 21)))
    (is (= (context-graph-average-shortest-path ctx1) (/ 5 4)))
    (is (= (context-graph-average-shortest-path (random-context 50 0)) nil)))

  (with-testing-data [ctx (random-contexts 5 50)]
    (let [value (context-graph-average-shortest-path ctx)]
      (or (nil? value)
          (<= 1
              value
              (+ (count (objects ctx))
                 (count (attributes ctx))))))))

(deftest test-object-projection-average-shortest-paths
  (let [ctx (make-context-from-matrix 3 3
                               [1 0 0
                                1 1 1
                                0 0 0])
        ctx1 (make-context-from-matrix ['a 'b 'c 'd 'e] ['a 'b 'c]
                                       [1 0 1
                                        0 1 0
                                        1 1 0
                                        0 1 1
                                        0 0 1])]
    (is (= (object-projection-average-shortest-path ctx) 1))
    (is (= (object-projection-average-shortest-path ctx1) (/ 13 10)))
    (is (nil? (object-projection-average-shortest-path (random-context 50 0)))))

  (with-testing-data [ctx (random-contexts 5 60)]
    (let [value (object-projection-average-shortest-path ctx)]
      (or (nil? value)
          (<= 1
              value
              (count (objects ctx)))))))

(deftest test-attribute-projection-average-shortest-path
  (let [ctx (make-context-from-matrix 4 5
                                      [1 0 1 1 0
                                       0 1 1 0 0
                                       1 0 0 1 0
                                       0 1 1 0 1])
        ctx1 (make-context-from-matrix 4 4
                                      [1 0 0 0
                                       1 1 0 0
                                       0 1 1 0
                                       0 0 1 1])]
    (is (= (attribute-projection-average-shortest-path ctx) (/ 7 5)))
    (is (= (attribute-projection-average-shortest-path ctx1) (/ 5 3)))
    (is (nil? (attribute-projection-average-shortest-path (random-context 60 0)))))

  (with-testing-data [ctx (random-contexts 7 50)]
    (let [value (attribute-projection-average-shortest-path ctx)]
          (or (nil? value)
              (<= 1
                  value
                  (count (attributes ctx)))))))

(deftest test-context-graph-vertex-degrees
  (let [ctx (make-context-from-matrix 5 3
                                      [1 1 0
                                       0 1 0
                                       0 0 1
                                       0 1 1
                                       1 0 1])
        ctx1 (make-context-from-matrix ['a, 'b, 'c, 'd] ['a, 'b, 'c]
                                       [1 1 0
                                        0 1 1
                                        1 0 0
                                        0 1 0])]
    (is (= (sort (context-graph-vertex-degrees ctx))
           '(1 1 2 2 2 2 3 3)))
    (is (= (sort (context-graph-vertex-degrees ctx1))
           '(1 1 1 2 2 2 3))))

  (with-testing-data [ctx (random-contexts 7 150)]
                     (let [degrees (context-graph-vertex-degrees ctx)
                           m (count (objects ctx))
                           n (count (attributes ctx))]
                       (and (= (count degrees) (+ m n))
                            (every? #(<= 0 % (max m n)) degrees)))))

(deftest test-object-projection-vertex-degrees
  (let [ctx (make-context-from-matrix 5 3
                                      [1 1 0
                                       0 1 0
                                       0 0 1
                                       0 1 1
                                       1 0 1])
        ctx1 (make-context-from-matrix ['a, 'b, 'c, 'd] ['a, 'b, 'c]
                                       [1 1 0
                                        0 1 1
                                        1 0 0
                                        0 1 0])]
    (is (= (sort (object-projection-vertex-degrees ctx))
           '(3 3 4 4 5)))
    (is (= (sort (object-projection-vertex-degrees ctx1))
           '(2 3 3 4))))

  (with-testing-data [ctx (random-contexts 7 150)]
                     (let [degrees (object-projection-vertex-degrees ctx)
                           n (count (objects ctx))]
                       (and (= (count degrees) n)
                            (every? #(<= 0 % n) degrees)))))

(deftest test-attribute-projection-vertex-degrees
  (let [ctx (make-context-from-matrix 5 3
                                      [1 1 0
                                       0 1 0
                                       0 0 1
                                       0 1 1
                                       1 0 1])
        ctx1 (make-context-from-matrix ['a, 'b, 'c] ['a, 'b, 'c, 'd, 'e]
                                       [1 0 0 1 0
                                        1 1 1 0 0
                                        0 0 0 1 0])]
    (is (= (sort (attribute-projection-vertex-degrees ctx))
           '(3 3 3)))
    (is (= (sort (attribute-projection-vertex-degrees ctx1))
           '(0 2 3 3 4))))

  (with-testing-data [ctx (random-contexts 7 150)]
                     (let [degrees (attribute-projection-vertex-degrees ctx)
                           n (count (attributes ctx))]
                       (and (= (count degrees) n)
                            (every? #(<= 0 % n) degrees)))))

;Betweenes-centrality

(deftest test-betweenes-centrality
  (let [g {1 #{2 3 4 5} 2 #{1 3} 3 #{1 2 5}
           4 #{1} 5#{1 3}}
        h {1 #{5} 2 #{4} 3 #{4} 4 #{2 3 5} 5#{1 4 6}
           6 #{5 7 8} 7 #{6 8 9} 8 #{6 7 9} 9 #{7 8}}
        i {'a #{'b 'c 'd} 'b #{'a 'e} 'c #{'a 'e}
           'd #{'a 'e} 'e #{'b 'c 'd}}
        j (zipmap (range 1 100) (repeat #{}))
        centrality-i (betweenes-centrality i)]
    (is (= (betweenes-centrality g) {1 7.0 2 0 3 1.0 4 0 5 0}))
    (is (= (betweenes-centrality h) {1 0 2 0 3 0 4 26.0 5 38.0 6 30.0
                                     7 6.0 8 6.0 9 0}))
    (are [x y] (close? x y 0.1)
         (centrality-i 'a) 3
         (centrality-i 'e) 3
         (centrality-i 'b) 0.66
         (centrality-i 'c) 0.66
         (centrality-i 'd) 0.66)
    (is (= (set (vals (betweenes-centrality j))) #{0}))))

(deftest test-betweenes-centrality-normalized
  (let [centrality-g (betweenes-centrality-normalized
                       {1 #{2 3 4 5} 2 #{1 3} 3 #{1 2 5}
                        4 #{1} 5#{1 3}})
        centrality-h (betweenes-centrality-normalized
                       {1 #{5} 2 #{4} 3 #{4} 4 #{2 3 5} 5#{1 4 6}
                        6 #{5 7 8} 7 #{6 8 9} 8 #{6 7 9} 9 #{7 8}})
        centrality-i (betweenes-centrality-normalized
                       {'a #{'b 'c 'd} 'b #{'a 'e} 'c #{'a 'e}
                        'd #{'a 'e} 'e #{'b 'c 'd}})
        j (zipmap (range 1 100) (repeat #{}))]
    (are [x y] (close? x y 0.1)
         (centrality-g 1) 1
         (centrality-g 2) 0
         (centrality-g 3) (/ 1 7)
         (centrality-g 4) 0
         (centrality-g 5) 0
         ;;;
         (centrality-h 1) 0
         (centrality-h 2) 0
         (centrality-h 3) 0
         (centrality-h 4) (/ 26 38)
         (centrality-h 5) 1
         (centrality-h 6) (/ 30 38)
         (centrality-h 7) (/ 6 30)
         (centrality-h 8) (/ 6 30)
         (centrality-h 9) 0
         ;;;
         (centrality-i 'a) 1
         (centrality-i 'b) 0
         (centrality-i 'c) 0
         (centrality-i 'd) 0
         (centrality-i 'e) 1)
    (is (= (set (vals (betweenes-centrality-normalized j)))) #{0})))

;;;

nil
