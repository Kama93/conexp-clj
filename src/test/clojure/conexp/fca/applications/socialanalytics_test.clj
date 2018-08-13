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
            [clojure.test :refer [deftest is]]
            [conexp.base :refer [with-testing-data]]))

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

;;; Clustering-coefficients

(deftest test-local-clustering-coefficient
  (let [g {0 #{1 2} 1 #{0 3} 2 #{0 3 4} 3 #{1 2 4} 4 #{2 3}}
        h {'a #{'a 'b 'c} 'b #{'a 'c} 'c #{'a 'b 0} 0 #{'c 'd 0}
           'd #{0 'e 'f} 'e #{'d 'f} 'f #{'d 'e}}]
    (is (= (set (map #(local-clustering-coefficient g %)
                     (keys g)))
           #{0 (/ 1 3) 1}))
    (is (= (set (map #(local-clustering-coefficient h %)
                     (keys h)))
           #{0 1 (/ 1 3)})))

  (with-testing-data [ctx (random-contexts 10 100)]
    (let [g (context-graph ctx)]
      (or (empty? g)
          (= (set (map #(local-clustering-coefficient g %)
                       (keys g)))
             #{0}))))

  (with-testing-data [ctx (take 10 (repeat (random-context 50 1.0)))]
    (let [g (attribute-projection ctx)]
      (or (empty? g)
          (= (set (map #(local-clustering-coefficient g %)
                       (keys g)))
             #{1})))))

(deftest test-clustering-coefficient
  (let [g {0 #{1 2} 1 #{0 3} 2 #{0 3 4} 3 #{1 2 4} 4 #{2 3}}
        h {'a #{'a 'b 'c} 'b #{'a 'c} 'c #{'a 'b 0} 0 #{'c 'd 0}
           'd #{0 'e 'f} 'e #{'d 'f} 'f #{'d 'e}}]
    (is (= (clustering-coefficient g)
           (/ 1 3))
        (= (clustering-coefficient h)
           (/ 2 3))))

  (with-testing-data [ctx (random-contexts 7 100)]
    (or (empty? (context-graph ctx))
        (= (clustering-coefficient (context-graph ctx))
           0)))

  (with-testing-data [ctx (take 10 (repeat (random-context 50 1.0)))]
    (= (clustering-coefficient (object-projection ctx))
       1)))

(deftest test-object-projection-clustering-coefficient
  (let [ctx (make-context-from-matrix 5 3 [1 1 0
                                           0 1 0
                                           0 0 1
                                           0 1 1
                                           1 0 1])
        ctx1 (make-context-from-matrix 4 3 [1 1 0
                                            0 1 1
                                            1 0 0
                                            0 1 0])]

    (is (= (object-projection-clustering-coefficient ctx)
           (/ 23 30)))
    (is (= (object-projection-clustering-coefficient ctx1)
           (/ 7 12))))

  (with-testing-data [ctx (random-contexts 7 100)]
    (or (empty? (object-projection ctx))
        (<= 0 (object-projection-clustering-coefficient ctx) 1))))

(deftest test-attribute-projection-average-locac-clustering-coefficient
  (let [ctx (make-context-from-matrix 3 5
                                      [1 0 0 1 0
                                       1 1 1 0 0
                                       0 0 0 1 0])
        ctx1 (make-context-from-matrix ['a 'b 'c 'd] 5
                                       [1 1 0 0 0
                                        0 1 1 0 0
                                        0 0 1 1 0
                                        1 0 0 1 1])]

    (is (= (attribute-projection-clustering-coefficient ctx)
           (/ 7 15)))
    (is (= (attribute-projection-clustering-coefficient ctx1)
           (/ 1 3))))

  (with-testing-data [ctx (random-contexts 7 100)]
    (or (empty? (attribute-projection ctx))
        (<= 0
            (attribute-projection-clustering-coefficient ctx)
            1))))


;;;
nil
