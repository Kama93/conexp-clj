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
                                         attributes
                                         incidence-relation]]
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

;;;Modularity

(defn- close?
  ([x y epsilon]
   (<= (Math/abs (- x y)) epsilon))
  ([x y] (close? x y 0.0001)))

(deftest test-modularity
  (let [g {1 #{1 2 3} 2 #{1}
           3 #{1}}
        h {1 #{2 3 4} 2 #{1 3 4}
           3 #{1 2 4 5} 4 #{1 2 3}
           5 #{3 6 7} 6 #{5 8}
           7 #{5 9} 8 #{6 9}
           9 #{7 8}}]
    (is (close? (modularity g [#{1,2} #{3}])
                (/ 7 36)))
    (is (= (modularity h
                       #{#{1 2 3 4}
                         #{5 6 7}
                         #{8 9}})
           0.34375))))

(deftest test-context-graph-modularity
  (let [ctx (make-context-from-matrix 4 4
                                     [1 0 1 1
                                      0 1 1 0
                                      1 0 1 0
                                      0 0 1 1])]
    (is (close? (context-graph-modularity ctx
                                          [{:objects #{0 1} :attributes #{0 1}},
                                           {:objects #{2 3} :attributes #{2 3}}])
                (/ 4 81))))
  (with-testing-data [ctx (random-contexts 10 10)]
                      (or (empty? (incidence-relation ctx))
                          (= 0.0
                             (context-graph-modularity
                               ctx
                               #{{:objects (objects ctx)
                                  :attributes (attributes ctx)}})))))

(deftest test-object-projection-modularity
  (let [ctx (make-context-from-matrix ['a 'b 'c 'd] [0 1 2 3]
                                      [1 0 0 1
                                       0 1 1 0
                                       1 1 0 0
                                       0 0 0 1])]
    (is (close? (object-projection-modularity ctx [#{'a 'c} #{'b} #{'d}])
                (/ 96 196))))
  (with-testing-data [ctx (random-contexts 20 20)]
                     (let [n (quot (count (objects ctx))
                                   2)]
                       (is (or (empty? (incidence-relation ctx))
                               (<= -1
                                   (object-projection-modularity ctx
                                                                 [(set (take n (objects ctx))),
                                                                  (set (drop n (objects ctx)))]))
                               1)))))

(deftest test-attribute-projection-modularity
  (let [ctx (make-context-from-matrix ['a 'b 'c 'd] [0 1 2 3]
                                      [1 0 0 1
                                       0 1 1 0
                                       1 1 0 0
                                       0 0 0 1])]
    (is (close? (attribute-projection-modularity ctx [#{0 1} #{2 3}])
                (/ 88 196))))
  (with-testing-data [ctx (random-contexts 20 20)]
                     (let [n (quot (count (attributes ctx))
                                   2)]
                       (is (or (empty? (incidence-relation ctx))
                               (<= -1
                                   (attribute-projection-modularity ctx
                                                                 [(set (take n (attributes ctx))),
                                                                  (set (drop n (attributes ctx)))]))
                               1)))))

;;;

nil
