;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.more-test
  (:require [clojure.test :refer [deftest is]]
            [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.contexts-test :refer :all]
            [conexp.fca.implications :refer :all]
            [conexp.fca.implications-test :as impls]
            [conexp.fca.more :refer :all]))

;;; Bonds

(def small-contexts [test-ctx-01, test-ctx-04, test-ctx-07, test-ctx-08])

(deftest test-all-bonds
  (is (= 2216 (count (all-bonds (make-context-from-matrix 5 5 [0 1 0 1 0
                                                               1 0 1 1 0
                                                               1 0 0 0 0
                                                               1 0 1 1 1
                                                               0 0 0 1 1])
                                (make-context-from-matrix 5 5 [1 1 1 0 1
                                                               1 0 1 1 1
                                                               1 1 1 0 0
                                                               1 1 0 0 0
                                                               0 1 1 0 0])))))
  (with-testing-data [ctx small-contexts]
    (every? #(bond? ctx ctx %) (all-bonds ctx ctx))))

(deftest test-smallest-bond
  (with-testing-data [ctx small-contexts,
                      :let [bonds (all-bonds ctx ctx)]
                      rel (map (fn [_]
                                 (incidence-relation (rand-context (objects ctx)
                                                                   (attributes ctx)
                                                                   (rand))))
                               (range 10))]
    (let [smallest (smallest-bond ctx ctx rel)]
      (and (exists [b bonds]
             (= smallest b))
           (forall [b bonds]
             (=> (subset? rel (incidence-relation b))
                 (subset? (incidence-relation smallest) (incidence-relation b))))))))

;;;

(deftest test-all-shared-intents
  (with-testing-data [ctx small-contexts]
    (let [intents (intents ctx)]
      (= intents (all-shared-intents ctx ctx))))
  (with-testing-data [ctx-1 small-contexts,
                      ctx-2 small-contexts,
                      :when (= (attributes ctx-1) (attributes ctx-2))]
    (= (set (all-shared-intents ctx-1 ctx-2))
       (intersection (set (intents ctx-1))
                     (set (intents ctx-2))))))

(deftest test-all-bonds-by-shared-intents
  (with-testing-data [ctx-1 small-contexts,
                      ctx-2 small-contexts]
    (= (set (all-bonds ctx-1 ctx-2))
       (set (all-bonds-by-shared-intents ctx-1 ctx-2)))))

;;;

(deftest test-context-from-clop
  (is (let [impls #{(make-implication #{1} #{2}),
                    (make-implication #{4} #{2 3})
                    (make-implication #{1 3} #{4 5})}]
        (equivalent-implications?
         impls
         (stem-base (context-from-clop #{1 2 3 4 5}
                                       (clop-by-implications impls))))))
  (is (equivalent-implications?
       impls/testing-data
       (stem-base (context-from-clop (reduce (fn [set impl]
                                               (union set (premise impl) (conclusion impl)))
                                             #{}
                                             impls/testing-data)
                                     (clop-by-implications impls/testing-data)))))
  (with-testing-data [ctx (random-contexts 20 10)]
    (let [ctx (reduce-objects (clarify-context ctx)),
          cct (context-from-clop (attributes ctx)
                                  #(context-attribute-closure ctx %))]
      (and (= (count (objects ctx))
              (count (objects cct)))
           (= (attributes ctx)
              (attributes cct))
           (forall [g (objects ctx)]
             (exists [h (objects cct)]
               (= (object-derivation ctx #{g})
                  (object-derivation cct #{h}))))))))

;;;

(deftest test-concept-stability
  (let [context (make-context 10 10
                              #{[8 7] [9 8] [8 9] [0 0] [3 9] [7 7] [2 3] [2 5] [0 6]
                                [3 3] [5 4] [1 1] [7 3] [8 6] [4 2] [7 8] [3 0] [6 6]
                                [9 6] [1 9] [5 3] [9 9] [4 7] [4 9] [0 9] [4 1] [4 6]
                                [5 7] [4 8] [1 7] [0 3] [6 1] [5 6] [0 7] [5 5] [7 9]
                                [5 9] [2 4] [3 6] [4 5] [9 1] [9 7] [7 0] [6 9] [2 0]
                                [0 4] [3 1] [9 5] [3 8] [9 4] [4 4] [7 5] [2 6] [5 0]
                                [6 2] [3 5] [0 8] [4 0]})
        concept [#{0 7} #{0 7 3 9 8}]]
    (is (= (concept-stability context concept) 1/4))
    (is (every? #(<= % 1)
                (let [ctx (random-context 10 0.5)]
                  (map #(concept-stability ctx %) (concepts ctx)))))
    (with-testing-data [context (random-contexts 10 (rand-int 13))]
      (= (reduce + (map (fn [[A B]]
                          (* (concept-stability context [A B])
                             (expt 2 (count A))))
                        (concepts context)))
         (expt 2 (count (objects context)))))))

;;;

(deftest test-concept-robustness
  (let [ctx1 (make-context-from-matrix 3 3
                                       [0 0 1
                                        1 0 1
                                        1 1 0])
        cpt1 [#{0 1} #{2}]
        ctx2 (make-context-from-matrix ['a 'b 'c 'd 'e 'f]
                                       ['a 'b 'c 'd 'e 'f]
                                       [1 1 1 0 1 1
                                        0 1 0 0 1 0
                                        0 0 1 0 0 1
                                        0 0 0 1 1 1
                                        0 0 0 0 1 0
                                        0 0 0 0 0 1])
        concepts1 (concepts ctx1)
        concepts2 (concepts ctx2)
        cpt2 [#{'d} #{'d 'e 'f}]
        cpt3 [#{'a 'd} #{'e 'f}]
        rctx (random-context 10 0.5)
        conceptsr1 (concepts rctx)
        rctx2 (random-context 15 0.3)
        conceptsr2 (concepts rctx2)]

    (is (= (concept-robustness cpt1 concepts1 (/ 1 3)) (/ 1 3)))
    (is (= (concept-robustness cpt2 concepts2 (/ 3 4)) (/ 3 4)))
    (is (= (concept-robustness cpt3 concepts2 (/ 1 2)) (/ 1 4)))

    (with-testing-data [cpt conceptsr1]
      (and (<= 0 (concept-robustness cpt conceptsr1 0.42))
           (<= (concept-robustness cpt conceptsr1 0.13) 1)))

    (with-testing-data [cpt conceptsr2]
      (= (concept-robustness cpt conceptsr2 (/ 1 2)) (concept-stability rctx2 cpt)))))

(deftest test-average-concept-robustness
  (let [ctx1 (make-context-from-matrix 3 3
                                    [0 0 1
                                     1 0 1
                                     1 1 0])]
    (is (= (average-concept-robustness (concepts ctx1) 0.5) 0.5))
    (is (= (average-concept-robustness (concepts ctx1) (/ 1 3)) (/ 10 27))))

  (with-testing-data [ctx (random-contexts 5 8)]
    (let [cpts (concepts ctx)]
      (= (average-concept-robustness cpts (/ 1 2))
         (/ (reduce (fn [y cpt] (+ y (concept-stability ctx cpt))) 0 cpts)
            (count cpts))))))

(deftest test-jaccard-index
  (let [a #{1 2 3 4 5}
        b #{0 2 4 8}
        e #{}]
    (is (= (jaccard-index a a) 1))
    (is (= (jaccard-index e e) 1))
    (is (= (jaccard-index a e) 0))
    (is (= (jaccard-index a b) (/ 2 7)))))

(deftest test-sorensen-coefficient
  (let [a #{1 2 3 4 5}
        b #{0 2 4 8}
        e #{}]
    (is (= (sorensen-coefficient a a) 1))
    (is (= (sorensen-coefficient e e) 1))
    (is (= (sorensen-coefficient a e) 0))
    (is (= (sorensen-coefficient a b) (/ 4 9)))))

(deftest test-weighted-concept-similarity
  (let [c1 [#{1 2 3} #{4 5 6}]
        c2 [#{2} #{4 6}]
        c3 [#{} #{4 5 6 7}]
        c4 [#{1 2 3} #{}]
        c5 [#{:a :b} #{:c :d}]]

    (doseq [sim [jaccard-index sorensen-coefficient]]
      (is (= (weighted-concept-similarity sim [c1 c1]) 1))
      (is (= (weighted-concept-similarity sim [c3 c3]) 1))
      (is (= (weighted-concept-similarity sim [c3 c4]) 0))
      (is (= (weighted-concept-similarity sim [c1 c5]) 0))
      (is (= (weighted-concept-similarity sim [c3 c5]) 0))
      (doseq [cother [c2 c3 c4]] (is (< 0 (weighted-concept-similarity sim [c1 cother]) 1))))

    (is (= (weighted-concept-similarity jaccard-index [c1 c2]) (/ 1 2)))
    (is (= (weighted-concept-similarity sorensen-coefficient [c1 c2]) (/ 13 20)))

    (is (= (weighted-concept-similarity jaccard-index [c1 c2] (/ 1 3)) (/ 5 9)))
    (is (= (weighted-concept-similarity sorensen-coefficient [c1 c2] (/ 1 3)) (/ 14 20)))

    (is (= (weighted-concept-similarity jaccard-index [c1 c3]) (/ 3 8)))
    (is (= (weighted-concept-similarity sorensen-coefficient [c1 c3]) (/ 3 7)))))

;;;
nil
