;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.contexts
  (:use conexp.base))

(ns-doc
 "Provides the implementation of formal contexts and functions on
  them.")

;;;

(defprotocol Context
  (objects [ctx]    "Returns the objects of a context.")
  (attributes [ctx] "Returns the attributes of a context.")
  (incidence [ctx]  "Returns the incidence of a context as a set of pairs."))

(deftype Formal-Context [objects attributes incidence]
  Object
  (equals [this other]
    (generic-equals [this other] Formal-Context [objects attributes incidence]))
  (hashCode [this]
    (hash-combine-hash Formal-Context objects attributes incidence))
  ;;
  Context
  (objects [this] objects)
  (attributes [this] attributes)
  (incidence [this] incidence))

(defn context?
  "Returns true iff thing is a formal context."
  [thing]
  (instance? Formal-Context thing))

(defn ^String context-to-string
  "Prints contexts in a human readable form."
  ([ctx]
     (context-to-string ctx sort-by-second sort-by-second))
  ([ctx order-on-objects order-on-attributes]
     (let [str #(if (= % nil) "nil" (str %))

           attributes (vec (sort order-on-objects (attributes ctx)))
           objects    (vec (sort order-on-attributes (objects ctx)))
           incidence  (incidence ctx)

           max-att (reduce #(max %1 (count (str %2))) 0 attributes)
           max-obj (reduce #(max %1 (count (str %2))) 0 objects)]
       (with-str-out
         (ensure-length "" max-obj " ") " |" (for [att attributes]
                                               [(print-str att) " "]) "\n"
         (ensure-length "" max-obj "-") "-+" (for [att attributes]
                                               (ensure-length "" (inc (count (print-str att))) "-")) "\n"
         (for [obj objects]
           [(ensure-length (print-str obj) max-obj)
            " |"
            (for [att attributes]
              [(ensure-length (if (incidence [obj att]) "x" ".")
                              (count (print-str att)))
               " "])
            "\n"])))))

(defmethod print-method Formal-Context [ctx out]
  (.write ^java.io.Writer out
          ^String (context-to-string ctx)))

;;;

(defmulti make-context
  "Standard constructor for contexts. Takes a sequence of objects,
  a sequence of attributes and either a set of pairs or function of
  two elements being true iff its arguments are incident. Note that
  the object and attribute sequences are converted to sets and
  therefore have to not contain any douplicate elements. The incidence
  relation is auzomatically restricted to the cartesian product of the
  object an the attribute set."
  {:arglists '([objects attributes incidence])}
  (fn [& args]
    (vec (map clojure-type args))))

(defmethod make-context [clojure-coll clojure-coll clojure-coll]
  [objects attributes incidence]
  (when-not (and (not (map? objects)) (not (map? attributes)))
    (illegal-argument "Objects and attributes should not be given as a map."))
  (let [objs (set objects)
        atts (set attributes)
        inz  (set-of [g m] [[g m] incidence
                            :when (and (contains? objs g)
                                       (contains? atts m))])]
    (Formal-Context. objs atts inz)))

(defmethod make-context [clojure-coll clojure-coll clojure-fn]
  [objects attributes incidence]
  (when-not (and (not (map? objects)) (not (map? attributes)))
    (illegal-argument "Objects and attributes should not be given as a map."))
  (Formal-Context. (set objects)
                   (set attributes)
                   (set-of [x y] [x objects
                                  y attributes
                                  :when (incidence x y)])))

(defmethod make-context :default [obj att inz]
  (illegal-argument "The arguments " obj ", " att " and " inz " are not valid for a formal context."))

(defmulti make-context-nc
  "Context constructor similar to make-context, but does not restrict
  incidence to the crossproduct of object and attribute set and is
  therefore faster. Use with care."
  {:arglists '([objects attributes incidence])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-context-nc [clojure-coll clojure-coll clojure-coll]
  [objects attributes incidence]
  (Formal-Context. (set objects) (set attributes) (set incidence)))

(defmethod make-context-nc :default [obj att inz]
  (illegal-argument "The arguments " obj ", " att " and " inz " are not valid for a formal context."))

;;; Common Operations in Contexts

(defn context-size
  "Returns tuple of number of objects, number of attributes and fill rate."
  [ctx]
  (let [obj-cnt (count (objects ctx)),
        att-cnt (count (attributes ctx)),
        inz-cnt (count (incidence ctx))]
    [obj-cnt, att-cnt, (if (or (zero? obj-cnt)
                               (zero? att-cnt))
                         Double/NaN
                         (double (/ inz-cnt obj-cnt att-cnt)))]))

(defn rename-objects
  "Rename objects in ctx by given function old-to-new."
  [ctx old-to-new]
  (let [objs (map old-to-new (objects ctx)),
        inz  (set-of [(old-to-new g) m] [[g m] (incidence ctx)])]
    (make-context-nc objs (attributes ctx) inz)))

(defn rename-attributes
  "Rename attributes in ctx by given function old-to-new."
  [ctx old-to-new]
  (let [atts (map old-to-new (attributes ctx)),
        inz  (set-of [g (old-to-new m)] [[g m] (incidence ctx)])]
    (make-context-nc (objects ctx) atts inz)))

(defn make-context-from-matrix
  "Given objects G and attribute M and an incidence matrix constructs
  the corresponding context. G and M may also be numbers where they
  represent (range G) and (range M) respectively."
  [G M bits]
  (let [G (if (number? G) (range G) G),
        M (if (number? M) (range M) M),
        m (count G),
        n (count M)]
    (assert (= (* m n) (count bits)))
    (make-context-nc G M
                     (set-of [a b] [i (range (count G)),
                                    j (range (count M)),
                                    :when (= 1 (nth bits (+ (* n i) j)))
                                    :let [a (nth G i),
                                          b (nth M j)]]))))

(defn subcontext?
  "Tests whether ctx-1 is a subcontext ctx-2 or not."
  [ctx-1 ctx-2]
  (let [objs-1 (objects ctx-1)
        objs-2 (objects ctx-2)
        atts-1 (attributes ctx-1)
        atts-2 (attributes ctx-2)]
    (and (subset? objs-1 objs-2)
         (subset? atts-1 atts-2)
         (forall [[g m] (incidence ctx-1)]
           (=> (and (contains? objs-1 g)
                    (contains? atts-1 m))
               (contains? (incidence ctx-2) [g m]))))))

(defn restrict-concept
  "Restricts the given concept to the given subcontext."
  [concept subcontext]
  [(intersection (first concept) (objects subcontext)),
   (intersection (second concept) (attributes subcontext))])

(defn object-derivation
  "Computes set of attributes common to all objects in context."
  [ctx objects]
  (let [inz  (incidence ctx),
        atts (attributes ctx)]
    (set-of m [m atts :when (forall [g objects] (inz [g m]))])))

(defn attribute-derivation
  "Computes set of objects common to all attributes in context."
  [ctx attributes]
  (let [inz  (incidence ctx),
        objs (objects ctx)]
    (set-of g [g objs :when (forall [m attributes] (inz [g m]))])))

(defn concept?
  "Tests whether given pair is a concept in context ctx."
  [ctx [set-of-obj set-of-att]]
  (and (set? set-of-obj)
       (set? set-of-att)
       (subset? set-of-obj (objects ctx))
       (subset? set-of-att (attributes ctx))
       (= set-of-obj (attribute-derivation ctx set-of-att))
       (= set-of-att (object-derivation ctx set-of-obj))))

(defn clarify-objects
  "Clarifies objects in context ctx."
  [ctx]
  (let [prime (memoize (partial object-derivation ctx))
        new-objs (set (distinct-by-key (objects ctx) #(prime #{%})))]
    (make-context new-objs (attributes ctx) (incidence ctx))))

(defn clarify-attributes
  "Clarifies attributes in context ctx."
  [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))
        new-atts (set (distinct-by-key (attributes ctx) #(prime #{%})))]
    (make-context (objects ctx) new-atts (incidence ctx))))

(defn clarify-context
  "Clarifies context ctx."
  [ctx]
  (clarify-objects (clarify-attributes ctx)))

(defn object-clarified?
  "Tests whether given context ctx is object clarified."
  [ctx]
  (let [prime (memoize (partial object-derivation ctx))]
    (not (exists [g (objects ctx)
                  h (objects ctx)]
           (and (not= g h) (= (prime #{g}) (prime #{h})))))))

(defn attribute-clarified?
  "Tests whether given context ctx is attribute clarified."
  [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))]
    (not (exists [m (attributes ctx)
                  n (attributes ctx)]
           (and (not= m n) (= (prime #{m}) (prime #{n})))))))

(defn clarified?
  "Tests whether given context ctx is clarified."
  [ctx]
  (and (object-clarified? ctx)
       (attribute-clarified? ctx)))

(defn down-arrows
  "Computes the down arrow relation of ctx."
  [ctx]
  (let [obj   (objects ctx),
        att   (attributes ctx),
        inz   (incidence ctx),
        prime (map-by-fn #(object-derivation ctx #{%}) obj)]
    (set-of [g m]
            [g obj
             m att
             :when (and (not (inz [g m]))
                        (forall [h obj]
                          (=> (proper-subset? (prime g) (prime h))
                              (inz [h m]))))])))

(defn up-arrows
  "Computes the up arrow relation of ctx."
  [ctx]
  (let [obj   (objects ctx),
        att   (attributes ctx),
        inz   (incidence ctx),
        prime (map-by-fn #(attribute-derivation ctx #{%}) att)]
    (set-of [g m]
            [g obj,
             m att
             :when (and (not (inz [g m]))
                        (forall [n att]
                          (=> (proper-subset? (prime m) (prime n))
                              (inz [g n]))))])))

(defn up-down-arrows
  "Returns up-down-arrow relation of ctx."
  [ctx]
  (intersection (up-arrows ctx) (down-arrows ctx)))

(defn reduce-clarified-context
  "Reduces context ctx assuming it is clarified."
  [ctx]
  (let [uda     (up-down-arrows ctx),
        new-obj (set-of g [[g _] uda]),
        new-att (set-of m [[_ m] uda])]
    (make-context-nc new-obj new-att (incidence ctx))))

(defn reduce-context-objects
  "Object reduction for ctx."
  [ctx]
  (make-context-nc (set-of g [[g _] (down-arrows ctx)])
                   (attributes ctx)
                   (incidence ctx)))

(defn reduce-context-attributes
  "Attribute reduction for ctx."
  [ctx]
  (make-context-nc (objects ctx)
                   (set-of m [[_ m] (up-arrows ctx)])
                   (incidence ctx)))

(defn reduce-context
  "Reduces context ctx."
  [ctx]
  (if (clarified? ctx)
    (reduce-clarified-context ctx)
    (reduce-clarified-context (clarify-context ctx))))

(defn reduced?
  "Tests whether given context ctx is reduced or not."
  [ctx]
  (and (clarified? ctx)
       (let [obj (objects ctx),
             att (attributes ctx),
             uda (up-down-arrows ctx)]
         (and (forall [g obj]
                (exists [[h _] uda]
                  (= g h)))
              (forall [m att]
                (exists [[_ n] uda]
                  (= m n)))))))

(defn context-object-closure
  "Computes double prime in context ctx for the given set-of-objects."
  [ctx set-of-objects]
  (attribute-derivation ctx (object-derivation ctx set-of-objects)))

(defn context-extents
  "Computes a sequence of all extents of ctx."
  [ctx]
  (all-closed-sets (objects ctx) (partial context-object-closure ctx)))

(defn context-attribute-closure
  "Computes double prime in context ctx for the given set-of-attributes."
  [ctx set-of-attributes]
  (object-derivation ctx (attribute-derivation ctx set-of-attributes)))

(defn context-intents
  "Computes a sequence of all intents of ctx."
  [ctx]
  (all-closed-sets (attributes ctx) (partial context-attribute-closure ctx)))

(defn- cbo-test
  "Simple implementation of the test used by the «Close by One»
  algorithm."
  [attribute j B D]
  (loop [i 0]
    (if (= i j)
      true
      (if (not= (contains? B (attribute i))
                (contains? D (attribute i)))
        false
        (recur (inc i))))))

(defn concepts
  "Returns a sequence of all concepts of ctx."
  [ctx]
  (let [n             (count (attributes ctx)),
        attribute     (vec (attributes ctx)),
        att-extent    (vec (map #(attribute-derivation ctx #{%}) attribute)),
        generate-from (fn generate-from [A B y]
                        (lazy-seq
                         (cons [A B]
                               (when (not= B (attributes ctx))
                                 (mapcat (fn [j]
                                           (when-not (contains? B (attribute j))
                                             (let [C (intersection A (att-extent j)),
                                                   D (object-derivation ctx C)]
                                               (when (cbo-test attribute j B D)
                                                 (generate-from C D (inc j))))))
                                         (range y n)))))),
        empty-obj     (attribute-derivation ctx #{}),
        empty-att     (object-derivation ctx empty-obj)]
    (generate-from empty-obj empty-att 0)))

;;; Common Operations with Contexts

(defn dual-context
  "Dualizes context ctx, that is (G,M,I) gets (M,G,I^{-1})."
  [ctx]
  (make-context-nc (attributes ctx) (objects ctx) (set-of [m g] [[g m] (incidence ctx)])))

(defn invert-context
  "Inverts context ctx, that is (G,M,I) gets (G,M,(G x M) \\ I)."
  [ctx]
  (make-context-nc (objects ctx) (attributes ctx) (set-of [g m] [g (objects ctx)
                                                                 m (attributes ctx)
                                                                 :when (not ((incidence ctx) [g m]))])))

(defn context-union
  "Returns the union of ctx-1 and ctx-2. Note that this union is
  inclusive, use context-disjoint-union if this is not what you want."
  [ctx-1 ctx-2]
  (make-context-nc (union (objects ctx-1) (objects ctx-2))
                   (union (attributes ctx-1) (attributes ctx-2))
                   (union (incidence ctx-1) (incidence ctx-2))))

(defn context-disjoint-union
  "Returns the disjoint union of ctx-1 and ctx-2."
  [ctx-1 ctx-2]
  (let [new-objs (disjoint-union (objects ctx-1)
                                 (objects ctx-2)),
        new-atts (disjoint-union (attributes ctx-1)
                                 (attributes ctx-2))
        new-inz  (set-of [[g idx-g] [m idx-m]]
                         [[g idx-g] new-objs
                          [m idx-m] new-atts
                          :when (or ((incidence ctx-1) [g m])
                                    ((incidence ctx-2) [g m]))])]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-intersection
  "Returns context intersection of ctx1 and ctx2."
  [ctx1 ctx2]
  (make-context-nc (intersection (objects ctx1) (objects ctx2))
                   (intersection (attributes ctx1) (attributes ctx2))
                   (intersection (incidence ctx1) (incidence ctx2))))

(defn context-composition
  "Returns context composition of ctx-1 and ctx-2, that is

    (G_1,M_1,I_1) o (G_2,M_2,I_2) := (G_1,M_2,I_1 o I_2).
  "
  [ctx-1 ctx-2]
  (make-context-nc (objects ctx-1)
                   (attributes ctx-2)
                   (set-of [g m]
                           [g (objects ctx-1)
                            m (attributes ctx-2)
                            :when (exists [x (intersection (attributes ctx-1)
                                                           (objects ctx-2))]
                                          (and ((incidence ctx-1) [g x])
                                               ((incidence ctx-2) [x m])))])))

(defn context-apposition
  "Returns context apposition of ctx-1 and ctx-2, that is

   (G_1,M_1,I_1) | (G_1,M_2,I_2) := (G_1,M_1 dunion M_2,I_1 dunion I_2).
  "
  [ctx-1 ctx-2]
  (if (not= (objects ctx-1) (objects ctx-2))
    (illegal-argument "Cannot do context apposition, since object sets are not equal."))
  (let [new-atts (union (set-of [m 1] [m (attributes ctx-1)])
                        (set-of [m 2] [m (attributes ctx-2)]))
        new-inz  (union (set-of [g [m 1]] [[g m] (incidence ctx-1)])
                        (set-of [g [m 2]] [[g m] (incidence ctx-2)]))]
    (make-context-nc (objects ctx-1) new-atts new-inz)))

(defn context-subposition
  "Returns context subposition of ctx-1 and ctx-2, that is

     (G_1,M_1,I_1)
    --------------- := (G_1 dunion G_2, M_1, I_1 union I_2).
     (G_2,M_1,I_2)
  "
  [ctx-1 ctx-2]
  (if (not= (attributes ctx-1) (attributes ctx-2))
    (illegal-argument "Cannot do context subposition, since attribute sets are not equal."))
  (let [new-objs (union (set-of [g 1] [g (objects ctx-1)])
                        (set-of [g 2] [g (objects ctx-2)]))
        new-inz  (union (set-of [[g 1] m] [[g m] (incidence ctx-1)])
                        (set-of [[g 2] m] [[g m] (incidence ctx-2)]))]
    (make-context-nc new-objs (attributes ctx-1) new-inz)))

(defn context-transitive-closure
  "Transitively closes incidence relation of ctx and returns corresponding context."
  [ctx]
  (make-context-nc (objects ctx) (attributes ctx) (transitive-closure (incidence ctx))))

(defn rand-context
  "Randomly fills context on base-sets with crosses and propability fill-rate."
  ([base-set fill-rate]
     (rand-context base-set base-set fill-rate))
  ([objects attributes fill-rate]
     (when-not (and (number? fill-rate)
                    (<= 0 fill-rate 1))
       (illegal-argument "Fill-rate must be a number between 0 and 1."))
     (make-context objects attributes (fn [_ _] (> fill-rate (rand))))))

(defn random-contexts
  "Returns a sequence of number contexts, with random fill rate and random
  size between 0 and upper-limit."
  [number upper-limit]
  (repeatedly number #(rand-context (set-of-range (dec (rand upper-limit)))
                                    (set-of-range (dec (rand upper-limit)))
                                    (rand 1.0))))

(defn one-context
  "Returns context full of crosses."
  [base-set]
  (make-context base-set base-set (fn [_ _] true)))

(defn null-context
  "Returns context with no crosses."
  [base-set]
  (make-context base-set base-set (fn [_ _] false)))

(defn diag-context
  "Returns = on base-set as context."
  [base-set]
  (make-context base-set base-set =))

(defn adiag-context
  "Returns not= on base-set as context."
  [base-set]
  (make-context base-set base-set not=))

(defn context-sum
  "Computes the context sum of ctx-1 and ctx-2, that is

    (G_1,M_1,I_1) + (G_2,M_2,I_2) :=
      (G_1 dunion G_2, M_1 dunion M_2, I_1 dunion I_2
       dunion (G_1 x M_2) dunion (G_2 x M_1))

  where all set unions are disjoint set unions."
  [ctx-1 ctx-2]
  (let [new-objs (union (set-of [g_1 1] [g_1 (objects ctx-1)])
                        (set-of [g_2 2] [g_2 (objects ctx-2)]))
        new-atts (union (set-of [m_1 1] [m_1 (attributes ctx-1)])
                        (set-of [m_2 2] [m_2 (attributes ctx-2)]))
        new-inz  (union (set-of [[g_1 1] [m_1 1]]
                                [[g_1 m_1] (incidence ctx-1)])
                        (set-of [[g_2 2] [m_2 2]]
                                [[g_2 m_2] (incidence ctx-2)])
                        (set-of [[g_1 1] [m_2 2]]
                                [g_1 (objects ctx-1)
                                 m_2 (attributes ctx-2)])
                        (set-of [[g_2 2] [m_1 1]]
                                [g_2 (objects ctx-2)
                                 m_1 (attributes ctx-1)]))]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-product
  "Computes the context product of ctx-1 and ctx-2, that is

    (G_1,M_1,I_1) x (G_2,M_2,I_2) :=
      (G_1 x G_2, M_1 x M_2, nabla)

  where

    (g_1,g_2) nabla (m_1,m_2) <=> g_1I_1m_1 or g_2I_2m_2.
  "
  [ctx-1 ctx-2]
  (let [new-objs (cross-product (objects ctx-1) (objects ctx-2))
        new-atts (cross-product (attributes ctx-1) (attributes ctx-2))
        inz-1    (incidence ctx-1)
        inz-2    (incidence ctx-2)
        new-inz  (set-of [[g_1, g_2], [m_1, m_2]]
                         [[g_1, g_2] new-objs
                          [m_1, m_2] new-atts
                          :when (or (inz-1 [g_1, m_1])
                                    (inz-2 [g_2, m_2]))])]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-semiproduct
  "Computes the context semiproduct of ctx-1 and ctx-2, where for
  contexts (G_1,M_1,I_1) and (G_2,M_2,I_2) their semidirect product is
  defined as

    (G_1 x G_2, M_1 dunion M_2, nabla)

  where

    (g_1,g_2) nabla (j,m) <=> g_jI_jm for j in {1,2}.
  "
  [ctx-1 ctx-2]
  (let [new-objs (cross-product (objects ctx-1) (objects ctx-2))
        new-atts (disjoint-union (attributes ctx-1) (attributes ctx-2))
        inzs     [(incidence ctx-1) (incidence ctx-2)]
        new-inz  (set-of [g, [m, idx]]
                         [g new-objs
                          [m, idx] new-atts
                          :when ((inzs idx) [(g idx) m])])]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-xia-product
  "Computes Xia's product of ctx-1 and ctx-2, where for two contexts
  (G_1,M_1,I_1) and (G_2,M_2,I_2) their Xia product is defined as

    (G_1 x G_2, M_1 x M_2, ~)

  where

    (g_1,g_2) ~ (m_1,m_2) <=> (g_1I_1m_1 <=> g_2I_2m_2).
  "
  [ctx-1 ctx-2]
  (let [G_1 (objects ctx-1)
        G_2 (objects ctx-2)
        M_1 (attributes ctx-1)
        M_2 (attributes ctx-2)
        I_1 (incidence ctx-1)
        I_2 (incidence ctx-2)

        new-objs (cross-product G_1 G_2)
        new-atts (cross-product M_1 M_2)
        new-inz  (set-of [[g_1, g_2], [m_1, m_2]]
                         [[g_1,g_2] new-objs
                          [m_1,m_2] new-atts
                          :when (<=> (I_1 [g_1,m_1])
                                     (I_2 [g_2,m_2]))])]
    (make-context-nc new-objs new-atts new-inz)))

;;;

nil
