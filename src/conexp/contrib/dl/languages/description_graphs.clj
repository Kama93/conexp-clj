;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.description-graphs
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes)
  (:use clojure.contrib.pprint))

;;;

(deftype Description-Graph [language vertices neighbours vertex-labels])

(defn vertices
  "Returns vertices of given description graph."
  [description-graph]
  (:vertices description-graph))

(defn neighbours
  "Returns a function mapping vertices to sets of pairs of roles and
  names."
  [description-graph]
  (:neighbours description-graph))

(defn vertex-labels
  "Returns vertex labeling function of given description graph."
  [description-graph]
  (:vertex-labels description-graph))

(defn graph-language
  "Returns the underlying description language of the given
  description graph."
  [description-graph]
  (:language description-graph))

(defn make-description-graph
  "Creates and returns a description graph for the given arguments."
  [language vertices neighbours vertex-labels]
  (Description-Graph language vertices neighbours vertex-labels))

(defn edges
  "Returns labeld edges of given description graph."
  [description-graph]
  (set-of [A r B] [A (vertices description-graph),
		   [r B] ((neighbours description-graph) A)]))

(defmethod print-method ::Description-Graph [dg out]
  (let [#^String output (with-out-str
			  (pprint (list 'Description-Graph
					(vertices dg)
					(neighbours dg)
					(vertex-labels dg))))]
    (.write out (.trim output))))

;;;

(defn- new-var
  "Returns a new variable name (globally unique)."
  []
  (gensym "C"))

(defn- normalize-definition
  "Normalzes given definition with additional defintions, returning
  the normalized definition and a possibly enhanced structure of
  definitions."
  ;; stupid description
  [definition term-names]
  (let [language (expression-language (definition-expression definition)),
	target   (definition-target definition),
	expr     (let [expr (definition-expression definition)]
		   (if (or (not (compound? expr))
			   (not= 'and (operator expr)))
		     (make-dl-expression language (list 'and expr))
		     expr))]
    (loop [args       (vec (arguments expr)),
	   normalized [],
	   names      term-names]
      (if (empty? args)
	[(make-dl-definition target (make-dl-expression language (if (= 1 (count normalized))
								   (first normalized)
								   (list* 'and normalized))))
	 names]
	(let [next-term (first args)]
	  (if (atomic? next-term)
	    (recur (rest args) (conj normalized next-term) names)
	    ;; next-term is an existential quantification
	    (let [[r B] (vec (arguments next-term))]
	      (if (atomic? B)
		(recur (rest args) (conj normalized next-term) names)
		;; B is an existential quantification
		(let [name (get names B nil)]
		  (if-not (nil? name)
		    (recur (rest args) (conj normalized (list 'exists r name)) names)
		    (let [new-name (new-var),
			  new-names (conj names [B new-name])]
		      (recur (rest args) (conj normalized (list 'exists r new-name)) new-names))))))))))))

(defn- tbox-from-names
  "Creates and returns a tbox from given names and language."
  [language names]
  (make-tbox language (set-of (make-dl-definition A (make-dl-expression language def-A))
			      [[def-A A] names])))

(defn normalize
  "Normalizes given tbox."
  [tbox]
  (let [language (tbox-language tbox),
	[normalized-definitions new-names] (reduce (fn [[n-definitions names] definition]
						     (let [[n-definition new-names] (normalize-definition definition names)]
						       [(conj n-definitions n-definition) new-names]))
						   [#{} {}]
						   (tbox-definitions tbox))]
    (if (empty? new-names)
      (make-tbox language normalized-definitions)
      (let [names-tbox (normalize (tbox-from-names language new-names))]
	(make-tbox language (union normalized-definitions
				   (tbox-definitions names-tbox)))))))

(defn tbox->description-graph
  "Converts a tbox to a description graph."
  [tbox]
  (let [normalized-tbox (normalize tbox),
	definitions     (tbox-definitions tbox),

	language        (tbox-language tbox),
	vertices        (defined-concepts tbox),
	neighbours      (into {} (for [def definitions]
				   [(definition-target def)
				    (set (map (comp vec arguments)
					      (filter compound?
						      (arguments (definition-expression def)))))])),
	vertex-labels   (into {} (for [def definitions]
				   [(definition-target def),
				    (set (filter atomic?
						 (arguments (definition-expression def))))]))]
    (make-description-graph language vertices neighbours vertex-labels)))

(defn description-graph->tbox
  "Converts a description graph to a tbox."
  [description-graph]
  (let [language    (graph-language description-graph),
	labels      (vertex-labels description-graph),
	neighbours  (neighbours description-graph),

	definitions (set-of (make-dl-definition A def-exp)
			    [A (vertices description-graph)
			     :let [def-exp (make-dl-expression language
							       (list* 'and
								      (concat (labels A)
									      (for [[r B] (neighbours A)]
										(list 'exists r B)))))]])]
    (make-tbox language definitions)))

(defn model->description-graph
  "Converts given model to a description graph."
  [model]
  (let [language            (model-language model),
	base-interpretation (model-base-interpretation model),

	vertices            (model-base-set model),
	neighbours          (hashmap-by-function (fn [x]
						   (set-of [r y] [r (role-names language),
								  y (model-base-set model),
								  :when (contains? (base-interpretation r) [x y])]))
						 (model-base-set model)),
	vertex-labels       (hashmap-by-function (fn [x]
						   (set-of P [P (concept-names language),
							      :when (contains? (base-interpretation P) x)]))
						 (model-base-set model))]
    (make-description-graph language vertices neighbours vertex-labels)))

;;;

nil
