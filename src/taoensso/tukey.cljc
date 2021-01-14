(ns taoensso.tukey
  "Simple stats toolkit for Clojure/Script.

  Objectives:
    - Pure Clojure/Script implementation.
    - Pleasant+consistant API w/ beginner-friendly documentation.
    - Start with basic, commonly used utils. Can expand over time.
    - Start with short, understandable algorithms. Can optimise over time.
    - Have some fun :-)

  Refererences:
    [R1] A Computer Method for Calculating Kendall's Tau with Ungrouped Data
         by William R. Knight, Journal of the American Statistical Association
         Vol. 61, No. 314, Part 1 (Jun., 1966), pp. 436-439 (4 pages).

    [R2] Efficiently Computing Kendall's Tau by Matt Adereth,
         http://adereth.github.io/blog/2013/10/30/efficiently-computing-kendalls-tau/"

  {:author "Peter Taoussanis (@ptaoussanis)"}
  (:require
   [taoensso.encore     :as enc  :refer-macros []]
   [taoensso.tukey.impl :as impl :refer-macros []
    :refer #?(:clj [] :cljs [DoublePair DoubleTrip])]

   [clojure.test        :as test :refer [is]])

  #?(:cljs (:require-macros [taoensso.tukey :refer []]))
  #?(:clj  (:import [taoensso.tukey.impl SortedLongs SortedDoubles
                     DoublePair DoubleTrip])))

(enc/assert-min-encore-version [3 9 1])

(comment
  (remove-ns      'taoensso.tukey)
  (test/run-tests 'taoensso.tukey))

;; "∴ ∵ ℕ ℤ ℝ ∞ ≠ ∈ ∉ ⇒⇔ → × ⊃⊂ ⊇⊆ ≡ ¬ ∀ ∃ ∝"

;;;; TODO
;; - Imports
;;   - tl-core, classification, Nippy, ucb1
;;   - Incanter, other libs?
;; - T, Z tests (1+2 sided?)
;;
;; - Use from Tufte

;; - Polish
;;   - ^double + throw rather than ?double ?
;;   - Consistent naming scheme
;;   - Nice, helpful docstrings with references
;;   - Nice error messages + checks

;;;; Scratch
;; Parametric tests               Non-parametric equivalents
;; 1-sample t test                Wilcoxin Rank-Sum test
;; Independent t test             Mann-Whitney U test
;; Paired samples t test          Wilcoxin Test
;; 1-way Independent ANOVA        Kruskal-Wallis test
;; 1-way Repeated measures ANOVA  Friedman test
;;                                Chi-square test: does my sample look like the population?
;;
;;     Parametric tests: Pearson
;; Non-parametric tests: Spearman, Kendall
;;  non-parametric => doesn't depend on assumptions about distributions of X or Y
;; esp. good for nominal or ordinal data
;; not sensitve to outliers than parametric tests
;; ranking uses median rather than mean

;;;; Misc

(enc/defalias enc/approx==)

;;;; Sorting

(enc/defalias impl/sorted-longs)
(enc/defalias impl/sorted-doubles)

(enc/defalias impl/sorted-longs?)
(enc/defalias impl/sorted-doubles?)

(comment (sorted? (sorted-longs [1.0 2 3.0])))

;;;; Combinatorics

(let [cache_ (atom {0 1, 1 1})]
  (defn ^{:cache_ cache_} factorial
    "Returns factorial n! ∈ ℕ = n∙(n-1)∙(n-2)...3∙2∙1, caching each step.
    O(n!). May auto-promote to BigInt for Clj."
    ;; Implementation here mostly just for fun :-)
    [n]
    ;; (println)
    (let [n (long n)]
      (if-let [cached (get @cache_ n)]
        cached
        (let [_
              (if (< n 0)
                (throw (ex-info "`factorial`: need n>0" {:n n}))
                #?(:cljs
                   (when (> n 17) ; Needs BigInt
                     (throw (ex-info "`factorial`: need n<=17" {:n n})))))

              ;; Seek back from n to find best cached `idx!`
              [^long best-starting-idx best-starting-acc]
              (let [cache @cache_]
                (or
                  (reduce
                    (fn [_ n]
                      (when-let [cached (get cache n)]
                        (reduced [n cached])))
                    nil
                    (range n 0 -1))
                  [1 1]))

              ;; Work forward from best cached `idx!` to cache and build result
              [result -new-cache]
              (loop [idx (inc best-starting-idx)
                     acc      best-starting-acc
                     new-cache (transient {})]

                (let [new-acc   #?(:clj (*' acc idx) :cljs (* acc idx))
                      new-cache (assoc! new-cache idx new-acc)]

                  (if (== idx n)
                    [new-acc new-cache]
                    (recur (inc idx) new-acc new-cache))))]

          (swap! cache_ conj (persistent! -new-cache))
          result)))))

(test/deftest test-factorial
  (is (enc/thrown (factorial -2)))
  (is (=          (factorial  5) 120))
  (is (=          (factorial  9) 362880))
  (is (=          (factorial 14) 87178291200))
  (is (=          (factorial 17) 355687428096000))
  #?(:clj  (is (= (factorial 21) 51090942171709440000N)))
  #?(:cljs (is (enc/thrown (factorial 18)))))

(comment
  (count @(:cache_ (meta #'factorial)))
  (reset! (:cache_ (meta #'factorial)) {}))

(enc/defalias impl/reduce-combinations) ; Main combinatorial fn

(defn reduce-permutations
  "Like `reduce-combinations`, but always ordered."
  ([      rfn init                  xs] (reduce-combinations nil   rfn init {:ordered? true}   (count xs) xs))
  ([xform rfn init           n-take xs] (reduce-combinations xform rfn init {:ordered? true}       n-take xs))
  ([xform rfn init repeated? n-take xs] (reduce-combinations xform rfn init {:ordered? true
                                                                             :repeated? repeated?} n-take xs)))

(defn combinations
  "TODO Nice docstring!"
  ([                   n-take xs] (combinations {:n-take n-take :xs xs}))
  ([repeated? ordered? n-take xs] (combinations {:n-take n-take :xs xs
                                                 :repeated? repeated?
                                                 :ordered?  ordered?}))

  ([{:keys [repeated? ordered? n-take xs] :as opts}]
   (persistent! (reduce-combinations nil conj! (transient []) opts n-take xs))))

(test/deftest test-combinations
  (is (= (count (combinations {:n-take 3 :xs [:a :b :c :d]                  }))   4))
  (is (= (count (combinations {:n-take 3 :xs [:a :b :c :d] :repeated? true  }))  20))
  (is (= (count (combinations {:n-take 5 :xs [:a :b :c :d :e :f :g :h :i :j]})) 252)))

(defn permutations
  "Like `combinations`, but always ordered."
  ([                 xs] (combinations {:ordered? true :xs xs :n-take (count xs)}))
  ([          n-take xs] (combinations {:ordered? true :xs xs :n-take n-take}))
  ([repeated? n-take xs] (combinations {:ordered? true :xs xs :n-take n-take
                                        :repeated? repeated?})))

(test/deftest test-permutations
  (is (= (count (permutations             [:a :b :c :d]                  ))    24))
  (is (= (count (permutations :repeated 3 [:a :b :c :d]                  ))    64))
  (is (= (count (permutations nil       5 [:a :b :c :d :e :f :g :h :i :j])) 30240)))

(comment
  (require '[clojure.math.combinatorics :as combo])

  (enc/qb 1e2 ; [199.54 268.94 209.65]
    (count (permutations            [:a :b :c :d :e :f :g]))
    (count (vec (combo/permutations [:a :b :c :d :e :f :g])))
    (count (vec (combo/permutations [ 1  2  3  4  5  6  7]))))

  (enc/qb 1e4 ; [126.13 159.55 152.79]
    (count (combinations 3      [:a :b :c :d :e :f :g]))
    (count (combo/combinations  [:a :b :c :d :e :f :g] 3))
    (count (combo/combinations  [ 1  2  3  4  5  6  7] 3))))

(defn n-combinations
  "Like `combinations`, but only returns the cardinality of the result.
  May auto-promote to BigDecimal for Clj."
  ([                   n-take n-xs] (n-combinations {:n-take n-take :n-xs n-xs}))
  ([repeated? ordered? n-take n-xs] (n-combinations {:n-take n-take :n-xs n-xs
                                                     :repeated? repeated?
                                                     :ordered?  ordered?}))
  ([{:keys [repeated? ordered? n-take n-xs]}]
   ;; Ref. https://www.mathsisfun.com/combinatorics/combinations-permutations-calculator.html
   (let [r  (long n-take)
         n  (long n-xs)
         !  factorial
         *' #?(:clj *' :cljs *)]

     ;; TODO Symbolic factorial elimination for fun?

     (if repeated?
       (if ordered?
         (impl/pow' n r) ; n^r
         (/ ; (r+n-1)! / r! (n-1)!
           (! (dec (+ r n)))
           (*' (! r) (! (dec n)))))

       (if ordered?
         (/ (! n)           (! (- n r)))  ; n! /    (n-r)!
         (/ (! n) (*' (! r) (! (- n r)))) ; n! / r! (n-r)!
         )))))

(test/deftest test-n-combinatorics
  (is (= (n-combinations {:n-take 5 :n-xs 10 :repeated? true  :ordered? true}) 100000))
  (is (= (n-combinations {:n-take 5 :n-xs 10 :repeated? true  :ordered? false})  2002))
  (is (= (n-combinations {:n-take 5 :n-xs 10 :repeated? false :ordered? true})  30240))
  (is (= (n-combinations {:n-take 5 :n-xs 10 :repeated? false :ordered? false})   252)))

(defn n-permutations
  "Like `permutations`, but only returns the cardinality of the result.
  May auto-promote to BigDecimal for Clj."
  ([                 n-xs] (n-combinations {:ordered? true :n-xs n-xs :n-take (count n-xs)}))
  ([          n-take n-xs] (n-combinations {:ordered? true :n-xs n-xs :n-take n-take}))
  ([repeated? n-take n-xs] (n-combinations {:ordered? true :n-xs n-xs :n-take n-take
                                            :repeated? repeated?})))

;;;; Summary statistics

(defn- mean-sum "Returns [<n> <sum>]"
  [op init xs]
  (if (counted? xs)
    [(count xs) (double (reduce (fn [^double acc x] (op acc (double x))) init xs))]

    (let [n_ (enc/counter)]
      [@n_ (double (reduce (fn [^double acc x] (n_) (op acc (double x))) init xs))])))

(test/deftest test-mean-sum
  (is (= (mean-sum + 0.0 [2 3 4 5]) [4 14.0])))

(defn arithmetic-mean
  "Returns the arithmetic mean x̄ ∈ ℝ of the given values (n>0)."
  ;; TODO Compare to others
  ^double [xs]
  (let [[n sum] (mean-sum + 0.0 xs)]
    (/
      (double sum)
      (impl/is-not-zero "`mean`: need >0 values" {:xs xs}
        (count xs)))))

(test/deftest test-arithmetic-mean
  (is (enc/thrown (arithmetic-mean [])))
  (is (=          (arithmetic-mean [2 3 4 2]) 2.75)))

(enc/defalias mean arithmetic-mean)

(defn geometric-mean
  "TODO Docstring, compare to others"
  ^double [xs]
  (let [[n sum] (mean-sum * 1.0 xs)]
    (Math/pow sum
      (/ 1.0
        (impl/is-not-zero "`geometric-mean`: need >0 values" {:xs xs}
          n)))))

(test/deftest test-geometric-mean
  (is (enc/thrown (geometric-mean [])))
  (is (approx==   (geometric-mean [1 1 1 2]) 1.189)))

(defn median
  "TODO Docstring, compare to others"
  ^double [xs]
  (impl/is-not-zero "`median`: need >0 values" {:xs xs} (count xs))
  (let [xs (impl/sorted-doubles xs)]
    (impl/sorted-percentile 0.5 xs)))

(test/deftest test-median
  (is (enc/thrown (median [])))
  (is (= (median [2          ]) 2.0))
  (is (= (median [1 2 3.1 4 5]) 3.1))
  (is (= (median [1 2     4 5]) 3.0)))

(defn- variance-sum-rf
  [^double xbar ^double acc x]
  (+ acc (Math/pow (- (double x) xbar) 2.0)))

(defn variance-sum
  "Returns the sum ∑ (x - x̄)² ∈ ℝ of the given values."
  ^double [xs]
  (let [xbar (mean xs)]
    (double
      (reduce (partial variance-sum-rf xbar)
        0.0 xs))))

(enc/defonce ^:dynamic *bessel-correction* "TODO Docstring" true)
(defn- bessel-correction ^double [n ^double add]
  (if *bessel-correction*
    (+  (double n) add)
    (do (double n))))

(comment (bessel-correction 5 -1.0))

(defn variance
  "Returns the variance σ² ∈ ℝ of the given values (n>1)."
  ^double [xs]
  (/
    (variance-sum xs)
    (impl/is-not-zero "`variance`: need more values" {:xs xs}
      (bessel-correction (count xs) -1.0))))

(test/deftest test-variance
  (is (enc/thrown (variance [1])))
  (is (approx==   (variance [1 2 4]) 2.333)))

(defn arithmetic-std-deviation
  "Returns the standard deviation σ ∈ ℝ of the given values (n>1)."
  ^double [xs] (Math/sqrt (variance xs)))

(comment (arithmetic-std-deviation [1 2 4]))

(enc/defalias std-deviation arithmetic-std-deviation)

(defn- abs-deviation-sum-rf
  [^double central-point ^double acc x]
  (+ acc (Math/abs (- (double x) central-point))))

(defn abs-deviation-sum
  "Returns the sum ∑ |x - cp| ∈ ℝ of the given values."
  (^double [              xs] (abs-deviation-sum :mean xs))
  (^double [central-point xs]
   (let [central-point
         (double
          (case central-point
            (:mean :arithmetic-mean) (mean   xs)
            :median                  (median xs)
            central-point))]

     (reduce (partial abs-deviation-sum-rf central-point)
       0.0 xs))))

(defn mean-abs-deviation
  "Returns the mean absolute deviation MAD ∈ ℝ of the given values (n>0)
  from a central point (:mean, :median, or arbitrary val)."
  (^double [              xs] (mean-abs-deviation :mean xs))
  (^double [central-point xs]
   (/
     (abs-deviation-sum central-point xs)
     (impl/is-not-zero "`mean-abs-deviation`: need >0 values"
       {:xs xs :central-point central-point}
       (count xs)))))

(test/deftest test-mean-abs-deviation
  (is (enc/thrown (mean-abs-deviation [])))
  (is (approx==   (mean-abs-deviation :mean [1 1 1 1 1 1 2]) 0.2449)))

(defn covariance
  "Returns the covariance σ(x,y) ∈ ℝ between the given value colls of
  same size (>1)."
  ^double [xs ys]
  (let [xs (vec xs)
        ys (vec ys)
        n  (impl/is-same-count :covariance xs ys)

        xbar (mean xs)
        ybar (mean ys)

        sum
        (double
          (reduce
            (fn [acc idx]
              (let [xi (double (get xs idx))
                    yi (double (get ys idx))]
                (+ (double acc) (* (- xi xbar) (- yi ybar)))))
            0.0 (range 0 n)))]

    (/ sum
      (impl/is-not-zero "`covariance`: need more values" {:xs xs :ys ys}
        (bessel-correction n -1.0)))))

(test/deftest test-covariance
  (is (enc/thrown (covariance [   ] [])))
  (is (enc/thrown (covariance [1 2] [])))
  (is (approx==   (covariance [1 2 3 4] [ 1  2  3  4])  1.6667))
  (is (approx==   (covariance [1 2 3 4] [-1 -2 -3 -4]) -1.6667))
  (is (approx==   (binding [*bessel-correction* false]
                    (covariance [1 2 3 4] [ 1  2  3  4])) 1.25)))

(defn percentiles
  "Returns ?[min p25 p50 p75 p90 p95 p99 max] doubles in:
    - O(1) for Sorted types (SortedLongs, SortedDoubles),
    - O(n) otherwise."
  [xs]
  (let [max-idx (dec (count xs))]
    (when (>= max-idx 0)
      (let [xs (if (impl/sorted-fixnums? xs) xs (impl/sorted-doubles xs))]
        [(double (nth xs 0))
         (impl/double-nth xs (* max-idx 0.25))
         (impl/double-nth xs (* max-idx 0.50))
         (impl/double-nth xs (* max-idx 0.75))
         (impl/double-nth xs (* max-idx 0.90))
         (impl/double-nth xs (* max-idx 0.95))
         (impl/double-nth xs (* max-idx 0.99))
         (double (nth xs max-idx))]))))

(test/deftest test-percentiles
  (is (= (percentiles                 [1 5 2 4 3])  [1.0 2.0 3.0 4.0 4.6 4.8 4.96 5.0]))
  (is (= (percentiles (sorted-doubles [1 5 2 4 3])) [1.0 2.0 3.0 4.0 4.6 4.8 4.96 5.0]))
  (is (= (percentiles (sorted-longs   [1 5 2 4 3])) [1.0 2.0 3.0 4.0 4.6 4.8 4.96 5.0])))

(defn ranks<=
  "Returns the number of ranks ∈ ℕ of `xs` <= given `y` in O(log_n)."
  ^long [xs y]
  (let [xs (if (impl/sorted-fixnums? xs) xs (impl/sorted-doubles xs))
        n  (count  xs)
        y  (double y)]

    ;; Binary search to find idx of first x>y
    (loop [l 0 r n]
      (if (== l r)
        0
        (let [idx (long (/ (+ l r) 2))
              x   (double (nth xs idx))]
          ;; (println {:l l :r r :idx idx :x x})
          (enc/cond
            (>= y x) (let [l* idx] (if (== l* l) (inc idx) (recur l* r)))
            (<  y x) (let [r* idx] (if (== r* r) (inc idx) (recur l  r*)))))))))

(test/deftest test-ranks<=
  (is (= (ranks<= [               ]  10) 0))
  (is (= (ranks<= [1              ]  10) 1))
  (is (= (ranks<= [1              ] -10) 0))
  (is (= (ranks<= [1 1 1 2 2 3 4 4] 2.5) 5)))

(defn percentile-rank
  "Returns the percentage ∈ ℝ of `xs` <= given `y`."
  ^double [xs y]
  (let [xs (if (impl/sorted-fixnums? xs) xs (impl/sorted-doubles xs))]
    (/
      (* (double (ranks<= xs y)) 100.0)
      (impl/is-not-zero "`percentile-rank`: need >0 values"
        {:xs xs :y y} (count xs)))))

(test/deftest test-percentile-rank
  (is (enc/thrown (percentile-rank [               ]  10)))
  (is (=          (percentile-rank [1              ]  10) 100.0))
  (is (=          (percentile-rank [1              ] -10) 0.0))
  (is (=          (percentile-rank [1 1 1 2 2 3 4 4] 2.5) 62.5)))

(enc/defalias impl/min-max)

(defn- double-sum-rf [^double acc ^double in] (+ acc in))

;; Member names chosen to avoid shadowing
(defrecord MergeableStats [nx xmin xmax xsum xmean xvar-sum xmad-sum xvar xmad p25 p50 p75 p90 p95 p99])
(defn mergeable-stats
  "TODO Docstring common summary stats in a form that can be merged with `merge-stats`."
  [xs]
  (when xs
    (let [sorted-xs (impl/sorted-doubles xs)
          nx (count sorted-xs)]

      (when-not (zero? nx)
        (let [xsum (double (reduce double-sum-rf 0.0 sorted-xs))
              xbar (/ xsum (double nx))

              [^double xvar-sum ^double xmad-sum]
              (impl/multi-reduce
                (partial variance-sum-rf      xbar) 0.0
                (partial abs-deviation-sum-rf xbar) 0.0
                sorted-xs)

              xvar (when (> nx 1) (/ xvar-sum (bessel-correction nx -1.0)))
              xmad                (/ xmad-sum                    nx)

              [xmin p25 p50 p75 p90 p95 p99 xmax]
              (percentiles sorted-xs)]

          (->MergeableStats
            nx xmin xmax xsum xbar xvar-sum xmad-sum xvar xmad
            p25 p50 p75 p90 p95 p99))))))

(comment (mergeable-stats (impl/rand-longs 100)))

(enc/defalias stats mergeable-stats)

(defn merge-stats
  "(merge-stats (mergeable-stats xs1) (mergeable-stats xs2)) returns a basic
  approximation of (mergeable-stats (into xs1 xs2))).

  This is useful when dealing with large numbers of values (i.e. when it's
  expensive/infeasible to keep all of `xs1` and `xs2` for accurate merging).

  Compare data size:
    -                  xs  is O(n)
    - (mergeable-stats xs) is O(1)"

  [m1 m2]
  (if m1
    (if m2
      (let [_ (assert (get m1 :nx))
            _ (assert (get m2 :nx))

            {^long   nx1       :nx
             ^double xmin1     :xmin
             ^double xmax1     :xmax
             ^double xsum1     :xsum
             ^double xvar-sum1 :xvar-sum
             ^double xmad-sum1 :xmad-sum
             ^double p25-1     :p25
             ^double p50-1     :p50
             ^double p75-1     :p75
             ^double p90-1     :p90
             ^double p95-1     :p95
             ^double p99-1     :p99} m1

            {^long   nx2       :nx
             ^double xmin2     :xmin
             ^double xmax2     :xmax
             ^double xsum2     :xsum
             ^double xvar-sum2 :xvar-sum
             ^double xmad-sum2 :xmad-sum
             ^double p25-2     :p25
             ^double p50-2     :p50
             ^double p75-2     :p75
             ^double p90-2     :p90
             ^double p95-2     :p95
             ^double p99-2     :p99} m2

            _ (assert (pos? nx1))
            _ (assert (pos? nx2))

            nx3       (+ nx1 nx2)
            nx1-ratio (/ (double nx1) (double nx3))
            nx2-ratio (/ (double nx2) (double nx3))

            xsum3 (+ xsum1 xsum2)
            xbar3 (/ (double xsum3) (double nx3))
            xmin3 (if (< xmin1 xmin2) xmin1 xmin2)
            xmax3 (if (> xmax1 xmax2) xmax1 xmax2)

            ;; Batched "online" calculation here is better= the standard
            ;; Knuth/Welford method, Ref. http://goo.gl/QLSfOc,
            ;;                            http://goo.gl/mx5eSK.
            ;; No apparent advantage in using `xbar3` asap (?).
            xvar-sum3 (+ xvar-sum1 xvar-sum2)
            xmad-sum3 (+ xmad-sum1 xmad-sum2)

            ;;; These are pretty rough approximations. More sophisticated
            ;;; approaches not worth the extra cost/effort in our case.
            p25-3 (+ (* nx1-ratio (double p25-1)) (* nx2-ratio (double p25-2)))
            p50-3 (+ (* nx1-ratio (double p50-1)) (* nx2-ratio (double p50-2)))
            p75-3 (+ (* nx1-ratio (double p75-1)) (* nx2-ratio (double p75-2)))
            p90-3 (+ (* nx1-ratio (double p90-1)) (* nx2-ratio (double p90-2)))
            p95-3 (+ (* nx1-ratio (double p95-1)) (* nx2-ratio (double p95-2)))
            p99-3 (+ (* nx1-ratio (double p99-1)) (* nx2-ratio (double p99-2)))

            xvar3 (when (> nx3 2) (/ xvar-sum3 (bessel-correction nx3 -2.0)))
            xmad3                 (/ xmad-sum3                    nx3)]

        (->MergeableStats
          nx3 xmin3 xmax3 xsum3 xbar3 xvar-sum3 xmad-sum3 xvar3 xmad3
          p25-3 p50-3 p75-3 p90-3 p95-3 p99-3))
      m1)
    m2))

(defn- stats-approx== [signf m1 m2]
  (reduce-kv
    (fn [acc k v]
      (if (approx== signf v (get m2 k))
        true
        (reduced false)))
    true
    m1))

(comment (map-approx== 0.001 {:a 100 :b 100} {:a 100 :b 100.0001}))

(test/deftest test-mergeable-stats
  (is (stats-approx== 0.1 (stats (range 1000))
        {:nx 1000, :xmin 0.0, :xmax 999.0, :xsum 499500.0, :xmean 499.5,
         :xvar-sum 8.333325E7, :xmad-sum 250000.0, :xvar 83416.66666666667, :xmad 250.0,
         :p25 250.0, :p50 500.0, :p75 749.0, :p90 899.0, :p95 949.0, :p99 989.0}))

  (is (stats-approx== 0.1
        (merge-stats
          (stats (range 0   900))
          (stats (range 200 500)))

        {:nx 1200, :xmin 0.0, :xmax 899.0, :xsum 509400.0, :xmean 424.5,
         :xvar-sum 6.29999E7, :xmad-sum 225000.0, :xvar 52587.562604340565, :xmad 187.5,
         :p25 237.5, :p50 425.0, :p75 611.5, :p90 724.0, :p95 761.5, :p99 791.5})))

(comment
  (defn compare-stats [xs1 xs2]
    (let [m1 (mergeable-stats xs1)
          m2 (mergeable-stats xs2)]

      {:merged (into {} (merge-stats m1 m2))
       :direct (into {} (mergeable-stats (into xs1 xs2)))}))

  (compare-stats (impl/rand-longs 100) (impl/rand-longs 100))

  {:merged {:xmax 99.0, :xmin -99.0, :p75 44.5, :p99 98.0, :xvar 3477.6212121212134, :xmad 49.83580000000002, :p25 -42.5, :nx 200, :p90 84.0, :xmad-sum 9967.160000000003, :xvar-sum 688569.0000000002, :xmean -1.57, :xsum -314.0, :p50 -5.0, :p95 94.5},

   :direct {:xmax 99.0, :xmin -99.0, :p75 46.0, :p99 99.0, :xvar 3462.4372864321613, :xmad 49.967199999999906, :p25 -43.0, :nx 200, :p90 88.0, :xmad-sum 9993.43999999998, :xvar-sum 689025.0200000001, :xmean -1.57, :xsum -314.0, :p50 -5.0, :p95 95.0}})

;;;; Correlation

(defn pearson-correlation
  "Returns Pearson product-moment correlation coefficient (PPMCC) r ∈ ℝ[-1,+1]
  between the given value colls of same size (>1). O(n).

  Measures the strength and direction of relationship between `xs` and `ys`.

  Properties:
    - Parametric measure.
    - Supported value types: continuous.
    - Supported relationships: linear.

  See also `spearman-correlation`, `kendall-correlation`."
  ^double [xs ys]
  (/
    (covariance xs ys)
    (*
      (std-deviation xs)
      (std-deviation ys))))

(test/deftest test-pearson-correlation
  (is
    (approx== 0.041 ; https://www.socscistatistics.com/tests/pearson/
      (pearson-correlation
        [22 -46 -63 -12 -12 36 72 -83 21 93 31 23 94 22 6]
        [56 -30 74 54 -5 30 -66 -48 32 -49 71 71 71 64 -35]))))

(defn mean-ranks
  "Returns ranks ∈ ℝ for the given values, with ties as mean rank:
    (mean-ranks [0  1   2]) -> [0.0 1.0 2.0]
    (mean-ranks [0 10 200]) -> [0.0 1.0 2.0]
    (mean-ranks [0 10  10]) -> [0.0 1.5 1.5]"
  [xs]
  (let [xform ; {<val> <mean-rank>}
        (let [idx_ (enc/counter)]
          (persistent!
            (transduce
              (partition-by identity) ; TODO Optimise?
              (completing
                (fn [m vs]
                  (let [[v1] vs
                        mean-idx
                        (let [i0 (long (idx_ (count vs)))
                              in (dec  (long @idx_))]
                          (/ (+ in i0) 2.0))]
                    (assoc! m v1 mean-idx))))
              (transient {})
              (sort xs))))]

    (mapv xform xs)))

(test/deftest test-mean-ranks
  (is (= (mean-ranks [0  1   2    ]) [0.0 1.0 2.0]))
  (is (= (mean-ranks [0 10 200    ]) [0.0 1.0 2.0]))
  (is (= (mean-ranks [0 10  10    ]) [0.0 1.5 1.5]))
  (is (= (mean-ranks [2 33 6 55 -2]) [1.0 3.0 2.0 4.0 0.0])))

(defn spearman-correlation
  "Returns Spearman rank correlation coefficient rho ρ ∈ ℝ[-1,+1] between the
  given value colls of same size (>1). O(n).

  Measures the strength and direction of relationship between `xs` and `ys`.

  Properties:
    - Non-parametric measure.
    - Supported value types: continuous, nominal/ordinal.
    - Supported relationship: linear, monotonic.

  A good general-purpose choice as correlation measure if you're not confident
  about the probability distribution of your random variables, etc.

  Calculated as Pearson correlation of value ranks, with ties as mean rank.
  Will have a similar coefficient to Pearson for tame data, but is more robust
  to outliers, and more general (supports any monotonic relationship, not just
  linear).

  See also `kendall-correlation`, `pearson-correlation`."
  ^double [xs xy]
  (let [rxs (mean-ranks xs)
        rys (mean-ranks xy)]
    (pearson-correlation rxs rys)))

(test/deftest test-spearman-correlation
  (approx== -0.02511 ; https://www.socscistatistics.com/tests/spearman/
    (spearman-correlation
      [72 11 11 -28 80 -94 -26 76 56 -87 -63 -61 30 67 -30]
      [-75 51 -36 20 -99 -22 62 52 52 52 -93 -57 -47 -12 -89])))

(enc/defalias impl/joint-vals)

(defn kendall-correlation
  "Returns Kendall rank correlation coefficient tau τ ∈ ℝ[-1,+1] between the
  given value colls of same size (>0). O(n.log_n) algorithm.

  Measures the strength and direction of relationship between `xs` and `ys`.

  Properties:
    - Non-parametric measure.
    - Supported value types: continuous, nominal/ordinal.
    - Supported relationship: linear, monotonic.

  A good general-purpose choice as correlation measure if you're not confident
  about the probability distribution of your random variables, etc.

  Similar to Spearman correlation but:
    - [Pro] More robust toward ties.
    - [Pro] Easier intuitive interpretation (comparing `concordant` and
      `discordant` pairs of values).
    - [Neutral] Less sensitive to a small number of large discrepancies. May
      be good or bad depending on your needs.
    - [Con] O(n.log_n) vs O(n).

  Supports 3 `kind`s:
    - :tao-a ; Simplest implementation.
    - :tao-b ; Preferred if values may contain ties.
    - :tao-c ; Preferred if `xs`, `ys` have uneven number of
               unique values.

  See also `spearman-correlation`, `pearson-correlation`."

  ([xs ys] (kendall-correlation nil xs ys))

  ([{:keys [kind impl]
     :or   {kind :tao-b
            impl :auto}}
    xs ys]

   (let [xys (joint-vals xs ys)
         _   (impl/is-not-zero "`kendall-correlation`: need >0 values"
               {:xs xs :ys ys} (count xys))

         {:keys [n_xt n_yt n_xyt n_p n_c-n_d]}
         (let [;; :naive is faster for v. small inputs
               impl (case impl :auto (if (>= (count xys) 11) :knight :naive) impl)]

           (case impl
             :knight (impl/kendall-correlation-impl-knight                    xys)
             :naive  (impl/kendall-correlation-impl-naive reduce-combinations xys)))]

     (case kind
       (:tao-a :a) (/ (double n_c-n_d) (double n_p))

       (:tao-b :b)
       (/
         (double n_c-n_d)
         (Math/sqrt
           (double
             (*
               (- (double n_p) (double n_xt))
               (- (double n_p) (double n_yt))))))

       (:tao-c :c)
       (let [n_rows (count (set xs))
             n_cols (count (set ys))
             n_min  (enc/min* n_rows n_cols)]
         (/
           (* 2.0 (double n_c-n_d))
           (/
             (* (Math/pow (count xs) 2.0) (- n_min 1.0))
             n_min)))))))

(test/deftest test-kendall-correlation
  (is (approx== (kendall-correlation {:kind :tao-a} [1 2 3 3 4 5] [3 2 5 6 6 7]) 0.7333))
  (is (approx== (kendall-correlation {:kind :tao-b} [1 2 3 3 4 5] [3 2 5 6 6 7]) 0.7857))
  (is (approx== (kendall-correlation {:kind :tao-c} [1 2 3 3 4 5] [3 2 5 6 6 7]) 0.7638))
  (is (approx== 0.03864 ; https://www.wessa.net/rwasp_kendall.wasp
        (kendall-correlation
          [-46 -60 -61 69 51 51 58 98 -17 65 41 73 33 -20 -81]
          [-73 -86 93 -55 -46 19 -54 77 -80 -80 29 29 -26 -36 40]))))

(comment
  (enc/qb 1e4 ; [96.13 157.79 89.0]
    (kendall-correlation {:impl :auto}   [1 2 3 3 4 5] [3 2 5 6 6 7])
    (kendall-correlation {:impl :knight} [1 2 3 3 4 5] [3 2 5 6 6 7])
    (kendall-correlation {:impl :naive}  [1 2 3 3 4 5] [3 2 5 6 6 7]))

  (let [xs (impl/rand-longs 400)
        ys (impl/rand-longs 400)]
    (enc/qb 1e1 ; [528.65 19.7 4.78 0.58]
      (kendall-correlation {:impl :naive}  xs ys)
      (kendall-correlation {:impl :knight} xs ys)
      (spearman-correlation                xs ys)
      (pearson-correlation                 xs ys))))

;;;; Probabilities

;;; TODO Document
(enc/defalias enc/pnum?)
(defn pcompl ^double [p]                  (- 1.0 (double p)))
(defn psignf ^double [p] (Math/abs        (- 0.5 (double p))))
(defn pclamp ^double [p] (enc/clamp* 0.001 0.999 (double p)))
(defn pneutralize ^double [p pconfidence]
  (let [p           (double p)
        pconfidence (double pconfidence)]
    (enc/clamp* 0.0 1.0
      (+
        (* p           pconfidence)
        (* 0.5 (pcompl pconfidence))))))

(comment (pneutralize 0.9 0.6))

;;;; Chi square

(defn inverse-chi-square
  "Alpha API, subject to change.

  Returns probability ∈ ℝ[0,1] of obtaining a value >= given x in a
  chi-square distribution with given degrees of freedom.

  Beautifully simple implementation originally written in Python by
  Gary Robinson; translated to CL by Peter Siebel; translated to Clojure by
  Gavin McGovern. Finally rewritten by me as a single `reduce` for better
  performance.

  Note that this algorithm rounds degrees-of-freedom to nearest multiple of 2,
  an acceptable tradeoff for speed+simplicity for classification purposes,
  Ref. http://garyrob.blogs.com/chi2p.py"
  ^double [value degrees-of-freedom]
  (let [value (double value)
        dof   (double degrees-of-freedom)
        m     (/ value 2.0)
        init  (Math/exp (- m))

        ^DoublePair dp ; [<acc> <acc-sum>]
        (reduce
          (fn [^DoublePair dp in]
            (let [acc     (.-x dp)
                  acc-sum (.-y dp)
                  acc (* acc (/ m (double in)))]
              (DoublePair. acc (+ acc-sum acc))))
          (DoublePair. init init)
          (range 1 (quot dof 2)))]

    ;; Ensure ∈[0,1] despite imprecise arithmetic
    (enc/min* 1.0 (.-y dp))))

(test/deftest test-inverse-chi-square
  ;; Ref. http://goo.gl/fBdre
  (is (approx== (inverse-chi-square 0   1) 1.0))
  (is (approx== (inverse-chi-square 0.5 1) 0.778))
  (is (approx== (inverse-chi-square 1   1) 0.6065))
  (is (approx== (inverse-chi-square 3   1) 0.2231))
  (is (approx== (inverse-chi-square 3   3) 0.2231))
  (is (approx== (inverse-chi-square 3   6) 0.8088))
  (is (approx== (inverse-chi-square 3  18) 0.999)))

(comment (enc/qb 1e5 (inverse-chi-square 3 3))) ; 21.21

(defn fisher-combination
  "Alpha API, subject to change.

  Returns 2-sided Fisher chi-square combined probability ∈ ℝ[0,1] for given
  probabilities of form [<p ∈ ℝ[0,1]> <freq/weight ∈ ℝ[0,∞]>].

  2-sided combination done as per Gary Robinson (Ref. http://goo.gl/eyb2H) to
  help produce a value near 0.5 (neutral) when classifying content that has
  strong evidence in both directions (e.g. of both spamminess and hamminess).

  TODO Explain more"
  ^double [pfs]
  (if (empty? pfs)
    0.5 ; Common-case optimization
    (let [^DoubleTrip dt ; [<p-sum> <pcompl-sum> <f-sum>]
          (reduce
            (fn [^DoubleTrip dt in]
              (if (nil? in) ; Ignore nil `?pfs`
                dt
                (let [[p f] in
                      p (double p)
                      f (double f)

                      p-sum      (.-x dt)
                      pcompl-sum (.-y dt)
                      f-sum      (.-z dt)]

                  (when (or (< p 0.0) (> p 1.0))
                    (throw
                      (ex-info "`fisher-combination`: `p`s in `pfs` must be ∈ ℝ[0,1]"
                        {:pf [p f] :pfs pfs})))

                  (when (< f 0)
                    (throw
                      (ex-info "`fisher-combination`: `f`s in `pfs` must be ∈ ℝ[0,∞]"
                        {:pf [p f] :pfs pfs})))

                  (DoubleTrip.
                    (+ p-sum      (* f (Math/log (pclamp         p))))
                    (+ pcompl-sum (* f (Math/log (pclamp (pcompl p)))))
                    (+ f-sum         f)))))

            (DoubleTrip. 0.0 0.0 0.0)
            pfs)

          p-sum      (.-x dt)
          pcompl-sum (.-y dt)
          f-sum      (.-z dt)]

      (/
        (inc
          (- (inverse-chi-square (* -2.0 p-sum)      (* 2.0 f-sum))
             (inverse-chi-square (* -2.0 pcompl-sum) (* 2.0 f-sum))))
        2.0))))

(test/deftest test-fisher-combinations
  (is (=        (fisher-combination [nil nil                  ]) 0.5))
  (is (=        (fisher-combination [[1.0 0] [1.0 0] [1.0 0]  ]) 0.5))
  (is (=        (fisher-combination [nil nil [1.0 1.0] [1.0 0]]) 0.999))
  (is (approx== (fisher-combination [[1 1]                    ]) 0.999))
  (is (approx== (fisher-combination [[1 0.3]                  ]) 0.937))
  (is (approx== (fisher-combination [[0 1]                    ]) 0.001))
  (is (approx== (fisher-combination [[1 1] [0 1]              ]) 0.5))
  (is (approx== (fisher-combination [[0 1] [1 1] [0.5 2]      ]) 0.5)))

(comment (enc/qb 1e5 (fisher-combination [[1 1] [1 0.3]]))) ; 80.24
