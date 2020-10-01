(ns taoensso.tukey
  "Simple stats toolkit for Clojure/Script.

  Objectives:
    - All Clojure/Script implementations.
    - Pleasant+consistant API w/ beginner-friendly documentation.
    - Start with basic, commonly used utils. Can expand over time.
    - Start with short, understandable algorithms. Can optimise over time.

  Refererences:
    [R1] A Computer Method for Calculating Kendall's Tau with Ungrouped Data
         by William R. Knight, Journal of the American Statistical Association
         Vol. 61, No. 314, Part 1 (Jun., 1966), pp. 436-439 (4 pages).

    [R2] Efficiently Computing Kendall's Tau by Matt Adereth,
         http://adereth.github.io/blog/2013/10/30/efficiently-computing-kendalls-tau/"

  {:author "Peter Taoussanis (@ptaoussanis)"}
  (:require
   [taoensso.encore     :as enc  :refer-macros []]
   [taoensso.tukey.impl :as impl :refer-macros []])
  #?(:cljs (:require-macros [taoensso.tukey :refer []]))
  #?(:clj  (:import [taoensso.tukey.impl SortedLongs SortedDoubles DoublePair])))

;;;; TODO
;; ∈ ∉ ⇒⇔ → × ⊃⊂ ⊇⊆ ≡ ¬ ∀ ∃ ∝
;; - Setup initial reference tests
;; - Generative: `n-combinations` vs `combinations` count
;; - Generative: Kendall 2x impl. equality
;; - Import from: Tufte, WS, Qvr, tl-core, classification, Nippy
;; - Use from Tufte
;; - T test (1+2 sided?)
;; - Z test (1+2 sided?)
;; - Incanter, other libs?
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

(enc/assert-min-encore-version [3 6 0])

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
    [n]
    ;; (println)
    (let [n (long n)]
      (if-let [cached (get @cache_ n)]
        cached
        (let [_ (when (< n 0) (throw (ex-info "`factorial`: need n>0" {:n n})))

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
              (loop [idx best-starting-idx
                     acc best-starting-acc
                     new-cache (transient {})]

                (let [new-acc   #?(:clj (*' acc idx) :cljs (* acc idx))
                      new-cache (assoc! new-cache idx new-acc)]
                  (if (== idx n)
                    [new-acc new-cache]
                    (recur (inc idx) new-acc new-cache))))]

          (swap! cache_ conj (persistent! -new-cache))
          result)))))

(comment
  (count @(:cache_ (meta #'factorial)))
  (reset! (:cache_ (meta #'factorial)) {})
  (factorial 8)
  (factorial 23))

(comment (factorial 21))

;; Main combinatorial fn
(defn reduce-combinations
  "Like `combinations` but instead of producing and returning a vector of
  combinations, acts like `reduce` (or `transduce` when `xform` provided).

  Since it avoids the construction of a data structure, this can be a lot more
  efficient if the number of combinations is large, and you don't actually
  need the data structure.

  The `xform` also offers a lot of flexibility for filtering, early
  termination, etc."
  ([      rfn init                              n-take xs] (reduce-combinations nil   rfn init nil n-take xs))
  ([xform rfn init                              n-take xs] (reduce-combinations xform rfn init nil n-take xs))
  ([xform rfn init {:keys [repeated? ordered?]} n-take xs]
   (let [mxs   (impl/indexed xs) ; {1 x1, 2 x2, ...}, to allow duplicates
         nmax  (dec (long (or n-take (count mxs))))
         mxs>= (when-not ordered?
                 (enc/fmemoize (fn [idx] (reduce dissoc mxs (range 0 idx)))))

         rfn (if xform (xform rfn) rfn)
         f
         (fn self [acc base ^long n mxs]
           ;; (println {:base base :mxs mxs})
           (reduce-kv
             (fn [acc idx x]
               (let [mxs (if ordered?  mxs (mxs>=      idx))
                     mxs (if repeated? mxs (dissoc mxs idx))]
                 ;; (println {:base base :idx idx :mxs (keys mxs)})
                 (if (== n nmax)
                   (rfn  acc (conj base x))
                   (self acc (conj base x) (inc n) mxs))))
             acc mxs))]
     ;; (println)
     (f init [] 0 mxs))))

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

(defn permutations
  "Like `combinations`, but always ordered."
  ([                 xs] (combinations {:ordered? true :xs xs :n-take (count xs)}))
  ([          n-take xs] (combinations {:ordered? true :xs xs :n-take n-take}))
  ([repeated? n-take xs] (combinations {:ordered? true :xs xs :n-take n-take
                                        :repeated? repeated?})))

(comment
  (count (combinations {:n-take 3 :xs [:a :b :c :d]}))                   ; 4
  (count (combinations {:n-take 3 :xs [:a :b :c :d] :repeated? true}))   ; 20
  (count (combinations {:n-take 5 :xs [:a :b :c :d :e :f :g :h :i :j]})) ; 252

  (count (permutations             [:a :b :c :d]))                   ; 24
  (count (permutations :repeated 3 [:a :b :c :d]))                   ; 64
  (count (permutations nil       5 [:a :b :c :d :e :f :g :h :i :j])) ; 30240

  (reduce-combinations (take 5) conj [] 3 [:a :b :c :d :e :f :g :h :i :j :k])

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

(defn n-permutations
  "Like `permutations`, but only returns the cardinality of the result.
  May auto-promote to BigDecimal for Clj."
  ([                 n-xs] (n-combinations {:ordered? true :n-xs n-xs :n-take (count n-xs)}))
  ([          n-take n-xs] (n-combinations {:ordered? true :n-xs n-xs :n-take n-take}))
  ([repeated? n-take n-xs] (n-combinations {:ordered? true :n-xs n-xs :n-take n-take
                                            :repeated? repeated?})))

(comment ; TODO Reference tests
  (n-combinations {:n-take 5 :n-xs 10 :repeated? true  :ordered? true})
  (n-combinations {:n-take 5 :n-xs 10 :repeated? true  :ordered? false})
  (n-combinations {:n-take 5 :n-xs 10 :repeated? false :ordered? true})
  (n-combinations {:n-take 5 :n-xs 10 :repeated? false :ordered? false}))

;;;; Summary statistics

(defn- -mean-sum "Returns [<n> <sum>]"
  [op init xs]
  (if (counted? xs)
    [(count xs) (double (reduce (fn [^double acc x] (op acc (double x))) init xs))]

    (let [n_ (enc/counter)]
      [@n_ (double (reduce (fn [^double acc x] (n_) (op acc (double x))) init xs))])))

(comment (-mean-sum + 0.0 [2 3 4 5]))

(defn mean
  "Returns the arithmetic mean x̄ ∈ ℝ of the given values (n>0)."
  ;; TODO Compare to others
  ^double [xs]
  (let [[n sum] (-mean-sum + 0.0 xs)]
    (/
      (double sum)
      (impl/is-not-zero "`mean`: need >0 values" {:xs xs}
        (count xs)))))

(comment (mean []) (mean [2 3 4 2]))

(def arithmetic-mean "Alias of `mean`" mean)

(defn geometric-mean
  "TODO Docstring, compare to others"
  ^double [xs]
  (let [[n sum] (-mean-sum * 1.0 xs)]
    (Math/pow sum
      (/ 1.0
        (impl/is-not-zero "`geometric-mean`: need >0 values" {:xs xs}
          n)))))

(comment (geometric-mean [1 1 1 2]))

(defn median
  "TODO Docstring, compare to others"
  ^double [xs]
  (impl/is-not-zero "`median`: need >0 values" {:xs xs} (count xs))
  (let [xs (impl/sorted-doubles xs)]
    (impl/sorted-median xs)))

(comment (median [2]) (median [1 5 2 4 3]))

(defn variance
  "Returns the variance σ² ∈ ℝ of the given values (n>1)."
  ^double [xs]
  (let [xbar (mean xs)
        sum
        (double
          (reduce
            (fn [acc x] (+ (double acc) (Math/pow (- (double x) xbar) 2.0)))
            0.0 xs))]
    (/ sum
      (impl/is-not-zero "`variance`: need >1 values" {:xs xs}
        (- (double (count xs)) 1.0)))))

(comment (variance [1]) (variance [1 2 4]))

(defn std-deviation
  "Returns the standard deviation σ ∈ ℝ of the given values (n>1)."
  ^double [xs] (Math/sqrt (variance xs)))

(comment (std-deviation [1 2 4]))

(def arithmetic-std-deviation "Alias of `std-deviation`." std-deviation)

(defn mean-abs-deviation
  "Returns the mean absolute deviation MAD ∈ ℝ of the given values (n>0)
  from a central point (:mean, :median, or arbitrary val)."
  (^double [              xs] (mean-abs-deviation :mean xs))
  (^double [central-point xs]
   (let [central-point
         (double
          (case central-point
            (:mean :arithmetic-mean) (mean   xs)
            :median                  (median xs)
            central-point))

         mad-sum
         (reduce
           (fn [^double acc x]
             (+ acc (Math/abs (- (double x) central-point))))
           0.0 xs)]

     (/
       (double mad-sum)
       (impl/is-not-zero "`mean-abs-deviation`: need >0 values"
         {:xs xs :central-point central-point}
         (count xs))))))

(comment
  (mean-abs-deviation :mean [1 1 1 1 1 1 2])
  (mean-abs-deviation (impl/rand-ints 10)))

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
      (impl/is-not-zero "`covariance`: need >1 values" {:xs xs :ys ys}
        (- (double n) 1.0)))))

(comment (covariance [1 2 3 4] [2 1 3 4]) (covariance [] []))

(defn percentiles
  "Returns ?[min p25 p50 p75 p90 p95 p99 max] in:
    - O(1) for Sorted types (SortedLongs, SortedDoubles),
    - O(n) otherwise."
  [xs]
  (let [max-idx (dec (count xs))]
    (when (>= max-idx 0)
      (let [xs (if (impl/sorted-fixnums? xs) xs (impl/sorted-doubles xs))]
        [(nth xs 0)
         (nth xs (Math/round (* max-idx 0.25)))
         (nth xs (Math/round (* max-idx 0.50)))
         (nth xs (Math/round (* max-idx 0.75)))
         (nth xs (Math/round (* max-idx 0.90)))
         (nth xs (Math/round (* max-idx 0.95)))
         (nth xs (Math/round (* max-idx 0.99)))
         (nth xs                max-idx)]))))

(comment
  (percentiles (impl/sorted-longs [1 5 2 4 3]))
  (percentiles                    [1 5 2 4 3]))

(defn min-max
  "Returns ?[<min> <max>] in:
    - O(1) for Sorted types (SortedLongs, SortedDoubles),
    - O(n) otherwise."
  [xs]
  (if (impl/sorted-fixnums? xs)
    (let [max-idx (dec (count xs))]
      (when (>= max-idx 0)
        [(nth xs 0) (nth xs max-idx)]))

    (if (zero? (count xs))
      nil
      (let [[v1] xs
            ^DoublePair min-max
            (reduce
              (fn [^DoublePair acc in]
                (let [in   (double in)
                      vmin (.-x acc)
                      vmax (.-y acc)]
                  (enc/cond
                    (> in vmax) (impl/->DoublePair vmin in)
                    (< in vmin) (impl/->DoublePair in vmin)
                    :else acc)))
              (impl/->DoublePair v1 v1) xs)]
        [(.-x min-max) (.-y min-max)]))))

(comment
  (enc/qb 1e6 (min-max [10 9 -3 12])) ; 330.13
  (min-max [4]))

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

(comment
  (enc/approx= ; TODO reference -> tests
    0.041 ; https://www.socscistatistics.com/tests/pearson/
    (pearson-correlation
      [22 -46 -63 -12 -12 36 72 -83 21 93 31 23 94 22 6]
      [56 -30 74 54 -5 30 -66 -48 32 -49 71 71 71 64 -35])))

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

(comment (mean-ranks [2 33 6 55 -2]))

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

(comment
  (enc/approx= ; TODO reference -> tests
    -0.02511 ; https://www.socscistatistics.com/tests/spearman/
    (spearman-correlation
      [72 11 11 -28 80 -94 -26 76 56 -87 -63 -61 30 67 -30]
      [-75 51 -36 20 -99 -22 62 52 52 52 -93 -57 -47 -12 -89])))

(defn joint-vals
  "Returns [[x1 y1] ... [xn yn]] for given joint value colls."
  [xs ys]
  (let [xs (vec xs)
        ys (vec ys)
        n  (impl/is-same-count :joint-pairs xs ys)]

    (persistent!
      (reduce (fn [acc idx] (conj! acc [(get xs idx) (get ys idx)]))
        (transient [])
        (range 0 n)))))

(comment (joint-vals [1 2 3 4] [10 20 30 40]))

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

(comment
  (enc/qb 1e4
    (kendall-correlation {:impl :auto}   [1 2 3 3 4 5] [3 2 5 6 6 7])
    (kendall-correlation {:impl :knight} [1 2 3 3 4 5] [3 2 5 6 6 7])
    (kendall-correlation {:impl :naive}  [1 2 3 3 4 5] [3 2 5 6 6 7])
    ) ; [96.13 157.79 89.0]

  (kendall-correlation {:kind :tao-a} [1 2 3 3 4 5] [3 2 5 6 6 7]) 0.7333333333333333
  (kendall-correlation {:kind :tao-b} [1 2 3 3 4 5] [3 2 5 6 6 7]) 0.7857142857142857
  (kendall-correlation {:kind :tao-c} [1 2 3 3 4 5] [3 2 5 6 6 7]) 0.7638888888888888

  (enc/approx= ; TODO reference -> tests
    0.0386477932333946 ; https://www.wessa.net/rwasp_kendall.wasp
    (kendall-correlation
      [-46 -60 -61 69 51 51 58 98 -17 65 41 73 33 -20 -81]
      [-73 -86 93 -55 -46 19 -54 77 -80 -80 29 29 -26 -36 40]))

  (let [xs (impl/rand-ints 400)
        ys (impl/rand-ints 400)]
    (enc/qb 1e1
      (kendall-correlation {:impl :naive}  xs ys)
      (kendall-correlation {:impl :knight} xs ys)
      (spearman-correlation                xs ys)
      (pearson-correlation                 xs ys)))
  ;; [528.65 19.7 4.78 0.58]
  )
