(ns taoensso.tukey.impl
  "Private implementation details."
  (:require
   [taoensso.encore :as enc  :refer-macros []]
   [clojure.test    :as test :refer [is]])

  #?(:cljs (:require-macros [taoensso.tukey.impl :refer []])))

(comment
  (remove-ns      'taoensso.tukey.impl)
  (test/run-tests 'taoensso.tukey.impl))

;;;; Assertions

(defmacro is-not-zero [ex-msg ex-map x]
  `(double
     (let [x# (double ~x)]
       (if (== x# 0.0)
         (throw (ex-info ~ex-msg ~ex-map))
         x#))))

(comment (is-not-zero "Boo" {} 2))

(defn is-same-count [context xs ys]
  (let [nx (count xs)
        ny (count ys)]
    (if-not (== nx ny)
      (throw
        (ex-info (str "`" (name context) "`: need same number of xs, ys")
          {:nx nx :ny ny}))
      nx)))

(comment (is-same-count :foo [:a :b] [:c :d :e]))

(defn is-p [x]
  (if (enc/pnum? x)
    x
    (throw
      (ex-info "Expected Number between 0 and 1"
        {:x x :type (type x)}))))

;;;; Sorting

#?(:clj (let [c (Class/forName "[J")] (defn longs?   [x] (instance? c x))))
#?(:clj (let [c (Class/forName "[D")] (defn doubles? [x] (instance? c x))))

(declare ^:private sorted-longs= sorted-doubles=)

(deftype SortedLongs [^longs a hash_ meta]
  #?@(:clj
      [clojure.lang.Sorted
       clojure.lang.Sequential
       clojure.lang.Counted   (count [_] (alength a))
       ;; clojure.lang.IDeref (deref [_]          a)
       clojure.lang.Indexed
       (nth [_ idx          ] (aget a idx))
       (nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))

       Object
       (hashCode [_] @hash_)
       (toString [_] (str (seq a)))

       clojure.lang.IHashEq (hasheq [_] @hash_)

       clojure.lang.IPersistentCollection
       (equiv  [_ o]                      (sorted-longs= a o))
       (equals [t o] (or (identical? t o) (sorted-longs= a o)))

       clojure.lang.IObj
       (meta     [_  ] meta)
       (withMeta [_ m] (SortedLongs. a hash_ m))

       clojure.lang.IReduceInit
       (reduce [_ f init]
         (reduce (fn [acc idx] (f acc (aget a idx))) init
           (range 0 (alength a))))]

      :cljs
      [ISorted
       ISequential
       ICounted  (-count [_] (alength a))
       ;; IDeref (-deref [_]          a)
       IIndexed
       (-nth [_ idx          ] (aget a idx))
       (-nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))

       Object (toString [_] (str (seq a)))

       IHash  (-hash  [_] @hash_)
       IEquiv (-equiv [_ other] (sorted-longs= a other))

       IMeta     (-meta      [_  ] meta)
       IWithMeta (-with-meta [_ m] (SortedLongs. a hash_ m))

       IReduce
       (-reduce [_ f init]
         (areduce a i acc init (f acc (aget a i))))]))

(deftype SortedDoubles [^doubles a hash_ meta]
  #?@(:clj
      [clojure.lang.Sorted
       clojure.lang.Sequential
       clojure.lang.Counted   (count [_] (alength a))
       ;; clojure.lang.IDeref (deref [_]          a)
       clojure.lang.Indexed
       (nth [_ idx          ] (aget a idx))
       (nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))

       Object
       (hashCode [_] @hash_)
       (toString [_] (str (seq a)))

       clojure.lang.IHashEq (hasheq [_] @hash_)

       clojure.lang.IPersistentCollection
       (equiv  [_ o]                      (sorted-doubles= a o))
       (equals [t o] (or (identical? t o) (sorted-doubles= a o)))

       clojure.lang.IObj
       (meta     [_  ] meta)
       (withMeta [_ m] (SortedDoubles. a hash_ m))

       clojure.lang.IReduceInit
       (reduce [_ f init]
         (reduce (fn [acc idx] (f acc (aget a idx))) init
           (range 0 (alength a))))]

      :cljs
      [ISorted
       ISequential
       ICounted  (-count [_] (alength a))
       ;; IDeref (-deref [_]          a)
       IIndexed
       (-nth [_ idx          ] (aget a idx))
       (-nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))

       Object (toString [_] (str (seq a)))

       IHash  (-hash  [_] @hash_)
       IEquiv (-equiv [_ other] (sorted-doubles= a other))

       IMeta     (-meta      [_  ] meta)
       IWithMeta (-with-meta [_ m] (SortedDoubles. a hash_ m))

       IReduce
       (-reduce [_ f init]
         (areduce a i acc init (f acc (aget a i))))]))

(defn sorted-longs?   [x] (do                             (instance? SortedLongs x)))
(defn sorted-doubles? [x] (do (instance? SortedDoubles x)                          ))
(defn sorted-fixnums? [x] (or (instance? SortedDoubles x) (instance? SortedLongs x)))
(defn is-sorted       [x]
  (if (sorted-fixnums? x)
    x
    (throw
      (ex-info "Expected SortedLongs or SortedDoubles"
        {:x x :type (type x)}))))

(defn- sorted-longs= [a y]
  (and
    (sorted-longs? y)
    #?(:clj  (java.util.Arrays/equals ^longs a ^longs (.-a ^SortedLongs y))
       :cljs (= (seq a) (seq (.-a ^SortedLongs y))))))

(defn- sorted-doubles= [a y]
  (and
    (sorted-doubles? y)
    #?(:clj  (java.util.Arrays/equals ^doubles a ^doubles (.-a ^SortedDoubles y))
       :cljs (= (seq a) (seq (.-a ^SortedDoubles y))))))

(defn sorted-longs
  "Advanced: returns a SortedLongs instance for given numerical values, backed by
  a sorted long array. May be provided to certain utils as a performance
  optimisation."
  ^SortedLongs [x]
  (cond
    (sorted-longs?   x) x
    (sorted-doubles? x)
    #?(:clj
       (let [^longs a (long-array (.-a ^SortedDoubles x))]
         (->SortedLongs a (delay (java.util.Arrays/hashCode a)) nil))

       :cljs
       (let [a (.-a ^SortedDoubles x)]
         (->SortedLongs
           (.-a     ^SortedDoubles x) ; No point in calling `long-array`
           (.-hash_ ^SortedDoubles x)
           nil)))

    :else
    #?(:clj
       (let [^longs a (if (longs? x) (aclone ^longs x) (long-array x))]
         (java.util.Arrays/sort a) ; O(n.log_n) on JDK 7+
         (->SortedLongs a (delay (java.util.Arrays/hashCode a)) nil))

       :cljs
       (let [a (if (array? x) (aclone x) (to-array x))]
         (goog.array/sort a)
         (->SortedLongs a (delay (hash-ordered-coll a)) nil)))))

(defn sorted-doubles
  "Advanced: returns a SortedDoubles instance for given numerical values, backed by
  a sorted double array. May be provided to certain utils as a performance
  optimisation."
  ^SortedDoubles [x]
  (cond
    (sorted-doubles? x) x
    (sorted-longs?   x)
    #?(:clj
       (let [^doubles a (double-array (.-a ^SortedLongs x))]
         (->SortedDoubles a (delay (java.util.Arrays/hashCode a)) nil))

       :cljs
       (let [a (.-a ^SortedLongs x)]
         (->SortedDoubles
           (.-a     ^SortedLongs x) ; No point in calling `double-array`
           (.-hash_ ^SortedLongs x)
           nil)))

    :else
    #?(:clj
       (let [^doubles a (if (doubles? x) (aclone ^doubles x) (double-array x))]
         (java.util.Arrays/sort a) ; O(n.log_n) on JDK 7+
         (->SortedDoubles a (delay (java.util.Arrays/hashCode a)) nil))

       :cljs
       (let [a (if (array? x) (aclone x) (to-array x))]
         (goog.array/sort a)
         (->SortedDoubles a (delay (hash-ordered-coll a)) nil)))))

(test/deftest test-sorted-fixnums
  (test/testing "Sorted fixnums"

    (test/testing "Sorted longs"
      (let [s (sorted-longs [1 2 3 4])]
        (is (counted?    s))
        (is (indexed?    s))
        (is (sequential? s))
        (is (sorted?     s))
        (is (= (nth s 2) 3))
        (is (= (reduce + 0 s) 10))
        (is (=    s (sorted-longs [1 2 3 4])))
        (is (=    s (sorted-longs (sorted-doubles [1 2 3 4]))))
        (is (not= s               (sorted-doubles [1 2 3 4])))

        (is (:meta? (meta (with-meta s {:meta? true}))))
        (is (= (count
                 (conj #{}
                   (sorted-longs   [1 2 3 4])
                   (sorted-longs   [1 2 3 4])
                   (sorted-longs   [4 3 2 1])
                   (sorted-longs   [5 6 7 8])
                   (sorted-doubles [1 2 3 4])))
              3))))

    (test/testing "Sorted doubles"
      (let [s (sorted-doubles [1 2 3 4])]
        (is (counted?    s))
        (is (indexed?    s))
        (is (sequential? s))
        (is (sorted?     s))
        (is (= (nth s 2) 3.0))
        (is (= (reduce + 0.0 s) 10.0))
        (is (=    s (sorted-doubles [1 2 3 4])))
        (is (=    s (sorted-doubles (sorted-longs [1 2 3 4]))))
        (is (not= s                 (sorted-longs [1 2 3 4])))

        (is (:meta? (meta (with-meta s {:meta? true}))))
        (is (= (count
                 (conj #{}
                   (sorted-doubles [1 2 3 4])
                   (sorted-doubles [1 2 3 4])
                   (sorted-doubles [4 3 2 1])
                   (sorted-doubles [5 6 7 8])
                   (sorted-longs   [1 2 3 4])))
              3))))))

(defn sorted-min "Returns ?el" [xs] (let [max-idx (dec (count (is-sorted xs)))] (when (>= max-idx 0) (nth xs 0))))
(defn sorted-max "Returns ?el" [xs] (let [max-idx (dec (count (is-sorted xs)))] (when (>= max-idx 0) (nth xs max-idx))))

(defn sorted-percentile-element
  "Like `sorted-percentile`, but return an actual ?element that's as close as
  possible to true median."
  [p xs]
  (let [max-idx (dec (count (is-sorted xs)))]
    (when (>= max-idx 0)
      (let [idx (* max-idx (double (is-p p)))]
        (nth xs (Math/round idx))))))

(defn double-nth "Returns double" [xs ^double idx]
  (let [idx-floor (Math/floor idx)
        idx-ceil  (Math/ceil  idx)]

    (if (== idx-ceil idx-floor)
      (double (nth xs (int idx)))

      ;; Generalization of (floor+ceil)/2
      (let [weight-floor (- idx-ceil idx)
            weight-ceil  (- 1 weight-floor)]
        (+
          (* weight-floor (double (nth xs (int idx-floor))))
          (* weight-ceil  (double (nth xs (int idx-ceil)))))))))

(test/deftest test-double-nth
  (is (= (double-nth [1  3] 0.5)  2.0))
  (is (= (double-nth [1 10] 0.5)  5.5))
  (is (= (double-nth [1 10] 0.75) 7.75)))

(defn sorted-percentile "Returns ?double" [p xs]
  (let [max-idx (dec (count (is-sorted xs)))]
    (when (>= max-idx 0)
      (let [idx (* max-idx (double (is-p p)))]
        (double-nth xs idx)))))

(test/deftest test-percentiles
  (is (nil? (sorted-percentile         0.5  (sorted-longs   []))))
  (is (=    (sorted-percentile         0.5  (sorted-doubles [1 2 3.1 4 5])) 3.1))
  (is (=    (sorted-percentile         0.5  (sorted-doubles [1 2     4 5])) 3.0))
  (is (=    (sorted-percentile         0.77 (sorted-longs   [1 5 2 4 3]))   4.08))

  (is (=    (sorted-percentile-element 0.5  (sorted-doubles [1 2 3.1 4 5])) 3.1))
  (is (=    (sorted-percentile-element 0.5  (sorted-doubles [1 2     4 5])) 4.0))
  (is (=    (sorted-percentile-element 0.77 (sorted-longs   [1 5 2 4 3]))     4)))

;;;; Math

(defn pow'
  "Auto-promoting `Math/pow`."
  [base exp]
  #?(:cljs (Math/pow base exp)
     :clj
     (let [d (.pow (bigdec base) exp)]
       (try
         (.longValueExact             d)
         (catch ArithmeticException _ d)))))

(comment (enc/qb 1e6 (Math/pow 3 9) (pow' 3 9))) ; [143.27 113.4]

;;;; Misc

(do ; For tests, etc.
  (defn rand-longs   [n] (into [] (repeatedly n #(- ^long   (rand-int 2000) 1000))))
  (defn rand-doubles [n] (into [] (repeatedly n #(- ^double (rand     2000) 1000.0)))))

(comment
  (rand-longs   4)
  (rand-doubles 3))

(do ; For micro-optimized reductions, etc.
  (deftype Pair       [        x         y          ])
  (deftype DoublePair [^double x ^double y          ])
  (deftype Trip       [        x         y         z])
  (deftype DoubleTrip [^double x ^double y ^double z]))

(defn min-max
  "Returns ?[<xmin> <xmax>] in:
    - O(1) for Sorted types (SortedLongs, SortedDoubles),
    - O(n) otherwise."
  [xs]
  (if (sorted-fixnums? xs)
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
                    (> in vmax) (DoublePair. vmin in)
                    (< in vmin) (DoublePair. in vmax)
                    :else acc)))
              (DoublePair. v1 v1) xs)]
        [(.-x min-max) (.-y min-max)]))))

(test/deftest test-min-max
  (is (= (min-max [ ]) nil))
  (is (= (min-max [4]) [4.0 4.0]))

  (let [xs   (rand-doubles 2000)
        xmin (double (apply min xs))
        xmax (double (apply max xs))]

    (is (= (min-max xs) [xmin xmax]))))

(comment (enc/qb 1e4 (min-max (range 1000))))

(defn multi-reduce
  "Like `reduce` but supports separate simultaneous accumulators
  as a micro-optimisation when reducing a large collection multiple
  times."
  ([f  init           coll] (reduce f init coll))
  ([f1 init1 f2 init2 coll]
   (let [^Pair tuple
         (reduce
           (fn [^Pair tuple in]
             (Pair.
               (f1 (.-x tuple) in)
               (f2 (.-y tuple) in)))
           (Pair. init1 init2)
           coll)]

     [(.-x tuple) (.-y tuple)]))

  ([f1 init1 f2 init2 f3 init3 coll]
   (let [^Trip tuple
         (reduce
           (fn [^Trip tuple in]
             (Trip.
               (f1 (.-x tuple) in)
               (f2 (.-y tuple) in)
               (f2 (.-z tuple) in)))
           (Trip. init1 init2 init3)
           coll)]

     [(.-x tuple) (.-y tuple) (.-z tuple)])))

(test/deftest test-multi-reduce
  (is (= (multi-reduce + 0 - 0 (range 1e4)) [49995000 -49995000])))

(defn indexed
  "[:a :b :c ...] -> sorted {0 :a, 1 :b, 2 :c ...},
  useful for distinguishing between duplicate elements in `xs`."
  [xs]
  ;; Unfortunately sorted-map doesn't currently support transients
  (enc/reduce-indexed (fn [acc i x] (assoc acc i x))
    (sorted-map) xs))

(comment (indexed [:a :b :c :d :d]))

(defn joint-vals
  "Returns [[x1 y1] ... [xn yn]] for given joint value colls."
  [xs ys]
  (let [xs (vec xs)
        ys (vec ys)
        n  (is-same-count :joint-pairs xs ys)]

    (persistent!
      (reduce (fn [acc idx] (conj! acc [(get xs idx) (get ys idx)]))
        (transient [])
        (range 0 n)))))

(comment (joint-vals [1 2 3 4] [10 20 30 40]))

;;;; Combinatorics

(defn reduce-combinations ; Main combinatorial fn
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
   (let [mxs   (indexed xs) ; {1 x1, 2 x2, ...}
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

(def ^:private rcs "reduce-combinations" reduce-combinations)
(test/deftest test-reduce-combinations
  (is (= (rcs nil conj [] {} 1 [:a :b :c]) [[:a] [:b] [:c]]))
  (is (= (rcs nil conj [] {} 2 [:a :b :c]) [[:a :b] [:a :c] [:b :c]]))
  (is (= (rcs nil conj [] {} 3 [:a :b :c]) [[:a :b :c]]))
  (is (= (count (rcs nil conj [] {} 4 [:a :b :c])) 0))

  ;; With duplicates
  (is (= (rcs nil conj [] {} 1 [:a :a :b :c]) [[:a] [:a] [:b] [:c]]))
  (is (= (rcs nil conj [] {} 2 [:a :a :b :c]) [[:a :a] [:a :b] [:a :c] [:a :b] [:a :c] [:b :c]]))
  (is (= (rcs nil conj [] {} 3 [:a :a :b :c]) [[:a :a :b] [:a :a :c] [:a :b :c] [:a :b :c]]))

  (is (= (rcs nil conj [] {:repeated? true} 1 [:a :b :c]) [[:a] [:b] [:c]]))
  (is (= (rcs nil conj [] {:repeated? true} 2 [:a :b :c]) [[:a :a] [:a :b] [:a :c] [:b :b] [:b :c] [:c :c]]))
  (is (= (rcs nil conj [] {:repeated? true} 3 [:a :b :c]) [[:a :a :a] [:a :a :b] [:a :a :c] [:a :b :b] [:a :b :c] [:a :c :c] [:b :b :b] [:b :b :c] [:b :c :c] [:c :c :c]]))
  (is (= (count (rcs nil conj [] {:repeated? true} 4 [:a :b :c])) 15))

  (is (= (rcs nil conj [] {:ordered? true} 1 [:a :b :c]) [[:a] [:b] [:c]]))
  (is (= (rcs nil conj [] {:ordered? true} 2 [:a :b :c]) [[:a :b] [:a :c] [:b :a] [:b :c] [:c :a] [:c :b]]))
  (is (= (rcs nil conj [] {:ordered? true} 3 [:a :b :c]) [[:a :b :c] [:a :c :b] [:b :a :c] [:b :c :a] [:c :a :b] [:c :b :a]]))
  (is (= (count (rcs nil conj [] {:ordered? true} 4 [:a :b :c])) 0))

  (is (= (rcs nil conj [] {:repeated? true :ordered? true} 1 [:a :b :c]) [[:a] [:b] [:c]]))
  (is (= (rcs nil conj [] {:repeated? true :ordered? true} 2 [:a :b :c]) [[:a :a] [:a :b] [:a :c] [:b :a] [:b :b] [:b :c] [:c :a] [:c :b] [:c :c]]))
  (is (= (rcs nil conj [] {:repeated? true :ordered? true} 3 [:a :b :c])
        [[:a :a :a] [:a :a :b] [:a :a :c] [:a :b :a] [:a :b :b] [:a :b :c]
         [:a :c :a] [:a :c :b] [:a :c :c] [:b :a :a] [:b :a :b] [:b :a :c]
         [:b :b :a] [:b :b :b] [:b :b :c] [:b :c :a] [:b :c :b] [:b :c :c]
         [:c :a :a] [:c :a :b] [:c :a :c] [:c :b :a] [:c :b :b] [:c :b :c]
         [:c :c :a] [:c :c :b] [:c :c :c]]))

  (is (= (count (rcs nil conj [] {:repeated? true :ordered? true} 4 [:a :b :c])) 81))

  (is (= (rcs (take 5) conj [] {:repeated? true} 3 [:a :b :c]) [[:a :a :a] [:a :a :b] [:a :a :c] [:a :b :b] [:a :b :c]]))

  (is (=        (rcs (take 5) conj [] {} 3 [:a :b :c :d :e :f :g :h :i :j :k]) [[:a :b :c] [:a :b :d] [:a :b :e] [:a :b :f] [:a :b :g]]))
  (is (= (count (rcs nil      conj [] {} 3 [:a :b :c :d :e :f :g :h :i :j :k])) 165)))

;;;; Kendall-Knight

(defn vsplit
  "Splits vector ~evenly into [<left> <right>] sub-vectors in O(1)."
  [v]
  (let [n (count v)
        i (bit-shift-right n 1)]
    [(subvec v 0 i) (subvec v i n)]))

(comment (vsplit [1 2 3]))

(defn counting-merge-sort
  "Modified merge sort implementation that tracks number of values
  that needed to be swapped, and returns [<n-swaps> <sorted-xs>] in O(n.log_n)."
  ;; TODO Use mutable arrays for extra speed?
  ([keyfn xs]
   (let [n-swaps_ (enc/counter)
         result   (counting-merge-sort keyfn n-swaps_ (vec xs))]
     [@n-swaps_ result]))

  ([keyfn n-swaps_ v] ; Implementation detail
   (if (<= (count v) 1)
     v ; End of recursive division
     (let [[l r] (vsplit v)

           ;; Recursive division phase
           init-l (counting-merge-sort keyfn n-swaps_ l)
           init-r (counting-merge-sort keyfn n-swaps_ r)]

       ;; Sorting merge phase
       (let [[out into* conj* done] ; Micro-optimisation
             (if-let [use-transients? (>= (+ (count init-l) (count init-r)) 11)]
               [(transient []) enc/into! conj! persistent!]
               [(do        [])     into  conj  identity])]

         (done
           (loop [l init-l r init-r out out]
             (enc/cond
               (nil? l) (into* out r)
               (nil? r) (into* out l)
               :let [[l1] l, [r1] r]

               (<= (double (keyfn l1)) (double (keyfn r1)))
               (recur (next l) r (conj* out l1))

               :else
               (do
                 (n-swaps_ (count l))
                 (recur l (next r) (conj* out r1)))))))))))

(test/deftest test-counting-merge-sort
  (is (= (counting-merge-sort identity [1 2 3 4 5]) [0 [1 2 3 4 5]])) ; 0 swaps
  (is (= (counting-merge-sort identity [1 2 4 3 5]) [1 [1 2 3 4 5]])) ; 1 swaps
  (is (= (counting-merge-sort identity [1 4 2 3 5]) [2 [1 2 3 4 5]])) ; 2 swaps
  (is (= (counting-merge-sort identity [4 1 2 3 5]) [3 [1 2 3 4 5]])) ; 3 swaps

  (let [xs (rand-doubles 2000)]
    (is (= (second (counting-merge-sort identity xs)) (enc/sortv xs)))))

(comment
  (require '[taoensso.lib.core :as tl-core])
  (enc/qb 1e4 ; [8.42 142.04] ~18x
    (enc/sortv                    [1 1 3 4 5 3 4 5 6 7 8 9 10])
    (counting-merge-sort identity [1 1 3 4 5 3 4 5 6 7 8 9 10])))

(defn kendall-knight-tied-pair-count
  "Given sorted `xs`, returns the total number of rank ties in all possible
  [xi xj] combinations, as per Knight's Tau method [R1], [R2]."
  ^double [keyfn sorted-xs]
  (get
    (reduce
      ;; Iterate through xs, keeping track of chain length (= x_n x_n-1)
      (fn [[last-x chain-len acc] x]
        (let [x (keyfn x)]
          (if-let [tie? (= x last-x)] ; = to also allow xy pairs
            [x (inc (long chain-len)) acc]
            ;; Each time a chain ends, add n(n-1)/2 to acc | n= chain size
            [x 1 (+ (double acc) (let [n (double chain-len)] (/ (* n (dec n)) 2.0)))])))
      [nil 0.0 0.0]
      (conj sorted-xs :end))
    2))

(comment (enc/qb 1e4 (kendall-knight-tied-pair-count identity [2 2 3 4 5 5 5 6 6 7 7 7])))

(defn kendall-correlation-impl-naive
  "Returns {:keys [n_c-n_d n_xy n_yt n_xyt n_p} in O(n²)
  using `reduce-combinations`."
  [reduce-combinations xys]
  (let [n-xys   (count xys)
        n-pairs (/ (* n-xys (dec n-xys)) 2.0) ; n(n-1) / 2 = (n-combinations 2 n)
        n-concordant_ (enc/counter)
        n-discordant_ (enc/counter)
        n-x-ties_     (enc/counter)
        n-y-ties_     (enc/counter)
        n-xy-ties_    (enc/counter)
        sign          #?(:clj #(Math/signum ^double %) :cljs #(double (Math/sign %)))]

    (reduce-combinations
      (fn [_ [[x1 y1] [x2 y2]]]
        (let [^double sigx (sign (- (double x2) (double x1)))
              ^double sigy (sign (- (double y2) (double y1)))
              sigxy (* sigx sigy)]

          (enc/cond
            (> sigxy 0) (n-concordant_)
            (< sigxy 0) (n-discordant_)
            :else
            (let [x? (zero? sigx)
                  y? (zero? sigy)]
              (when      x?     (n-x-ties_))
              (when         y?  (n-y-ties_))
              (when (and x? y?) (n-xy-ties_))))
          nil))
      nil
      2 xys)

    {:n_xt    @n-x-ties_
     :n_yt    @n-y-ties_
     :n_xyt   @n-xy-ties_
     :n_p     n-pairs
     :n_c-n_d (- (long @n-concordant_) (long @n-discordant_))}))

(defn kendall-correlation-impl-knight
  "Returns {:keys [n_c-n_d n_xy n_yt n_xyt n_p} in O(n.log_n)
  using Knight's Tau method [R1], [R2]."
  [xys]
  (let [sorted-xys (enc/sortv xys)
        n-xys      (count sorted-xys)
        n-pairs    (/ (* n-xys (dec n-xys)) 2.0) ; n(n-1) / 2 = (n-combinations 2 n)

        n-xy-ties (kendall-knight-tied-pair-count identity   sorted-xys)
        n-x-ties  (kendall-knight-tied-pair-count #(get % 0) sorted-xys)
        [n-swaps sorted-ys] (counting-merge-sort  #(get % 1) sorted-xys)
        n-y-ties  (kendall-knight-tied-pair-count #(get % 1) sorted-ys)]

    {:n_xt  n-x-ties
     :n_yt  n-y-ties
     :n_xyt n-xy-ties
     :n_p   n-pairs
     :n_c-n_d
     (-
       (+ (- n-pairs (+ n-x-ties n-y-ties)) n-xy-ties)
       (* 2.0 (double n-swaps)))}))

(test/deftest test-kendall-correlation-impls
  (let [xys (joint-vals
              [1 1 4 6 2 3 4 5 5 2 4 8 6 3 5]
              [1 3 5 3 4 3 7 5 5 9 1 3 8 6 3])]
    (is
      (=
        [(kendall-correlation-impl-naive  rcs xys)
         (kendall-correlation-impl-knight     xys)]
        [{:n_xt 10,   :n_yt 14,   :n_xyt 1,   :n_p 105.0, :n_c-n_d 6}
         {:n_xt 10.0, :n_yt 14.0, :n_xyt 1.0, :n_p 105.0, :n_c-n_d 6.0}]))

    #_
    (enc/qb 1e4 ; [641.16 432.14]
      (kendall-correlation-impl-naive  rcs xys)
      (kendall-correlation-impl-knight     xys))))
