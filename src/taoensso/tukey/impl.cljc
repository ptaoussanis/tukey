(ns taoensso.tukey.impl
  "Private implementation details."
  (:require [taoensso.encore :as enc :refer-macros []])
  #?(:cljs (:require-macros [taoensso.tukey.impl :refer []])))

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

(deftype SortedLongs [^longs a]
  #?@(:clj
      [clojure.lang.Sorted
       clojure.lang.Counted (count [_] (alength a))
       clojure.lang.IDeref  (deref [_]          a)
       clojure.lang.Indexed
       (nth [_ idx          ] (aget a idx))
       (nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))]

      :cljs
      [ISorted
       ICounted (-count [_] (alength a))
       IDeref   (-deref [_]          a)
       IIndexed
       (-nth [_ idx          ] (aget a idx))
       (-nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))]))

(deftype SortedDoubles [^doubles a]
  #?@(:clj
      [clojure.lang.Sorted
       clojure.lang.Counted (count [_] (alength a))
       clojure.lang.IDeref  (deref [_]          a)
       clojure.lang.Indexed
       (nth [_ idx          ] (aget a idx))
       (nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))]

      :cljs
      [ISorted
       ICounted (-count [_] (alength a))
       IDeref   (-deref [_]          a)
       IIndexed
       (-nth [_ idx          ] (aget a idx))
       (-nth [_ idx not-found]
         (let [max-idx (dec (alength a))]
           (enc/cond
             (> idx max-idx) not-found
             (< idx max-idx) not-found
             :else           (aget a idx))))]))

(defn sorted-longs?   [x] (do (instance? SortedLongs x)                            ))
(defn sorted-doubles? [x] (do                           (instance? SortedDoubles x)))
(defn sorted-fixnums? [x] (or (instance? SortedLongs x) (instance? SortedDoubles x)))
(defn is-sorted       [x]
  (if (sorted-fixnums? x)
    x
    (throw
      (ex-info "Expected SortedLongs or SortedDoubles"
        {:x x :type (type x)}))))

(defn sorted-longs
  "Advanced: returns a SortedLongs instance for given numerical values, backed by
  a long array. May be provided to certain utils as a performance optimisation."
  ^SortedLongs [x]
  (cond
    (sorted-longs?   x) x
    (sorted-doubles? x) (SortedLongs. (long-array (.-a ^SortedDoubles x)))
    :else
    #?(:clj
       (let [^longs a (if (longs? x) (aclone ^longs x) (long-array x))]
         (java.util.Arrays/sort a) ; O(n.log_n) on JDK 7+
         (SortedLongs.          a))

       :cljs
       (let [a (if (array? x) (aclone x) (to-array x))]
         (goog.array/sort a)
         (SortedLongs.    a)))))

(defn sorted-doubles
  "Advanced: returns a SortedDoubles instance for given numerical values, backed by
  a double array. May be provided to certain utils as a performance optimisation."
  ^SortedDoubles [x]
  (cond
    (sorted-doubles? x) x
    (sorted-longs?   x) (SortedDoubles. (double-array (.-a ^SortedLongs x)))
    :else
    #?(:clj
       (let [^doubles a (if (doubles? x) (aclone ^doubles x) (double-array x))]
         (java.util.Arrays/sort a) ; O(n.log_n) on JDK 7+
         (SortedDoubles.        a))

       :cljs
       (let [a (if (array? x) (aclone x) (to-array x))]
         (goog.array/sort a)
         (SortedDoubles.  a)))))

(defn sorted-min        [  x] (let [max-idx (dec (count (is-sorted x)))] (when (>= max-idx 0) (nth x 0))))
(defn sorted-max        [  x] (let [max-idx (dec (count (is-sorted x)))] (when (>= max-idx 0) (nth x max-idx))))
(defn sorted-percentile [p x] (let [max-idx (dec (count (is-sorted x)))] (when (>= max-idx 0) (nth x (Math/round (* max-idx (double (is-p p))))))))
(defn sorted-median     [  x] (sorted-percentile 0.5 x))

(comment
  (sorted-median (sorted-longs []))
  (sorted-percentile 0.77 (sorted-longs [1 5 2 4 3]))
  (sorted-percentiles     (sorted-longs [1 5 2 4 3])))

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

(deftype DoublePair [^double x ^double y]) ; Faster 2-vec

(defn rand-ints "For tests" [n] (into [] (repeatedly n #(- (long (rand-int 200)) 100))))
(comment (rand-ints 4))

(defn indexed
  "[:a :b :c ...] -> {0 :a, 1 :b, 2 :c ...},
  useful for distinguishing between duplicate elements in `xs`."
  [xs]
  (persistent!
    (enc/reduce-indexed (fn [acc i x] (assoc! acc i x))
      (transient {}) xs)))

(comment (indexed [:a :b :c :d :d]))

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

(comment
  (counting-merge-sort identity [1 2 3 4 5]) ; 0 swaps
  (counting-merge-sort identity [1 2 4 3 5]) ; 1 swaps
  (counting-merge-sort identity [1 4 2 3 5]) ; 2 swaps
  (counting-merge-sort identity [4 1 2 3 5]) ; 3 swaps

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
        n-xy-ties_    (enc/counter)]

    (reduce-combinations
      (fn [_ [[x1 y1] [x2 y2]]]
        (let [sigx (Math/signum (- (double x2) (double x1)))
              sigy (Math/signum (- (double y2) (double y1)))]

          (case (* sigx sigy)
            +1.0 (n-concordant_)
            -1.0 (n-discordant_)
            ;; 0.0
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

(comment
  (do
    (require '[taoensso.tukey :as tukey])
    (def rcs  tukey/reduce-combinations))

  (let [xs  [1 1 4 6 2 3 4 5 5 2 4 8 6 3 5]
        ys  [1 3 5 3 4 3 7 5 5 9 1 3 8 6 3]
        xys (tukey/joint-vals xs ys)]

    [(kendall-correlation-impl-naive  rcs xys)
     (kendall-correlation-impl-knight     xys)
     (enc/qb 1e4
       (kendall-correlation-impl-naive  rcs xys)
       (kendall-correlation-impl-knight     xys))])

  [{:n_xt 10,   :n_yt 14,   :n_xyt 1,   :n_p 105.0, :n_c-n_d 6}
   {:n_xt 10.0, :n_yt 14.0, :n_xyt 1.0, :n_p 105.0, :n_c-n_d 6.0}
   [518.35 462.48]])
