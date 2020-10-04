(ns taoensso.tukey-tests
  (:require
   [clojure.test                     :as test :refer [is]]
   ;; [clojure.test.check            :as tc]
   ;; [clojure.test.check.generators :as tc-gens]
   ;; [clojure.test.check.properties :as tc-props]
   [clojure.string      :as str]
   [taoensso.encore     :as enc]
   [taoensso.tukey      :as tukey]
   [taoensso.tukey.impl :as impl]))

(comment
  (remove-ns      'taoensso.tukey-tests)
  (test/run-tests 'taoensso.tukey-tests)
  (test/run-all-tukey-tests))

(defn run-all-tukey-tests []
  (test/run-tests
    'taoensso.tukey.impl
    'taoensso.tukey
    'taoensso.tukey-tests))

#?(:cljs (do (enable-console-print!) (run-all-tukey-tests)))
