(ns taoensso.tukey-tests
  (:require
   [clojure.test                  :as test :refer [is]]
   [clojure.test.check            :as tc]
   [clojure.test.check.generators :as tc-gens]
   [clojure.test.check.properties :as tc-props]
   [clojure.string      :as str]
   [taoensso.encore     :as enc]
   [taoensso.tukey      :as tukey]
   [taoensso.tukey.impl :as impl])
  ;; (:import [taoensso.tukey.impl])
  )

(comment
  (remove-ns      'taoensso.tukey-tests)
  (test/run-tests 'taoensso.tukey-tests))

(test/deftest _todo
  (is (= true true)))
