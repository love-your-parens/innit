(ns innit.innit-test
  (:require [clojure.test :refer :all]
            [innit.innit :refer :all]
            [innit.util :refer [run-rich-comment-tests-in-ns!]]))

(deftest rich-comment-tests
  (run-rich-comment-tests-in-ns! 'innit.innit))
