(ns innit.util
  (:require [clojure.java.io :as io]
            [com.mjdowney.rich-comment-tests :as rct])
  (:import (clojure.lang Namespace)))

(defn- ->ns
  [x]
  (if (instance? Namespace x)
    x
    (find-ns (symbol x))))

(defn- ns->resource-path
  [^Namespace ns]
  {:pre [(instance? Namespace ns)]}
  (str (->> ns
            str
            (replace {\- \_})
            (replace {\. \/})
            (apply str))
       ".clj"))

(comment
  (ns->resource-path *ns*)
  )

(defn- resource-path->file
  [path]
  (some-> path
          io/resource
          io/file))

(def ns->file
  (comp resource-path->file ns->resource-path))

(comment
  (ns->file *ns*)
  )

(defn run-rich-comment-tests-in-ns!
  [ns]
  (let [ns' (->ns ns)]
    (rct/run-file-tests! (-> ns' ns->file str) ns')))

(comment
  (run-rich-comment-tests-in-ns! *ns*)
  )
