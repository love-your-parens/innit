(ns innit.innit
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- ->multiline [line]
  (some-> (re-seq #"^([^;#]*)\\$" line)
          first
          second))

(comment
  (->multiline (first
               (str/split-lines "this-value = spans more \\
than one \\
line")))
  ;; => "this-value = spans more "
  (->multiline (first (str/split-lines "this-value = has an ; inline comment \\
so it can't escape line endings")))
  ;; => nil
  (->multiline "\\")
  ;; => ""
  )


(defn- ->section
  [line]
  (let [l (str/triml line)]
    (when (str/starts-with? l "[")
      ;; accept unterminated sections, e.g. '[my-section'
      (if-let [ends-at (str/index-of l "]" 1)]
        (subs l 1 ends-at)
        (subs l 1)))))

(comment
  (->section "[my-section-name]")
  ;; => "my-section-name"
  (->section "  [ take care with whitespace  ]")
  ;; => " take care with whitespace  "
  (->section "[ lenient about closing sections")
  ;; => " lenient about closing sections"
  (->section "not a section")
  ;; => nil
  (->section "also=not a section")
  ;; => nil
  )


(defn- ->continuation
  [line]
  (if-let [m (->multiline line)]
    [m true]
    [line false]))

(comment
  (->continuation "this line does not continue")
  ;; => ["this line does not continue" false]
  (->continuation "this line does continue \\")
  ;; => ["this line does continue " true]
  (->continuation "[the type of data doesn't matter]\\")
  ;; => ["[the type of data doesn't matter]" true]
  (->continuation "it=is just text")
  ;; => ["it=is just text" false]
  (->continuation "this continuation is ;; false \\")
  )


(defn- ->parameter
  [line]
  (if-let [separator-idx (str/index-of line "=")]
    (let [pname (subs line 0 separator-idx)
          [pvalue continues?] (->continuation (subs line (inc separator-idx)))]
      [(str/trim pname) pvalue continues?])
    [(str/trim line) nil false]))

(comment
  (->parameter "a=b")
  ;; => ["a" "b" false]
  (->parameter " a   =  the  value is quite    raw     ")
  ;; => ["a" "  the  value is quite    raw     " false]
  (->parameter "a")
  ;; => ["a" nil false]
  (->parameter "[abcd]")
  ;; => ["[abcd]" nil false]
  (->parameter (-> "this=is a multiline\\\nparamater" str/split-lines first))
  ;; => ["this" "is a multiline" true]
  )


(defn- ->value
  [s]
  (or
   ;; unquote
   (-> (re-seq #"^\s*\"(.*)\".*$" s) first second)
   ;; strip comments and whitespace
   (-> (re-seq #"^\s*([^;#]*[^;#\s]).*$" s) first second)))

(comment
  (->value "abc")
  ;; => "abc"
  (->value "  trim me please     ")
  ;; => "trim me please"
  (->value "ignore ;; this comment")
  ;; => "ignore"
  (->value "ignore # these ;; too")
  ;; => "ignore"
  (->value "\"  unwrap me but don't trim me  \"")
  ;; => "  unwrap me but don't trim me  "
  (->value "\"I am # part of the content \" # ...and I am not")
  ;; => "I am # part of the content "
)

(defn- fold
  [state section parameter value]
  (if (and parameter value)
    (assoc-in state [section parameter] (->value value))
    state))


(defn parse-ini-lines
  ([lines]
   (parse-ini-lines lines {} "" nil nil))
  ([lines state section param value]
   (let [[line & lines*] lines]
     (if line
       (if value
         ;; Continuation
         (let [[v continues?] (->continuation line)
               value* (str value "\n" v)]
           (if continues?
             (recur lines* state section param value*)
             (recur lines* (fold state section param value*) section nil nil)))
         (if-let [section* (->section line)]
           ;; New section
           (recur lines* state section* nil nil)
           ;; New parameter
           (let [[param* value* continues?] (->parameter line)]
             (recur lines*
                    (fold state section param* value*)
                    section
                    (when continues? param*)
                    (when continues? value*)))))
       ;; End of file - fold leftovers
       (fold state section param value)))))


(defn parse-ini-file
  [file-path]
  (-> file-path io/reader line-seq parse-ini-lines))


(defn parse-ini-string
  [s]
  (-> s str/split-lines parse-ini-lines))

(comment
  (def my-ini "param1=1
param2=2

[section1]
param1=11
param2=12

;; line comment
[section2]
param1= \"21\" ;; inline comment
param2=22

# different line comment

bonusmultiline=this line\\
continues ;; with trickery")
  (parse-ini-string my-ini)
  (def ini-lines (str/split-lines my-ini))
  (parse-ini-lines ini-lines)
;
)
