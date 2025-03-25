(ns innit.innit
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn- ->multiline
  "Parses line as multiline.
  Drops the escape sequence."
  [line]
  (some-> (re-seq #"^([^;#]*)\\$" line)
          first
          second))

^:rct/test
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
  "Parses line as a section when valid."
  [line]
  (let [l (str/triml line)]
    (when (str/starts-with? l "[")
      ;; accept unterminated sections, e.g. '[my-section'
      (if-let [ends-at (str/index-of l "]" 1)]
        (subs l 1 ends-at)
        (subs l 1)))))

^:rct/test
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
  "Evaluates line as a potential continuation.
  Returns [evaluated-line continues?]."
  [line]
  (if-let [m (->multiline line)]
    [m true]
    [line false]))

^:rct/test
(comment
  (->continuation "this line does not continue")
  ;; => ["this line does not continue" false]
  (->continuation "this line does continue \\")
  ;; => ["this line does continue " true]
  (->continuation "[the type of data doesn't matter]\\")
  ;; => ["[the type of data doesn't matter]" true]
  (->continuation "it=is just text")
  ;; => ["it=is just text" false]
  (->continuation "this continuation is ;; a fake \\")
  ;; => ["this continuation is ;; a fake \\" false]
  )


(defn- ->parameter
  "Parses line as a parameter, i.e. a key=value pair.
  Returns [key value continues?]"
  [line]
  (let [[l continues?] (->continuation line)]
    (if-let [separator-idx (str/index-of l "=")]
      (let [pname (str/trim (subs l 0 separator-idx))
            pvalue (subs l (inc separator-idx))]
        [pname pvalue continues?])
      [(str/trim l) nil continues?])))

^:rct/test
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
  (->parameter (-> "this is a multiline\\\nkey" str/split-lines first))
  ;; => ["this is a multiline" nil true]
  )


(defn- ->value
  "Parses a string as a configuration value.
  Unquotes, prunes comments, trims."
  [s]
  (or
   ;; unquote
   (-> (re-seq #"^\s*\"(.*)\".*$" s) first second)
   ;; strip comments and whitespace
   (-> (re-seq #"^\s*([^;#]*[^;#\s]).*$" s) first second)))

^:rct/test
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
  "Folds pending datapoint into the state container."
  [state section parameter value]
  (if (and parameter value)
    (assoc-in state [section parameter] (->value value))
    state))


(defn parse-ini-lines
  "Parses a sequence of .ini string lines into a well formed hash-map."
  ([lines]
   (parse-ini-lines lines {} "" nil nil))
  ([lines state section param value]
   (let [[line & lines*] lines]
     (if line
       ;; Continuation...
       (if param
         (if value
           ;; ...of value
           (let [[v continues?] (->continuation line)
                 value* (str value "\n" v)]
             (if continues?
               (recur lines* state section param value*)
               (recur lines* (fold state section param value*) section nil nil)))
           ;; ...of key
           (let [[p value* continues?] (->parameter line)
                 param* (str param "\n" p)]
             (if continues?
               (recur lines* state section param* value*)
               (recur lines* (fold state section param* value*) section nil nil))))
         ;; New...
         (if-let [section* (->section line)]
           ;; ...section
           (recur lines* state section* nil nil)
           ;; ...parameter
           (let [[param* value* continues?] (->parameter line)]
             (if continues?
               (recur lines* state section param* value*)
               (recur lines* (fold state section param* value*) section nil nil)))))
       ;; End of file - fold leftovers
       (fold state section param value)))))


(defn parse-ini-file
  "Parses an .ini file into a well formed hash-map."
  [file-path]
  (-> file-path io/reader line-seq parse-ini-lines))


;; FIXME add a unit test
(defn parse-ini-string
  "Parses an .ini string into a well formed hash-map."
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

multiline=this line\\
continues ;; with trickery

multiline\\
keys=are supported!")
  (parse-ini-string my-ini)
  (parse-ini-lines (str/split-lines my-ini))

;
  )
