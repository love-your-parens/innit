(ns innit.innit
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; FIXME
;; Quoting is quirky and should be considered experimental at the moment.
;; It won't correctly protect any tokens, i.e.: `=` `#` `;`.
;; It's only effective at preserving whitespace.
;;
;; FIXME
;; This won't continue:
;; abcd \# efgh \
;;
;; FIXME
;; This retains the backslash:
;; escaped \# comment
;;
;; TODO
;; The algorithm needs to be cleaned up wrt escape signs. Perhaps have one
;; `unescape` fn applied to individual strings at the tail of the process.
;; Similarly, subroutines like `->value` should perhaps be ignorant to comments,
;; relying instead on pre-pruning.

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


(defn- all-indexes-of
  "Similar to clojure.string/index-of, except it returns a lazy seq of all indexes."
  ([s search]
   (all-indexes-of s search nil))
  ([s search from-index]
   (let [match (if from-index
                 (str/index-of s search from-index)
                 (str/index-of s search))]
     (when match
       (lazy-seq (cons match (all-indexes-of s search (inc match))))))))

^:rct/test
(comment
  (all-indexes-of "alibaba" "a")
  ;; => (0 4 6)
  (all-indexes-of "alibaba" "a" 3)
  ;; => (4 6)
  (all-indexes-of "alibaba" "a" 7)
  ;; => nil
  )


(defn- escaped?
  "Checks if a character at the specified index in the string is escaped.
  Very simplistic, will not correctly interpret complex scenarios with
  subsequent escapes, for example: x in `\\\\x` will be seen as escaped."
  [string char-index]
  (and (pos? char-index)
       (< char-index (count string))
       (= \\ (nth string (dec char-index)))))

^:rct/test
(comment
  (escaped? "0123" 2)
  ;; => false
  (escaped? "012\\4" 4)
  ;; => true
  (escaped? "" -1)
  ;; => false
  (escaped? "" 100)
  ;; => false
  )


(defn- prune-comments
  "Prunes all unescaped comments from a single line."
  [line]
  (let [comment-idxs (filter (complement (partial escaped? line))
                             (concat (all-indexes-of line "#")
                                     (all-indexes-of line ";")))]
    (if-let [comment-idx (when (seq comment-idxs) (reduce min comment-idxs))]
      (subs line 0 comment-idx)
      line)))

(comment
  (prune-comments "No comments")
  ;; => "No comments"
  (prune-comments "This line should end # right here")
  ;; => "This line should end "
  (prune-comments "; this line is effectively empty")
  ;; => ""
  (prune-comments "this comment \\; escapes")
  ;; => "this comment \\; escapes"
  (prune-comments "this one \\;; only pretends to")
  ;; => "this one \\;"
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
   (str/trim (prune-comments s))))

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
   (let [[l & lines*] lines]
     (if-let [line (when l (prune-comments l))]
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


(defn parse-ini-string
  "Parses an .ini string into a well formed hash-map."
  [s]
  (-> s str/split-lines parse-ini-lines))

^:rct/test
(comment
  (parse-ini-string ";;;; INI string test
test.name = \" - INI string test - \"
# line comment
## --------
test.empty_values = ; void value, inline comment
test.multiple_line_values = this must\\
continue

; this-parameter = is commented out and should not be parsed
# this = is commented out too

[section-1]
nested-key   =    with a lot of unnecessary white space
test.name = should not produce a duplicate
test.NAME = should not produce a duplicate either...
test.NAME = ... but should get overwritten

[section-2]
# Empty sections are omitted")
  ;; => {""
  ;;     {"test.name" " - INI string test - ",
  ;;      "test.empty_values" "",
  ;;      "test.multiple_line_values" "this must\ncontinue"},
  ;;     "section-1"
  ;;     {"nested-key" "with a lot of unnecessary white space",
  ;;      "test.name" "should not produce a duplicate",
  ;;      "test.NAME" "... but should get overwritten"}}
  )


(defn- encode-entity
  "Encodes arbitrary data into ini format entities."
  [entity]
  (str/replace (cond
                 (nil? entity) ""
                 (false? entity) "0"
                 (keyword? entity) (name entity)
                 :else (-> entity str str/trim)) "\n" "\\\n"))

^:rct/test
(comment
  (encode-entity "No funny business going on at all!")
  ;; => "No funny business going on at all!"
  (encode-entity false)
  ;; => "0"
  (encode-entity :a-keyword)
  ;; => "a-keyword"
  (encode-entity nil)
  ;; => ""
  (encode-entity "this entity
spans
many lines")
  ;; => "this entity\\\nspans\\\nmany lines"
  (encode-entity "no=policing so=take care")
  ;; => "no=policing so=take care"
  )


(defn- encode-section
  "Encodes an ini section."
  [section-name]
  (format "[%s]" (encode-entity section-name)))

^:rct/test
(comment
  (encode-section "section 1")
  ;; => "[section 1]"
  (encode-section "no [policing]")
  ;; => "[no [policing]]"
  (encode-section "multiline\nis supported")
  ;; => "[multiline\\\nis supported]"
  )


(defn- encode-1
  "Takes ini-encodable input (a hashmap),
  uses it to produce a single ini-entity (section, parameter),
  returns a vector of: [encoded entity, updated input, currently visited section],
  or `nil` upon exhausting all valid input."
  ([input]
   (encode-1 input nil))
  ([input section]
   (if section
     (if-let [section-seq (seq (get input section))]
       ;; Section not empty - extract and serialize the next pair
       (let [[k v] (first section-seq)]
         [(if (nil? k) "" (format "%s = %s"
                                  (encode-entity k)
                                  (encode-entity v)))
          (update-in input [section] #(dissoc % k))
          section])
       ;; Section exhausted - drop it and continue
       ["" (dissoc input section) nil])
     ;; Nominate the next section
     (if (get input "") ; always start from the outer section
       (recur input "")
       (when-let [[k] (first input)]
         [(encode-section k) input k])))))

^:rct/test
(comment
  (let [ini-map {"" {1 1}
                 :some-section {11 1
                                12 2}}
        [o1 i1 s1] (encode-1 ini-map)
        [o2 i2 s2] (encode-1 i1 s1)
        [o3 i3 s3] (encode-1 i2 s2)]
    [[o1 o2 o3]
     [i1 i2 i3]
     [s1 s2 s3]])
  ;; => [["1 = 1" "" "[some-section]"]
  ;;     [{"" {}, :some-section {11 1, 12 2}}
  ;;      {:some-section {11 1, 12 2}}
  ;;      {:some-section {11 1, 12 2}}]
  ;;     ["" nil :some-section]]
  )


(defn- iterable-encode-1
  "Composes ini-entity encoding into an iterable process.
  The encoded entity is returned in the third position."
  [[input section & _]]
  (when-let [[output* input* section*] (encode-1 input section)]
    [input* section* output*]))

^:rct/test
(comment
  (let [ini-map {"" {1 1}
                 :some-section {11 1
                                12 2}}
        ini-iterator (iterate iterable-encode-1 [ini-map])]
    (->> ini-iterator (drop 1) (take 3)))
  ;; => ([{"" {}, :some-section {11 1, 12 2}} "" "1 = 1"]
  ;;     [{:some-section {11 1, 12 2}} nil ""]
  ;;     [{:some-section {11 1, 12 2}} :some-section "[some-section]"])
  )


(defn encode-as-lazy-seq
  [hash-map]
  (->> (iterate iterable-encode-1 [hash-map])
       (drop 1)
       (map (fn [[_ _ s]] s))
       (take-while identity)))


(defn encode-to-string
  "Encodes data as ini and returns it as a string."
  [hash-map]
  (str/join "\n" (encode-as-lazy-seq hash-map)))

^:rct/test
(comment
  (encode-to-string {"" {1 1}
                     :some-section {11 1}})
  ;; => "1 = 1\n\n[some-section]\n11 = 1\n"
  ;
  )


(defn encode-to-file
  "Encodes data as ini and writes it directly to a file.
  Data should be an ini-encodable hashmap.
  File can be anything that converts to `java.io.File`.
  Throws on IO errors."
  [hash-map file]
  (let [f (io/file file)
        w (io/writer f)
        ini (encode-as-lazy-seq hash-map)]
    (try
      (doseq [s ini]
        (.append w s)
        (.append w "\n"))
      (finally (.close w)))))

(comment
  (encode-to-file {"" {:a 'a :b 'b}}
                  "/tmp/my-ini-file.ini")
  ;
  )
