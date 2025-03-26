# Innit

INI configuration format decoding & encoding library.

*Innit* attempts to be reasonably thorough, yet with no formal standards to follow - much is based on assumptions and real-life use-cases. It currently supports:

- sections
- comments and inline comments
- quoting
- multi-line keys & values

> This software is still in roughly alpha stage. Most of the functionality is complete, but not properly battle-tested.

## Caveats

- Section names and key names are all case-sensitive
- Comments are not preserved
- Multiple nesting is not supported
- Decoded values are not implicitly type-cast, e.g. `false` will produce a string `"false"` - not a boolean 

## Installation

### With Clojure CLI

Add *Innit* to your `deps.edn` under `:deps`, for example:

``` clojure
{:deps {io.github.love-your-parens/innit {:git/tag "..." :git/sha "..."}}}
```

### With Leiningen

*Innit* is not yet on Clojars, so the only way to resolve it via *Leiningen* is to use an extension that enables git dependencies.

## Usage

Examples assume that you pulled Innit into your namespace like so:

``` clojure
(require '[innit/innit :as ini])
```

### Decoding

``` clojure
  ;;; Here's how we decode strings:
  (ini/parse-ini-string "some_key = some value or other
[a section]
another key = and another value")
  ;; => {"" {"some_key" "some value or other"},
  ;;     "a section" {"another key" "and another value"}}
  
  ;;; Much the same for files:
  (ini/parse-ini-file "/path/to/file.ini")
  ;; Note that the argument here can be anything that java.io.File can grok.
```

### Encoding

*Innit* can only encode hash-maps. Every key designates a section. The outermost, top section should be indexed under `""` (empty string). Multiple nesting is not supported.

``` clojure
  ;;; Here's how we encode to string:
  (ini/encode-to-string {"" {1 1}
                         :some-section {11 1}})
  ;; => "1 = 1\n\n[some-section]\n11 = 1\n"

  ;;; And for files, we just provide a valid path:
  (encode-to-file {"" {:a 'a :b 'b}}
                  "/tmp/my-ini-file.ini")
  ;; Keep in mind that IO errors will throw!
```

## Roadmap

- Automatic output quoting
- Clojars repository 
- ClojureScript support
- Additional escape sequences

## License

Copyright © 2025 Konrad Wątor

Distributed under the MIT License.
