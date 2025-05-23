# Innit

INI configuration format decoding & encoding library.

*Innit* attempts to be reasonably thorough, yet with no formal standards to follow - much is based on assumptions and real-life use-cases. It currently supports:

- sections
- comments, inline comments
- escaped comment-signs
- quoting to preserve whitespace
- multi-line keys & values

> This software is still in roughly alpha stage. Most of the functionality is complete, but not properly battle-tested.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Innit](#innit)
  - [Caveats](#caveats)
  - [Installation](#installation)
    - [With Clojure CLI](#with-clojure-cli)
    - [With Leiningen](#with-leiningen)
  - [Usage](#usage)
    - [Decoding](#decoding)
    - [Encoding](#encoding)
    - [Escaping](#escaping)
  - [Roadmap](#roadmap)
  - [License](#license)

<!-- markdown-toc end -->

## Caveats

- Section names and key names are all case-sensitive
- Comments are not preserved
- Multiple nesting is not supported
- Decoded values are not implicitly type-cast, e.g. `false` will produce a string `"false"` - not a boolean

*Innit* is not an advanced parser. It operates on a simplified, pattern-based model. It may trip over gotchas.

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

### Escaping

*Innit* only allows you to escape the comment sign (`;` or `#`), and the newline sign (which produces a multi-line). Both are accomplished by using the `\` (backslash) character. For example:

```
this line # cuts off
this one \# does not
```

And similarly:

```
this line\
wraps around
```

The escape sign itself cannot be escaped, so phrases like `\\#` will not be interpreted how you might expect.

> Note:
> Neither decoding, nor encoding an INI string will alter escape signs!

To remove escape signs from a string, simply use `unescape`. Conversely, use `escape` to protect all eligible characters.

``` clojure
(ini/escape "This is not # meant to be a comment")
;; => "This is not \\# meant to be a comment"

(ini/unescape "This is not \\# meant to be a comment")
;; => "This is not # meant to be a comment"
```

`escape` is idempotent:

``` clojure
(ini/escape "This is not \\# meant to be a comment")
;; => "This is not \\# meant to be a comment"
```

## Roadmap

- Automatic output quoting
- Clojars repository 
- ClojureScript support

## License

Copyright © 2025 Konrad Wątor

Distributed under the MIT License.
