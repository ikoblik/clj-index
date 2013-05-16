# clj-index [![Build Status](https://travis-ci.org/ikoblik/clj-index.png?branch=master)](https://travis-ci.org/ikoblik/clj-index)

Indexing and matching library written in Clojure. Works on sequences
of any type.

## Usage

### String index

There are currently two ways to index a string.

1. Boyer-Moore.
2. Knuth-Morris-Pratt.

To index a string use either bm-index or kmp-index. This will create
an instance of Matcher protocol that supports method match.

    (use 'clj-index.core)
    (def indexed-word (bm-index "word"))
    (println (match indexed-word "This string contains word word"))
    user=> (21 26)

`(21 26)` are indexes of the first characters of the word's occurances 
in the string. This search is lazy, so it can be potentially used 
on indefinite sequences.

Indexing and search is not constrained to strings. Example of indexing
array of integers and searching in an indefinite sequence.

    (def five-to-seven (bm-index [5 6 7]))
    (first (match five-to-seven (iterate inc 0)))
    user=> 5

### Dictionary index

To index a dictionary there's an implementation of Aho-Corasick method.
This algorithm guarantees `O(n)` search of dictionary words in text, 
where `n=length(text)`.

    (use 'clj-index.core)
    (def dict (ac-index ["this" "is" "test"])
    (match dict "testing search in this string")
    user=> ((0 3) (18 21) (20 21))

Here match returns list of indeces of first and last characters of the 
matching substrings. Returned intervals may overlap, in the example
string `this` matches `this` and its suffix matches `is`.

Aho-Corasick index can also be used for other data types:

    (def intervals (ac-index [[4 5 6] [100 101 102]]))
    (take 2 (match intervals (iterate inc 0)))
    user=> ((4 6) (100 102))

## Installation

Add dependency to project.clj:

    [org.clojars.ikoblik/clj-index "0.0.2"]

or download the sources, and run command:

    $ lein jar

If you don't have lein installed please install it from here:
https://github.com/technomancy/leiningen

Lein version 2.x is required.

## License

Source Copyright © 2011-2012 Ivan Koblik.
Distributed under the Eclipse Public License, the same as Clojure
uses.
http://opensource.org/licenses/eclipse-1.0.php
