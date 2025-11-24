(defpackage :paip-gps
  (:use #:cl #:alexandria))

(defpackage :paip-eliza
  (:use #:cl #:alexandria))

(defpackage :paip-trie
  (:use #:cl #:alexandria)
  (:export #:trie-search #:trie-delete #:trie-size #:make-trie))
