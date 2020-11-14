(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defun random-elt (set)
  (elt set (random (length set))))

(defun one-of (set)
  (list (random-elt set)))

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generates random sentence or phrase"
  (cond ((listp phrase) ;has more production rules, map/append results of going down levels
         (mappend #'generate phrase))

        ((rewrites phrase) ;has re-writes, not an actual word, keep going down
         (generate (random-elt (rewrites phrase))))

        (t (list phrase)))) ;we found a single word, put in list and return for mappend

(defun generate-2.1 (phrase)
  "Generates random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate-2.1 phrase))
      (let ((rw (rewrites phrase)))  
        (if rw
            (generate-2.1 (random-elt rw))
            (list phrase))))

(defun rewrites-p (phrase)
  (if (rewrites phrase) t nil))

(defun generate-2.2 (phrase)
  "Generates random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate-2.2 phrase))

        ((rewrites-p phrase)
         (generate-2.2 (random-elt (rewrites phrase))))

        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))

        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))

        (t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))

        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))

        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))

        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

(defun combine-all-x-product (xlist ylist)
  (cross-product #'append xlist ylist))

(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y)) xlist))
           ylist))
