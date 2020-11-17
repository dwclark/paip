(in-package :paip-tests)

(defsuite unit-tests ()
  
  (deftest test-pattern-match
    (is (equal '((t t)) (pattern-match '(a b c) '(a b c))))
    (is (null (pattern-match '(need long match) '(need))))
    (is (null (pattern-match '(need long match) '(need long match right now))))
    (is (equal '((?X 1)) (pattern-match '(?x 2) '(1 2))))
    (is (equal '((?X HAPPY) (?Y LIVES))
               (pattern-match '(the ?x man (?* ?y) well) '(the happy man lives well))))
    (is (equal '((?X GOOD MAN)) (pattern-match '(the (?* ?x)) '(the good man))))
    (is (equal '((?X THE GOOD MAN)) (pattern-match '((?* ?x)) '(the good man)))))

  (deftest test-aux-functions
    (is (symbol-variable-p '?y))
    (is (not (symbol-variable-p 'y)))

    (is (simple-matcher-p '1))
    (is (simple-matcher-p 'y))
    (is (not (simple-matcher-p nil)))
    (is (not (simple-matcher-p '?y)))

    (is (variable-matcher-p '?y))
    (is (not (variable-matcher-p 'y)))

    (is (segment-matcher-p '(?* ?y)))
    (is (not (segment-matcher-p '(?y ?*))))
    (is (not (segment-matcher-p '?y)))

    (is (eq '?y (segment-variable '(?* ?y))))))


