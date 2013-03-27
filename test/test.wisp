(test
  ("control - this test should fail" #f)
  ("map"
    (let ((r (map inc '(1 2 3))))
      (= r '(2 3 4))))
  ("let, currying, and scoping"
    (let ((a 1) (b 2) (c 3) (add3 (lambda (a b c) (+ a b c))))
      (and (= a 1)
           (= b 2)
           (= c 3)
           (= 6 (add3 a b c))
           (= 6 (((add3 a) b) c)))))
  ("type predicates"
   (and (list? '(1))
        (number? 72165972.451)
        (string? "no way bro")
        (symbol? 'hahawow)
        (bool? #f)
        (ref? (make-ref))
        (ref? (make-self-ref))
        (not (string? 'wat))
        (not (list? "glug"))
        (not (primitive? list))
        (primitive? +)
        (not (number? "pew pew pew"))))
  )

