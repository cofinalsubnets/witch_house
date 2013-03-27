module WitchHouse.Wisp.STL (stl) where

stl :: String
stl = unlines $
  [ "(begin"

  , "  (define (list . l) l)"

  , "  (define cond"
  , "    (macro (case . cases)"
  , "      (fold (lambda (l c)"
  , "              `(if ,(car c)"
  , "                   (begin @,(cdr c))"
  , "                   ,l))"
  , "            '(error \"cond: fell through\")"
  , "            (reverse (cons case cases)))))"

  , "  (define let"
  , "    (macro (binds . body)"
  , "      `((lambda ,(map car binds) @,body) @,(map cadr binds))))"

  , "  (define (member e lst)"
  , "    (cond ((null? lst) #f)"
  , "          ((= (car lst) e) lst)"
  , "          (#t (member e (cdr lst)))))"

  , "  (define (length l)"
  , "    (if (null? l)"
  , "        0"
  , "        (+ 1 (length (cdr l)))))"

  , "  (define (comp f g)"
  , "    (lambda (n) (f (g n))))"

  , "  (define (car l)"
  , "    (if (null? l)"
  , "        (error \"car: null list\")"
  , "        (apply (lambda (h . t) h)"
  , "               l)))"

  , "  (define (cdr l)"
  , "    (if (null? l)"
  , "        l"
  , "        (apply (lambda (h . t) t)"
  , "               l)))"

  , "  (define (/= a b) (not (= a b)))"
  , "  (define (> a b) (and (not (= a b))"
  , "                       (not (< a b))))"
  , "  (define (>= a b) (or (= a b) (> a b)))"
  , "  (define (<= a b) (or (= a b) (< a b)))"

  , "  (define (curry fn)"
  , "    (define args '())"
  , "    (define (inner . as)"
  , "      (let ((cas (append args as)))"
  , "        (cond ((= (length cas) (arity fn)) (apply fn cas))"
  , "              ((> (length cas) (arity fn))"
  , "               (error \"curry: too many arguments\"))"
  , "              (#t (set! args cas) inner))))"
  , "    inner)"

  , "  (define caar (comp car car))"
  , "  (define cadr (comp car cdr))"
  , "  (define cddr (comp cdr cdr))"
  , "  (define cdar (comp cdr car))"

  , "  (define caaar (comp car caar))"
  , "  (define caadr (comp car cadr))"
  , "  (define cadar (comp car cdar))"
  , "  (define caddr (comp car cddr))"
  , "  (define cdaar (comp cdr caar))"
  , "  (define cdadr (comp cdr cadr))"
  , "  (define cddar (comp cdr cdar))"
  , "  (define cdddr (comp cdr cddr))"

  , "  (define (assoc v l)"
  , "    (cond ((null? l) #f)"
  , "          ((= v (caar l)) (cadar l))"
  , "          (#t (assoc v (cdr l)))))"

  , "  (define (map op l)"
  , "    (if (null? l)"
  , "        l"
  , "        (cons (op (car l))"
  , "              (map op (cdr l)))))"

  , "  (define (filter p l)"
  , "    (cond ((null? l) l)"
  , "          ((p (car l))"
  , "           (cons (car l)"
  , "                 (filter p (cdr l))))"
  , "          (#t (filter p (cdr l)))))"

  , "  (define (fold op acc l)"
  , "    (if (null? l)"
  , "        acc"
  , "        (fold op (op acc (car l)) (cdr l))))"

  , "  (define (not v) (if v #f #t))"
  , "  (define (and a b) (if a b a))"
  , "  (define (or a b) (if a a b))"

  , "  (define (reverse l)"
  , "    (define (inner acc l)"
  , "      (if (null? l)"
  , "          acc"
  , "          (inner (cons (car l) acc)"
  , "                 (cdr l))))"
  , "    (inner '() l))"

  , "  (define (inc n) (+ n 1))"
  , "  (define (dec n) (- n 1))"
  , "  (define (id n) n)"
  , "  (define (const x) (lambda (y) x))"

  , "  (define (rem a b)"
  , "    (- a (* b (/ a b))))"

  , "  (define floor int)"

  , "  (define (ceil n)"
  , "    (let ((f (floor n)))"
  , "      (if (= n f) f (inc f))))"

  , "  (define (abs n) (* n (/ n n)))"

  , "  (define (append l1 l2)"
  , "    (if (null? l1)"
  , "        l2"
  , "        (cons (car l1)"
  , "              (append (cdr l1) l2))))"

  , "  (define (juxt f g)"
  , "    (lambda (h . t)"
  , "      (list (apply f (cons h t))"
  , "            (apply g (cons h t)))))"

  , "  (define (join strs j)"
  , "    (fold (lambda (s1 s2) (cat s1 j s2)) (car strs) (cdr strs)))"

  , ")"
  ]

