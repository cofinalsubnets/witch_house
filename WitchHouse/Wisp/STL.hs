module WitchHouse.Wisp.STL (stl) where

stl :: String
stl = unlines $
  [ "(do"
  , "  (df dfn (mfn (-fn-name -fn-params & -fn-body)"
  , "    `(df ,-fn-name (fn ,-fn-params @,-fn-body))))"
  , "  (df dfm (mfn (-mf-name -mf-params & -mf-body)"
  , "    `(df ,-mf-name (mfn ,-mf-params @,-mf-body))))"

  , "  (dfn list (& l) l)"

  , "  (dfm cond (& cases)"
  , "    (fold (fn (l c)"
  , "            `(if ,(car c)"
  , "                 (do @,(cdr c))"
  , "                 ,l))"
  , "          '(error \"cond: fell through\")"
  , "          (reverse cases)))"

  , "  (dfm let (binds & body)"
  , "    `((fn ,(map car binds) @,body) @,(map cadr binds)))"

  , "  (dfn member (e lst)"
  , "    (cond ((null? lst) #f)"
  , "          ((= (car lst) e) lst)"
  , "          (#t (member e (cdr lst)))))"

  , "  (dfn length (l)"
  , "    (if (null? l)"
  , "        0"
  , "        (+ 1 (length (cdr l)))))"

  , "  (dfn null? (l)"
  , "    (if (list? l)"
  , "      (= l '())"
  , "      (error (str \"ERROR: Bad type: \" l))))"

  , "  (dfn flip (f a b) (f b a))"

  , "  (dfn comp (f g)"
  , "    (fn (n) (f (g n))))"

  , "  (dfn car (l)"
  , "    (if (null? l)"
  , "        (error \"car: null list\")"
  , "        (apply (fn (h & _) h)"
  , "               l)))"

  , "  (dfn cdr (l)"
  , "    (if (null? l)"
  , "        l"
  , "        (apply (fn (_ & t) t)"
  , "               l)))"

  , "  (dfn /= (a b) (not (= a b)))"
  , "  (dfn > (a b) (and (not (= a b))"
  , "                       (not (< a b))))"
  , "  (dfn >= (a b) (or (= a b) (> a b)))"
  , "  (dfn <= (a b) (or (= a b) (< a b)))"

  , "  (df caar (comp car car))"
  , "  (df cadr (comp car cdr))"
  , "  (df cddr (comp cdr cdr))"
  , "  (df cdar (comp cdr car))"

  , "  (df caaar (comp car caar))"
  , "  (df caadr (comp car cadr))"
  , "  (df cadar (comp car cdar))"
  , "  (df caddr (comp car cddr))"
  , "  (df cdaar (comp cdr caar))"
  , "  (df cdadr (comp cdr cadr))"
  , "  (df cddar (comp cdr cdar))"
  , "  (df cdddr (comp cdr cddr))"

  , "  (dfn assoc (v l)"
  , "    (cond ((null? l) #f)"
  , "          ((= v (caar l)) (cadar l))"
  , "          (#t (assoc v (cdr l)))))"

  , "  (dfn map (op l)"
  , "    (if (null? l)"
  , "        l"
  , "        (cons (op (car l))"
  , "              (map op (cdr l)))))"

  , "  (dfn filter (p l)"
  , "    (cond ((null? l) l)"
  , "          ((p (car l))"
  , "           (cons (car l)"
  , "                 (filter p (cdr l))))"
  , "          (#t (filter p (cdr l)))))"

  , "  (dfn fold (op acc l)"
  , "    (if (null? l)"
  , "        acc"
  , "        (fold op (op acc (car l)) (cdr l))))"

  , "  (dfn cons (new-car new-cdr)"
  , "    (if (list? new-cdr)"
  , "     `(,new-car @,new-cdr)"
  , "     (error (str \"Bad type: \" new-cdr))))"

  , "  (dfn not (v) (if v #f #t))"
  , "  (dfn && (a b) (if a b a))"
  , "  (dfn || (a b) (if a a b))"
  , "  (dfm and (& clauses)"
  , "    (let (((cls1 & clss) (reverse clauses)))"
  , "      (fold (fn (prev cur) `(let ((current-clause ,cur)) (if current-clause ,prev current-clause))) cls1 clss)))"
  , "  (dfm or (& clauses)"
  , "    (let (((cls1 & clss) (reverse clauses)))"
  , "      (fold (fn (prev cur) `(let ((current-clause ,cur)) (if current-clause current-clause ,prev))) cls1 clss)))"

  , "  (dfn reverse (l)"
  , "    (dfn inner (acc l)"
  , "      (if (null? l)"
  , "          acc"
  , "          (inner (cons (car l) acc)"
  , "                 (cdr l))))"
  , "    (inner '() l))"

  , "  (dfn inc (n) (+ n 1))"
  , "  (dfn dec (n) (- n 1))"
  , "  (dfn id (n) n)"
  , "  (dfn const (x) (fn (y) x))"

  , "  (dfn rem (a b)"
  , "    (- a (* b (/ a b))))"

  , "  (df floor int)"

  , "  (dfn ceil (n)"
  , "    (let ((f (floor n)))"
  , "      (if (= n f) f (inc f))))"

  , "  (dfn abs (n) (* n (/ n n)))"

  , "  (dfn append (l1 l2)"
  , "    (if (null? l1)"
  , "        l2"
  , "        (cons (car l1)"
  , "              (append (cdr l1) l2))))"

  , "  (dfn juxt (f g)"
  , "    (fn (& t)"
  , "      (list (apply f t)"
  , "            (apply g t))))"

  , "  (dfn join (strs j)"
  , "    (fold (fn (s1 s2) (str s1 j s2)) (car strs) (cdr strs)))"

  , ")"
  ]

