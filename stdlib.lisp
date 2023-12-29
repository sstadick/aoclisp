
(define foldr (func end input) (if (nil? input) end (func (car input) (foldr func end (cdr input)))))
(define reduce (func end input) (foldr func end input))
(define map (func lst) (foldr (lambda (x y) (pair (func x) y)) '() lst))
(map (lambda (x) (+ x 1)) '(1 1 1))

(define filter (pred lst) (foldr (lambda (x y) (if (pred x) (pair x y) y)) '() lst))
(filter (lambda (x) (> x 10)) '(1 1 11 12))

(define foldl (func acc input) (if (nil? input) acc (foldl func (func acc (car input)) (cdr input)))) 
(define fold (func acc input) (foldl func acc input))
(fold + 0 '(1 2 3 4))

(define not (x) (if x #f #t))
(not #t)

(define sum (lst) (fold + 0 lst))
(sum '(1 2 3 4))

(define product (lst) (fold * 1 lst))
(product '(1 2 3 4))

(define all (lst) (fold and #t lst))
(all '(#t #t #f))

(define any (lst) (fold or #f lst))
(any '(#t #t #f))

(define second (lst) (car (cdr lst)))