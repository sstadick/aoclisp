(define foldr (func end input) (if (nil? input) end (func (car input) (foldr func end (cdr input)))))
(define reduce (func end input) (foldr func end input))
(define map (func lst) (foldr (lambda (x y) (pair (func x) y)) '() lst))
(define filter (pred lst) (foldr (lambda (x y) (if (pred x) (pair x y) y)) '() lst))
(define foldl (func acc input) (if (nil? input) acc (foldl func (func acc (car input)) (cdr input)))) 
(define fold (func acc input) (foldl func acc input))
(define not (x) (if x #f #t))
(define sum (lst) (fold + 0 lst))
(define product (lst) (fold * 1 lst))
(define all (lst) (fold and #t lst))
(define any (lst) (fold or #f lst))
(define second (lst) (car (cdr lst)))


(define get-red (colors) (car colors))
(define get-blue (colors) (car (cdr colors)))
(define get-green (colors) (car (cdr (cdr colors))))
(define set-red (red colors) (list red (get-blue colors) (get-green colors)))
(define set-blue (blue colors) (list (get-red colors) blue (get-green colors)))
(define set-green (green colors) (list (get-red colors) (get-blue colors) green))
(define get-cubes (game) (second game))

(define game-parts (game) (map chomp (split game '>)))
(define parse-game-id (game-id) (int_of_string (car (cdr (map chomp (split-ascii-whitespace game-id))))))
(define cube-to-color (cube) (
    list (second (split-ascii-whitespace cube)) (int_of_string (car (split-ascii-whitespace cube))))
)
(define cubes-to-colors (cubes) ( 
    fold
        (lambda (acc input) 
            (cond 
                ((eq (car input) 'red) (set-red (second input) acc))
                ((eq (car input) 'blue) (set-blue (second input) acc))
                ((eq (car input) 'green) (set-green (second input) acc))
            )
        ) '(0 0 0) (map cube-to-color (map chomp (split cubes ',)))
))

(define parse-cube-sets (game) (map cubes-to-colors (map chomp (split game '!))))

(define test-game-a (game min-cubes) (
    fold (
        lambda (acc colors) (
            list 
                (and (get-red acc)   (or (< (get-red colors)   (get-red min-cubes))   (= (get-red colors) (get-red min-cubes))))
                (and (get-blue acc)  (or (< (get-blue colors)  (get-blue min-cubes))  (= (get-blue colors) (get-blue min-cubes))))
                (and (get-green acc) (or (< (get-green colors) (get-green min-cubes)) (= (get-green colors) (get-green min-cubes))))
        )) '(#t #t #t) (get-cubes game)
))

(val text (lines 'input.txt))
(val parts (map game-parts text))
(val games (map (lambda (game) (list (parse-game-id (car game)) (parse-cube-sets (second game)) )) parts))
(print (sum (map (lambda (game) (car game)) (filter (lambda (game) (all (test-game-a game '(12 14 13)))) games))))
