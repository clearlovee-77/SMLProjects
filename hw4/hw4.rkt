
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define ones (lambda () (cons 1 ones)))
;1.
[define [sequence low high stride]
  [if [> low high]
      null
      [append [list low] [sequence [+ low stride] high stride]]]]

;2.
[define [string-append-map xs suffix]
  [map [lambda [str1] [string-append str1 suffix]] xs]]

;3.
[define [list-nth-mod xs n]
  [cond [[< n 0] (error "list-nth-mod: negative number")]
        [[null? xs] (error "list-nth-mod: empty list")]
        [#t [car [list-tail xs [remainder n [length xs]]]]]]]

;4.
[define [stream-for-n-steps s n]
  [if [= n 0]
      null
      [append [list [car [s]]] [stream-for-n-steps [cdr [s]] [- n 1]]]]]

;5.
[define [f x] [cons [if [= [remainder x 5] 0]
                        [- 0 x]
                        x] [lambda[] [f [+ x 1]]]]]
[define [funny-number-stream] [f 1]]

;6.
[define [g x] [cons [if [equal? x "dan.jpg"]
                        [begin [set! x "dog.jpg"] x]
                        [begin [set! x "dan.jpg"] x]] [lambda[] [g x]]]]
[define [dan-then-dog] [g "dog.jpg"]]

;7.
[define [add-zero x]
  [cons 0 [car [x]]]]
[define [stream-add-zero s]
  [lambda[]
    [cons [add-zero s] [stream-add-zero [cdr [s]]]]]]

;8.
[define [cycle-lists xs ys]
  [letrec [[f [lambda[x]
                [cons [cons [list-nth-mod xs x] [list-nth-mod ys x]] [lambda[] [f [+ x 1]]]]]]]
    [lambda[] [f 0]]]]

;9.
[define [vector-assoc v vec]
  [letrec [[f [lambda[x]
                [if [= x [vector-length vec]]
                    #f
                [if [not [pair? [vector-ref vec x]]]
                    [f [+ x 1]]
                 [if [equal? [car [vector-ref vec x]] v]
                             [vector-ref vec x]
                             [f [+ x 1]]]]]]]]
    [f 0]]]

;10.
[define [cached-assoc xs n]
  [letrec [[memo [make-vector n #f]]
           [next-slot 0]
           [f [lambda[x]
                [let [[ans [vector-assoc x memo]]]
                  [if ans
                      ans
                      [let [[new-ans [assoc x xs]]]
                        [if [equal? #f new-ans]
                          new-ans
                          [begin [vector-set! memo next-slot new-ans]
                                 [if [= next-slot [- n 1]]
                                     [set! next-slot 0]
                                     [set! next-slot [+ next-slot 1]]]]]
                          new-ans]]]]]]
    f]]

;11.
[define-syntax while-less
  [syntax-rules [do]
    [[while-less e1 do e2]
     [[letrec [[x e1]
               [loop [lambda[]
                       [let [[y e2]]
                       [if [< y x]
                           [begin y [loop]]
                           #t]]]]]
        loop]]]]]

(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
