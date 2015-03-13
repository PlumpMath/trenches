#lang racket

(provide read-xyz)

;; (: read-xyz (-> (Listof (Vector Number Number Number))))
(define (read-xyz)
  (let ([line (read-line (current-input-port) 'any)])
    (cond [(not (eof-object? line))
           (let ([xyz (map string->number
                           (regexp-split #px" " line))])
             (cond [(equal? (length xyz) 3)
                    (cons (list->vector xyz)
                          (read-xyz))]
                   [else empty]))]
          [else empty])))

;; (: distance (-> (Vector Number Number Number) (Vector Number Number Number) Number))
(define (distance p1 p2)
  (sqrt (+ (sqr (- (vector-ref p1 0)
                   (vector-ref p2 0)))
           (sqr (- (vector-ref p1 1)
                   (vector-ref p2 1)))
           (sqr (- (vector-ref p1 2)
                   (vector-ref p2 2))))))

;; (: make-distance<? (-> (Vector Number Number Number) (Vector Number Number Number) (Vector Number Number Number) Number))

