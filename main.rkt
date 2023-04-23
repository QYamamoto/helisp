#lang racket

(provide (all-defined-out))
(define (+print array)
    (displayln (string-join (map number->string (vector->list array)) " | ")))

(define (sprint array)
    (displayln (list->string (map integer->char (vector->list array)))))

(define (array-ref array indexes)
    (vector-map (compose (curry vector-ref array) sub1) indexes))

(define (array-set! array indexes value)
    (let ([value (vector-ref value 0)])
    (if (zero? (vector-ref indexes 0))
        (vector-fill! array value)
    (for ([i indexes])
        (vector-set! array (sub1 i) value)))))

(define (make-array length)
    (make-vector (vector-ref length 0) 0))

(define (increment array)
    (for ([i (vector-length array)])
        (vector-set! array i (add1 (vector-ref array i)))))

(define (mul left right)
    (let (
        [length-left (vector-length left)]
        [length-right (vector-length right)])
    (do (
        [i 0 (add1 i)]
        [acc 0 (+ acc (* (vector-ref left i) (vector-ref right i)))])
        ((or (= i length-left) (= i length-right))
            (vector acc)))))

(define (add left right)
    (let (
        [length-left (vector-length left)]
        [length-right (vector-length right)])
    (cond
    [(= length-left 1)
        (vector-map (curry + (vector-ref left 0)) right)]
    [(= length-right 1)
        (vector-map (curry + (vector-ref right 0)) left)]
    [(= length-left length-right)
        (vector-map + left right)])))

(define (sub left right)
    (let (
        [length-left (vector-length left)]
        [length-right (vector-length right)])
    (cond
    [(= length-right 1)
        (vector-map (lambda (i) (- i (vector-ref right 0))) left)]
    [(= length-left length-right)
        (vector-map - left right)])))

(define (cyberspaces)
    (displayln "// Getting your location...
// Your location is UNITED STATES.
// Congratulations! You are in the Cyber Spaces!"))