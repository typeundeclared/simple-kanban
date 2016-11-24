#lang typed/racket
(require (for-syntax racket/syntax))

(: zip (All (a b) (-> (Listof a) (Listof b) (Listof (Pair a b)))))
(define (zip ListA ListB)
  (for/list : (Listof (Pair a b))
    ((x : a ListA)
     (y : b ListB))
    (cons x y)))

(: zip-to-hash (All (a b) (-> (Listof a) (Listof b) (HashTable a b))))
(define (zip-to-hash ListA ListB)
  (make-immutable-hash (zip ListA ListB)))

(define-syntax (define-enum-type stx)
  (syntax-case stx ()
    [(_ name (val ...))
     (with-syntax ([range-name (format-id stx "~a-~a" #'name "range")]
                   [to-int-name (format-id stx "~a->~a" #'name "Value")]
                   [to-enum-name (format-id stx "~a->~a" "Value" #'name)])
       #'(begin
           (define range-name (list val ...))
           (define-type name (U val ...))
           (: to-int-name (-> name Integer))
           (define (to-int-name state)
             (hash-ref (zip-to-hash range-name
                                    (sequence->list (in-range (length range-name))))
                       state))
           (: to-enum-name (-> Integer name))
           (define (to-enum-name value)
             (hash-ref (ann (zip-to-hash (sequence->list (in-range (length range-name)))
                                    range-name) (HashTable Integer name))
                       value))
           ))]))

(define-enum-type State ('backlog 'open 'working 'closed 'archived))
