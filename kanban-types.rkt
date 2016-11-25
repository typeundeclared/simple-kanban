#lang typed/racket
(require typed/racket/date
         typed/db)
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

(struct task ([id : Integer]
              [title : String]
              [program : String]
              [state : State]
              [start-date : date]
              [end-date : date])
  #:mutable
  #:transparent)

(struct kanban ([db : Connection]))

(: kanban-initialize! (-> Path-String kanban))
(define (kanban-initialize! fpath)
  (define the-kanban (kanban (sqlite3-connect #:database fpath #:mode 'create)))
  (unless (table-exists? (kanban-db the-kanban) "tasks")
    (query-exec (kanban-db the-kanban)
                (string-append
                 "CREATE TABLE tasks "
                 "(id INTEGER PRIMARY KEY, title TEXT, program TEXT, state INTEGER, start_date INTEGER, end_date INTEGER DEFAULT 0)")))
  the-kanban)

(: kanban-programs (-> kanban (Listof String)))
(define (kanban-programs a-kanban)
  (filter (λ (x) (string? x))
          (for/list : (Listof SQL-Datum)
            ([p : (Vectorof Any) (query-rows (kanban-db a-kanban)
                                  "select program from tasks group by program order by program")])
            (vector-ref p 0))))

(define kanban-query-all-tasks
  "select id, title, program, state, start_date, end_date from tasks order by state, program, start_date")
(define kanban-query-tasks-by-state
  "select id, title, program, state, start_date, end_date from tasks where state=? order by state, program, start_date")
;(define (kanban-tasks a-kanban [state "ANY"])
;  (define results
;    (if (eq? state "ANY")
;        (in-query (kanban-db a-kanban) kanban-query-all-tasks)
;        (in-query (kanban-db a-kanban) kanban-query-tasks-by-state (map-state-to-integer state))))
;  (for/list ([(id title program state start end) results])
;    (task id title program (map-integer-to-state state) start end)))

(: kanban-tasks (-> kanban Statement (Listof task)))
(define (query-kanban-tasks a-kanban query)
  (for/list : (Listof task)
    ([p : (Vectorof Any)
        (query-rows (kanban-db a-kanban) query)])
    (match p
      [(vector id title program state start end)
       (if (and (exact-integer? id)
                (string? title)
                (string? program)
                (exact-integer? state)
                (exact-integer? start)
                (exact-integer? end))
           (task id title program (Value->State state) (seconds->date start) (seconds->date end))
           (error "bad types"))])))

