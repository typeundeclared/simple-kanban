#lang typed/racket
(require typed/db)
(require typed/racket/date)
(require "kanban-types.rkt")
(provide kanban-initialize!
         kanban-insert-task!
         kanban-update-task-state!
         kanban-update-task-nwa!
         kanban-query-task
         kanban-query-all-tasks
         kanban-query-tasks-by-state
         kanban-programs)

(: kanban-initialize! (-> Path-String kanban))
(define (kanban-initialize! fpath)
  (define the-kanban (kanban (sqlite3-connect #:database fpath #:mode 'create)))
  (unless (table-exists? (kanban-db the-kanban) "tasks")
    (query-exec (kanban-db the-kanban)
                (string-append
                 "CREATE TABLE tasks "
                 "(id INTEGER PRIMARY KEY, title TEXT, program TEXT, nwa TEXT, state INTEGER, start_date INTEGER, end_date INTEGER DEFAULT 0)")))
  the-kanban)

(: kanban-insert-task! (-> kanban String String String Void))
(define (kanban-insert-task! a-kanban title program [nwa ""])
  (query-exec
   (kanban-db a-kanban)
   "INSERT INTO tasks (title, program, nwa, state, start_date) VALUES (?, ?, ?, ?, ?)"
   title program nwa (State->Value 'open) (current-seconds)))

(: kanban-update-task-state! (-> kanban Integer State Void))
(define (kanban-update-task-state! a-kanban id new-state)
  (query-exec
   (kanban-db a-kanban)
   "UPDATE tasks SET state=? WHERE id=?"
   (State->Value new-state) id)
  (when (eq? new-state 'closed)
    (query-exec
     (kanban-db a-kanban)
     "UPDATE tasks set end_date=? where id=?"
     (current-seconds) id)))

(: kanban-update-task-nwa! (-> kanban Integer String Void))
(define (kanban-update-task-nwa! a-kanban id new-nwa)
  (query-exec
   (kanban-db a-kanban)
   "UPDATE tasks set nwa=? where id=?"
   new-nwa id))

(: kanban-programs (-> kanban (Listof String)))
(define (kanban-programs a-kanban)
  (filter (λ (x) (string? x))
          (for/list : (Listof SQL-Datum)
            ([p : (Vectorof Any) (query-rows (kanban-db a-kanban)
                                             "select program from tasks group by program order by program")])
            (vector-ref p 0))))

(: process-tasks (-> (Listof (Vectorof SQL-Datum)) (Listof task)))
(define (process-tasks db-tasks)
  (for/list : (Listof task)
    ([p : (Vectorof Any) db-tasks])
    (match p
      [(vector id title program nwa state start end)
       (if (and (exact-integer? id)
                (string? title)
                (string? program)
                (string? nwa)
                (exact-integer? state)
                (exact-integer? start)
                (exact-integer? end))
           (task id title program nwa (Value->State state) (seconds->date start) (seconds->date end))
           (error "bad types"))])))

(: kanban-query-task (-> kanban Integer task))
(define (kanban-query-task a-kanban id)
  (first
   (process-tasks
   (query-rows (kanban-db a-kanban)
               "select id, title, program, nwa, state, start_date, end_date from tasks where id=?" id))))

(: kanban-query-all-tasks (-> kanban (Listof task)))
(define (kanban-query-all-tasks a-kanban)
  (process-tasks
   (query-rows (kanban-db a-kanban)
               "select id, title, program, nwa, state, start_date, end_date from tasks order by state, program, start_date")))

(: kanban-query-tasks-by-state (-> kanban State (Listof task)))
(define (kanban-query-tasks-by-state a-kanban state)
  (process-tasks
   (query-rows (kanban-db a-kanban)
               "select id, title, program, nwa, state, start_date, end_date from tasks where state=? order by state, program, start_date"
               (State->Value state))))  