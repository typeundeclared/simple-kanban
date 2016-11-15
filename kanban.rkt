#lang racket
(require db)
(require racket/date)
(provide kanban-initialize!
         kanban-insert-task!
         kanban-update-task-state!
         kanban-tasks
         kanban-programs
         ;helpers
         possible-states
         primary-states
         next-state
         previous-state
         map-state-to-integer
         map-integer-to-state
         (struct-out task)
         (struct-out kanban))

; initialize-tasklist! : path? -> tasks?
; Sets up a task database (if it doesn't exist)
(struct kanban (db))
(define (kanban-initialize! fpath)
  (define db (sqlite3-connect #:database fpath #:mode 'create))
  (define the-kanban (kanban db))
  (unless (table-exists? db "tasks")
    (query-exec db
                (string-append
                 "CREATE TABLE tasks "
                 "(id INTEGER PRIMARY KEY, title TEXT, program TEXT, state INTEGER, start_date INTEGER, end_date INTEGER DEFAULT 0)")))
  the-kanban)

(define (kanban-insert-task! a-kanban title program)
  (query-exec
   (kanban-db a-kanban)
   "INSERT INTO tasks (title, program, state, start_date) VALUES (?, ?, ?, ?)"
   title program (map-state-to-integer 'open) (current-seconds)))

(define (kanban-update-task-state! a-kanban id new-state)
  (print id)
  (print new-state)
  (query-exec
   (kanban-db a-kanban)
   "UPDATE tasks SET state=? WHERE id=?"
   (map-state-to-integer new-state) id)
  (when (eq? new-state 'closed)
      (query-exec
       (kanban-db a-kanban)
       "UPDATE tasks set end_date=? where id=?"
       (current-seconds) id)))

(define possible-states '(backlog open working closed archived))
(define primary-states '(open working closed))
(define (next-state state)
  (case state
    [(backlog) 'open]
    [(open) 'working]
    [(working) 'closed]
    [(closed) 'archived]
    [(archived) 'archived]))
(define (previous-state state)
  (case state
    [(backlog) 'backlog]
    [(open) 'backlog]
    [(working) 'open]
    [(closed) 'working]
    [(archived) 'closed]))
(define (map-state-to-integer state)
  (case state
    [(backlog) 0]
    [(open) 1]
    [(working) 2]
    [(closed) 3]
    [(archived) 4]))
(define (map-integer-to-state integer)
  (case integer
    [(0) 'backlog]
    [(1) 'open]
    [(2) 'working]
    [(3) 'closed]
    [(4) 'archived]))
  
(struct task (id title program state start-date end-date) #:mutable)

(define (kanban-programs a-kanban)
  (for/list ([p (in-query (kanban-db a-kanban)
                          "select program from tasks group by program order by program")])
    p))

(define kanban-query-all-tasks
  "select id, title, program, state, start_date, end_date from tasks order by state, program, start_date")
(define kanban-query-tasks-by-state
  "select id, title, program, state, start_date, end_date from tasks where state=? order by state, program, start_date")
(define (kanban-tasks a-kanban [state "ANY"])
  (define results
    (if (eq? state "ANY")
        (in-query (kanban-db a-kanban) kanban-query-all-tasks)
        (in-query (kanban-db a-kanban) kanban-query-tasks-by-state (map-state-to-integer state))))
  (for/list ([(id title program state start end) results])
    (task id title program (map-integer-to-state state) start end)))