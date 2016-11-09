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
   title program (map-state-to-integer 'open) (date->julian/scalinger (current-date))))

(define (kanban-update-task-state! a-kanban id new-state)
  (print id)
  (print new-state)
  (query-exec
   (kanban-db a-kanban)
   "UPDATE tasks SET state=? WHERE id=?"
   (map-state-to-integer new-state) id))

(define possible-states '(open working closed))
(define (next-state state)
  (case state
    [(open) 'working]
    [(working) 'closed]
    [(closed) 'closed]))
(define (previous-state state)
  (case state
    [(open) 'open]
    [(working) 'open]
    [(closed) 'working]))
(define (map-state-to-integer state)
  (case state
    [(open) 0]
    [(working) 1]
    [(closed) 2]))
(define (map-integer-to-state integer)
  (case integer
    [(0) 'open]
    [(1) 'working]
    [(2) 'closed]))
  
(struct task (id title program state) #:mutable)

(define (kanban-programs a-kanban)
    (for/list ([p (in-query (kanban-db a-kanban)
                "select program from tasks group by program order by program")])
      p))

(define (kanban-tasks a-kanban state)
  (define results
    (query-rows (kanban-db a-kanban)
                "select id, title, program, state, start_date from tasks where state=? order by state, program, start_date"
                (map-state-to-integer state)))
  (map (lambda (x)
         (task (vector-ref x 0) (vector-ref x 1) (vector-ref x 2) (map-integer-to-state (vector-ref x 3))))
       results))