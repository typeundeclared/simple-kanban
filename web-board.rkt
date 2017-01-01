#lang web-server
(require racket/date)
(require web-server/formlets)
(require web-server/dispatch)
(require web-server/servlet
         web-server/servlet-env
	 web-server/configuration/responders)
(require "kanban.rkt" "kanban-types.rkt")


;;;;;;;;;;;;;;;;;;;;;

(define table-states '(open working closed))

(define (state-to-string state)
  (case state
    [(open) "OPEN"]
    [(working) "IN PROGRESS"]
    [(closed) "DONE"]))

(define color-list
  '("red" "blue" "green" "black" "brown"
          "cyan" "olive" "orangered" "seagreen" "teal"
          "yellow" "steelblue" "plum" "khaki" "bisque"))

(define (program-colors program-list)
  (map (λ (x y) (list x y))
       program-list
       (take color-list (length program-list))))

(define new-task-formlet
  (formlet
   (#%# ,{input-string . => . title}
        ,{input-string . => . program}
        ,{input-string . => . nwa})
   (values title program nwa)))

(define (render-task-button a-task dir)
  (let-values ([(sym dir-func) (if (eq? dir 'previous)
                                   (values "<" previous-state)
                                   (values ">" next-state))])
    (define new-state (dir-func (task-state a-task)))
    (if (eq? new-state (task-state a-task))
        sym
        `(form ([action ,(task-url change-task-state)])
               (input ([type "hidden"]
                       [name "id"]
                       [value ,(number->string (task-id a-task))]))
               (input ([type "hidden"]
                       [name "new-state"]
                       [value ,(number->string
                                (State->Value new-state))]))
               (input ([class "statebutton"] [type "submit"] [value ,sym]))))))
    
(define (render-task a-task program-color-list)
  `(li (div ((class "task"))
            ,(render-task-button a-task 'previous)
            (a ((href ,(task-url task-details (number->string (task-id a-task))))) ,(task-title a-task))
            ,(render-task-button a-task 'next)
            (br)
            (div ((style ,(string-join (list "color:" (second (assoc (task-program a-task) program-color-list))))))
                 ,(task-program a-task))
            ,(if (eq? 'archived (task-state a-task))
                 (date->string (task-end-date a-task))
                 "")
            )))

(define (render-tasks a-task-list program-color-list)
  `(div ((class "tasks"))
        (ul
         ,@(map (λ (x) (render-task x program-color-list)) a-task-list))))

(define (render-task-column a-kanban state program-colors-assoc)
  `(td
    ,(render-tasks (kanban-query-tasks-by-state a-kanban state) program-colors-assoc)))

(define (render-tasks-page a-kanban request)
  (define program-colors-assoc (program-colors (kanban-programs a-kanban)))
  (response/xexpr
   `(html (head (title "My Task List"))
          (body (h1 "My Tasks")
                (link ((rel "stylesheet")
                       (href "/todo/kanban.css")
                       (type "text/css")))
                ;(a ((href ,(task-url list-programs))) "Programs")
                (table
                 (tr
                  ,@(map (λ (x) `(th ,(string-titlecase (symbol->string x)))) table-states))
                 (tr
                  ,@(map (λ (x) (render-task-column a-kanban x program-colors-assoc)) table-states)))
                (form ([action ,(task-url submit-task)])
                      ,@(formlet-display new-task-formlet)
                      (input ([type "submit"])))
                (h2 "Backlog Tasks")
                (ul ,@(map (λ (x) (render-task x program-colors-assoc))
                           (kanban-query-tasks-by-state a-kanban 'backlog)))
                (h2 "Archived")
                (ul ,@(map (λ (x) (render-task x program-colors-assoc))
                           (kanban-query-tasks-by-state a-kanban 'archived)))
                ))))

(define (render-task-details global-kanban id request)
  (define tsk (kanban-query-task global-kanban (string->number id)))
  (response/xexpr
   `(html (head (title "Task" ,(task-title tsk)))
          (body
           (link ((rel "stylesheet")
                  (href "/kanban.css")
                  (type "text/css")))
           (table
            (tr (td "Title:") (td ,(task-title tsk)))
            (tr (td "Program:") (td ,(task-program tsk)))
            (tr (td "NWA:") (td
                             (form ([action ,(task-url change-task-nwa
                                                       (number->string (task-id tsk)))])
                                   (input ([type "text"]
                                           [name "nwa"]
                                           [value ,(task-nwa tsk)]))
                                   (input ([class "statebutton"] [type "submit"] [value "Update"])))))
            (tr (td "State:") (td ,(string-titlecase (symbol->string (task-state tsk)))))
            (tr (td "Created:") (td ,(date->string (task-start-date tsk))))
            (tr (td "Completed:")
                (td ,(if (<= 2000 (date-year (task-end-date tsk)))
                         (date->string (task-end-date tsk))
                         ""))))))))

(define (handle-task-submission a-kanban request)
  (define-values (title program nwa)
    (formlet-process new-task-formlet request))
  (kanban-insert-task! a-kanban title program nwa)
  (redirect-to (task-url list-tasks)))

(define (handle-task-state-change a-kanban request)
  (define bindings (request-bindings request))
  (kanban-update-task-state! a-kanban
                             (string->number (extract-binding/single 'id bindings))
                             (Value->State (string->number (extract-binding/single 'new-state bindings))))
  (redirect-to (task-url list-tasks)))

(define (handle-task-nwa-change a-kanban id request)
  (define bindings (request-bindings request))
  (kanban-update-task-nwa! a-kanban
                           (string->number id)
                           (extract-binding/single 'nwa bindings))
  (redirect-to (task-url task-details id)))

;(define (render-programs-page a-task-list request)
;  (define (response-generator embed/url)
;    (response/xexpr
;     `(html (head (title "Programs"))
;            (body (h1 "All Programs")
;                  (ul
;                   ,@(map (lambda (x) `(li ,(task-program x))) a-task-list))))))
;  (send/suspend/dispatch response-generator))


;;;;;;;;;;;;;;;;;;;;;

(define global-kanban (kanban-initialize! "./tasks.db"))
(define (list-tasks request)
  (render-tasks-page global-kanban request))
(define (submit-task request)
  (handle-task-submission global-kanban request))
(define (change-task-state request)
  (handle-task-state-change global-kanban request))
(define (task-details request id)
  (render-task-details global-kanban id request))
(define (change-task-nwa request id)
  (handle-task-nwa-change global-kanban id request))

(define-values (task-dispatch task-url)
  (dispatch-rules
   [("todo") list-tasks]
   [("todo" "new-task") submit-task]
   [("todo" "update-task") change-task-state]
   [("todo" "task" (string-arg)) task-details]
   [("todo" "update-nwa" (string-arg)) change-task-nwa]
   [("todo" "kanban.css") (λ (_) (file-response 200 #"OK" "kanban.css"))]
   ;    [("programs") list-programs]
   ;[("todo" "programs" (string-arg)) list-tasks-on-program]
   ;[("todo" "archive" (integer-arg) (integer-arg)) review-archive]
   ))

(define (start request)
  (task-dispatch request))

(serve/servlet start
	       #:command-line? #t
               #:servlet-path "/todo"
               #:servlet-regexp #rx""
	       #:server-root-path (current-directory)
	       #:extra-files-paths (list (current-directory))
               #:port 8181)
