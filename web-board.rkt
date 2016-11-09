#lang web-server
(require web-server/formlets)
(require web-server/dispatch)
(require web-server/servlet web-server/servlet-env)
(require "kanban.rkt")

;;;;;;;;;;;;;;;;;;;;;

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
        ,{input-string . => . program})
   (values title program)))

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
                                (map-state-to-integer new-state))]))
               (input ([class "statebutton"] [type "submit"] [value ,sym]))))))
    
(define (render-task a-task program-color-list)
  `(li (div ((class "task"))
            ,(render-task-button a-task 'previous)
            ,(task-title a-task)
            ,(render-task-button a-task 'next)
            (br)
            (div ((style ,(string-join (list "color:" (second (assoc (task-program a-task) program-color-list))))))
                 ,(task-program a-task)))))

(define (render-tasks a-task-list program-color-list)
  `(div ((class "tasks"))
        (ul
         ,@(map (λ (x) (render-task x program-color-list)) a-task-list))))

(define (render-task-column a-kanban state)
  `(td
    ,(render-tasks (kanban-tasks a-kanban state)
                   (program-colors (kanban-programs a-kanban)))))

(define (render-tasks-page a-kanban request)
  (response/xexpr
   `(html (head (title "My Task List"))
          (body (h1 "My Tasks")
                (link ((rel "stylesheet")
                       (href "/kanban.css")
                       (type "text/css")))
                ;(a ((href ,(task-url list-programs))) "Programs")
                (table
                 (tr
                  ,@(map (λ (x) `(th ,(state-to-string x))) possible-states))
                 (tr
                  ,@(map (λ (x) (render-task-column a-kanban x)) possible-states)))
                (form ([action ,(task-url submit-task)])
                      ,@(formlet-display new-task-formlet)
                      (input ([type "submit"])))))))

(define (handle-task-submission a-kanban request)
  (define-values (title program)
    (formlet-process new-task-formlet request))
  (kanban-insert-task! a-kanban title program)
  (redirect-to (task-url list-tasks)))

(define (handle-task-state-change a-kanban request)
  (define bindings (request-bindings request))
  (kanban-update-task-state! a-kanban
                             (string->number (extract-binding/single 'id bindings))
                             (map-integer-to-state (string->number (extract-binding/single 'new-state bindings))))
  (redirect-to (task-url list-tasks)))

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

;(define (list-programs request)
;  (render-programs-page tasks request))

(define-values (task-dispatch task-url)
  (dispatch-rules
   [("") list-tasks]
   [("new-task") submit-task]
   [("update-task") change-task-state]
   ;    [("programs") list-programs]
   ;[("programs" (string-arg)) list-tasks-on-program]
   ;[("archive" (integer-arg) (integer-arg)) review-archive]
   ))

(define (start request)
  (task-dispatch request))

(serve/servlet start
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:extra-files-paths (list "./")
               #:port 8181)