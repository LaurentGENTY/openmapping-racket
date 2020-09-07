#lang racket
(require racket/trace)
(require "graph.rkt")
(require "gps.rkt")
;(require "main.rkt")
(require "graph_construction.rkt")
(require xml)
(provide Dijkstra find-my-way list-assoc get-index find-a-way Dijkstra-nearest)


#| ----- Dijkstra ----- |#
  ;; @brief   : Dijkstra algorihtm
  ;; @return  : list

(define (Dijkstra graph id l)
  (Dijkstra-rec graph id l))

#| ----- Dijkstra-rec ----- |#
  ;; @brief   : recursive algorithm used by Dijkstra
  ;; @return  : list
(define (Dijkstra-rec graph id l)
  (letrec([aux
           (lambda (p graph vect-dist vect-visited l)
             (cond
               [(null? p) vect-dist]
               [else (let ([elem (mini-stack p l vect-dist)]) (aux (Dij-push p elem vect-visited l graph) graph
                          (update-vect-dist elem graph (n-neighbour (get-graph graph elem)) vect-dist l)
                          (and(vector-set! vect-visited (get-index l elem) 1) vect-visited) l))]))])
    (aux (list id) graph (initialize-vector (length (list-of-key graph)) id l) (make-vector (length (list-of-key graph)) 0) l)))

(define (mini-stack p l vect-dist)
  (letrec ([aux
            (lambda( mi rem p vect-dist)
            (cond
              [(null? p) rem]
              [(< (car(vector-ref vect-dist (get-index l (head p)))) mi) (aux (car(vector-ref vect-dist (get-index l (head p)))) (head p) (pop p) vect-dist)]
              [else (aux mi rem (cdr p) vect-dist)]))])
   ; (trace aux)
    (aux (car (vector-ref vect-dist (get-index l (head p)))) (head p) p vect-dist)))
#| ----- Dijkstra-push ----- |#
  ;; @brief   : Push the given id in the satck if he is not already definetely evaluated
  ;; @return  : stack
(define (Dij-push p elem vect-visited l graph)
  (if (equal? (vector-ref vect-visited (get-index l elem)) 1)
      (remove elem p)
      (push-neighbour elem (remove elem p) vect-visited l graph)))

#| ----- push-neighbour ----- |#
  ;; @brief   : push the neighbour that have to be push in the stack
  ;; @return  : stack
(define (push-neighbour id p vect-visited l-assoc graph)
  (letrec([aux
           (lambda (p l id)
             (cond
               [(null? l) p]
               [(equal? 1 (vector-ref vect-visited (get-index l-assoc (car l)))) (aux p (cdr l) id)]
               [else (aux (push (car l) p) (cdr l) id)]))])
    (aux  p (n-neighbour(get-graph graph id)) id)))  

#| ----- modif-vect ----- |#
  ;; @brief   : Change the value of the vector in the givan key 
  ;; @return  : return the changed vector
(define (modif-vect vect id)
  (vector-set! vect id 1) vect)

#| ----- push ----- |#
  ;; @brief   : push the given x in a stack
  ;; @return  : a stack
(define (push x stack)
  (if (list? x)
      (append x stack)
      (cons x stack)))

#| ----- pop ----- |#
  ;; @brief   : remove the first element of the stack
  ;; @return  : a stack
(define (pop stack)
  (cdr stack))

#| ----- head ----- |#
  ;; @brief   : return the head of the stack
  ;; @return  : a stack
(define (head stack)
  (car stack))

#| ----- initialize-vector ----- |#
  ;; @brief   : return a vector initialize with +inf everywhere except on the the positio id 
  ;; @return  : vector
(define (initialize-vector size id l)
  (let ([v (make-vector size '(+inf.0))])
    (vector-set! v (get-index l id) '(0))
    v))

#| ----- list-assoc ----- |#
  ;; @brief   : return the list of association of the given graph
  ;; @return  : list
(define (list-assoc graph)
  (letrec([aux
            (lambda (l1 l2 n)
              (if (null? l1)
                  l2
                  (aux (cdr l1) (cons (list (car l1) n) l2) (add1 n))
              ))])
    (aux (list-of-key graph) '() 0 )))

#| ----- get-index ----- |#
  ;; @brief   : Given the list of association return the index in the vector of the id
  ;; @return  : number
(define (get-index l id)
  (car(cdr(assoc id l))))

#| ----- update-vect-dist ----- |#
  ;; @brief   : update the distance of the neighbour of a node
  ;; @return  : vector
(define (update-vect-dist id graph neighbour vect-dist l)
  (letrec([aux
           (lambda (dist neighbour vect-dist graph id l)
             (cond
               [(null? neighbour) vect-dist]
               [(< (+ dist (distance-m (get-graph graph (car neighbour)) (get-graph graph id)))
                   (car (vector-ref vect-dist (get-index l (car neighbour)))))
                (aux dist
                     (cdr neighbour)
                     (and (vector-set! vect-dist (get-index l (car neighbour)) (list (+ dist
                                                                     (distance-m (get-graph graph (car neighbour))
                                                                                 (get-graph graph id))) id))vect-dist)
                     graph id l) ]
               [else (aux dist (cdr neighbour) vect-dist graph id l)]))])
    (aux (car(vector-ref vect-dist (get-index l id))) neighbour vect-dist graph id l)))

#| ----- find-my-way ----- |#
  ;; @brief   : given a graph, a first element and a last element compute the way to go from start to end
  ;; @return  : list
(define (find-my-way graph start end)
  (let* ([l (list-assoc graph)])
    (find-a-way end l (Dijkstra graph start l))))

#| ----- find-a-way ----- |#
  ;; @brief   : Find a way thanks to a vect-dist vector
  ;; @return  : list
(define (find-a-way i lis vect-dist)
  (letrec ([aux
            (lambda (i lis vect-dist l)
              (cond
                [(null? (cdr (vector-ref vect-dist (get-index lis i)))) (cons i l)]
                [else (aux (car (cdr (vector-ref vect-dist (get-index lis i)))) lis vect-dist (cons i l))]
            ))])
    (aux i lis vect-dist '())))

#| ----- Dijkstra-nearest ----- |#
;Special Dijkstra used by nearest-neighbour (vect-visited is given) 
(define (Dijkstra-nearest graph id l vect-visited)
  (Dijkstra-nearest-rec graph id l vect-visited))

#| ----- Dijkstra-nearest-rec ----- |#
;Special Dijkstra-rec used by nearest-neighbour (vect-visited is given) 
(define (Dijkstra-nearest-rec graph id l vect-visited)
  (letrec([aux
           (lambda (p graph vect-dist vect-visited l)
             (cond
               [(null? p) vect-dist]
               [else (let ([elem (mini-stack p l vect-dist)]) (aux (Dij-push p elem vect-visited l graph) graph
                          (update-vect-dist elem graph (n-neighbour (get-graph graph elem)) vect-dist l)
                          (and(vector-set! vect-visited (get-index l elem) 1) vect-visited) l))]))])
    (aux (list id) graph (initialize-vector (length (list-of-key graph)) id l) vect-visited l)))

