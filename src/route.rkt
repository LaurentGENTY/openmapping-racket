#lang racket

(provide find_path distance)

#| ----- Route : calcule a route (path) between two nodes given by their id ----- |#

#| ----- Requirement ----- |#
(require "gps.rkt")
(require "graph.rkt")
;;(require "main.rkt") ;; pour voir le "data-graph"


#| ----- Lists in order to find a path ----- |#
(define to_visit_list '()) ;; list de couple (id, distance_to_end)
(define already_visited '()) ;; list d'id
(define directions '()) ;; list de couple (id, id_precedent)

#| ----- add-all-neighbours ----- |#
  ;; @brief      : add all the neighbours of id to to_visit_list (closed list) with their distance to the end
  ;; @param g    : graph?
  ;; @param id1  : string?
  ;; @param end  : string?
(define (add-all-neighbours g id end)
  (letrec ((add-order (lambda (l)
                      (cond
                        ;; If the list is empty, we return the list full of nodes we have to visit
                        ((null? l) to_visit_list)
                        ;; If the current node wasnt already added (already_visited) and not in to_visit_list then add it
                        ((and (not (assoc (car l) to_visit_list))(not (member (car l) already_visited)))
                         (cons (cons (car l) (distance-m (get-graph g (car l)) (get-graph g end))) (add-order (cdr l))))
                        (else (add-order (cdr l)))))))
    ;; Affect to_visit_list to the list full of n-neighbours
    (set! to_visit_list (add-order (n-neighbour (get-graph g id))))
    ;; Affect already_visited with the id given
    (set! already_visited (cons id already_visited))))

#| ----- closest-node ----- |#
  ;; @brief      : find the id of the node which is the nearest of the end and remove it from the list
  ;; @return     : a, the closest node of to_visit_list
(define (closest-node)
  (if
   ;; If we have no nodes to visit
   (null? to_visit_list)
   "-1"
   (begin
     ;; Sort the list to_visit and affect it again
     (set! to_visit_list (sort to_visit_list <= #:key(lambda (x) (cdr x))))
     ;; a = id of the first node of the to_visit_list
     ;; affect the new to_visit_list to the cdr of to_visit_list (without a)
     (let ((a (caar to_visit_list)))
       (set! to_visit_list (cdr to_visit_list))
       a))))

#| ----- a-star ----- |#
  ;; @brief      : add all neighbour of id1 in stack, for each vertex(v) in open list : calculate the distance between v and id2.
  ;;               Take the smallest and repeat recursively with minimum v and id2
  ;; @param g    : graph?
  ;; @param id1  : string?
  ;; @param id2  : string?
  ;; @return     : boolean?
(define (a-star g id1 id2)
  ;; Add all nodes distances between id1 and id2 to visit
  (add-all-neighbours g id1 id2)
  ;; Retrieve the closest to_visit_list node (to_visit_list now doesnt have v anymore)
  (let ((v (closest-node)))
    (begin
      ;; We add a direction (a node to the path)
      (set! directions (cons (cons v id1) directions))
      (cond
        ;; If we find the end node (so if it's v)
        [(equal? v id2) #t]
        ;; If the end has higher cost : no path
        [(equal? v "-1") #f]
        ;; Repeat recursively but with v, now trying to find a path between v and id2
        [else (a-star g v id2)]))))

#| ----- a-star ----- |#
  ;; @brief      : add all neighbour of id1 in stack, for each vertex(v) in open list : calculate the distance between v and id2.
  ;;               Take the smallest and repeat recursively with minimum v and id2
  ;; @param g    : graph?
  ;; @param id1  : string?
  ;; @param id2  : string?
  ;; @return     : string?
;; reconstruction of the route from the end
(define (path start end)
  (if (equal? start end)
      '()
      (let ((previous (cdr (assoc end directions))))
        (cons previous (path start previous)))))

#| ----- init-lists ----- |#
  ;; @brief       : init with empty list all needed lists
(define (init-lists )
  (begin
    (set! to_visit_list '())
    (set! already_visited '())
    (set! directions '())))

#| ----- find_path ----- |#
  ;; @brief       : try to find a path between start and end with a* algo
  ;; @param graph : graph?
  ;; @param id1   : string?
  ;; @param id2   : string?
  ;; @return      : list of string?
(define (find_path graph start end)
  (begin
    ;; Re-init all lists
    (init-lists)
    ;; Try to find a path
    (if
     (a-star graph end start)
     ;; We find a path : return a list of node with the path between start and end
     (cons start (path end start))
     ;; We cannot reach the end from the start
     "no way to do this")))

#| ----- TESTS ----- |#
;;(find_path data-graph "3924371555" "345772340") ;; -> '("345772340" "345772924" "345773754" "3924371761" "3924371555")
;;(find_path data-graph "3924371770" "345772340") ;; -> '("345772340" "345772924" "345772925" "283653048" "3924371759" "3924371764" "3924371770")

;; =====================================================================================
;; calculate the total distance between two point using a_star

#| ----- distance ----- |#
  ;; @brief       : compute the distance between 2 nodes in a graph using a_star
  ;; @param g     : graph?
  ;; @param start : string? id of the start node
  ;; @param end   : string? id of the end node
  ;; @return      : number? total distance in meters between 2 nodes
(define (distance g start end)
  (letrec ([road (find_path g start end)]
           [auxiliary (lambda (l)
                        (match l
                          ((cons r1 (cons r2 r3)) (+ (distance-m (get-graph g r1) (get-graph g r2)) (auxiliary (cdr l))))
                          (else 0)))])
    (auxiliary road)))
;;(distance data-graph "3924371555" "345772340") ;; -> 310.7 m
