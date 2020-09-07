#lang racket
(require racket/trace)
(require "graph.rkt")
(require "gps.rkt")
(require "graph_construction.rkt")
(require "Dijkstra.rkt")
(require xml)
(define data (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/map2.osm")))))

#| ----- nearest-neighbour ----- |#
  ;; @brief   : given a graph and a list of city to visit return a path between the different cities following the algorithm of nearest cities
  ;; @return  : list of cities
(define (nearest-neighbour graph cities)
  (if (null? cities )
      (print "No way")
      (append (trip-nearest graph (car cities) cities) (list (car cities)))))

#| ----- trip-nearest ----- |#
  ;; @brief   : given a start point and cities compute recusively the nearest way 
  ;; @return  : list of cities
(define (trip-nearest graph start cities)
  (letrec ([aux
           (lambda (start cities visited)
           (cond
             [(null? visited) (aux start (cdr cities) (cons start visited))]
             [(null? (cdr cities)) (reverse (cons (car cities) visited))]
             [else (let ([l (sort-Dijkstra graph start cities )])  (aux l (remv l cities) (cons l visited)))]
             ))])
    (aux start cities '())))

#| ----- sort-Dijkstra ----- |#
  ;; @brief   : call Dijkstra function with a list of association and return the nearest city to start city 
  ;; @return  : the id of the nearest city
(define (sort-Dijkstra graph start list)
  (let ([l (list-assoc graph)])
    (cdr (mini graph start l list (Dijkstra graph start l)))))

#| ----- mini ----- |#
  ;; @brief   : return the nearest city to the city start given the computed distance vector 
  ;; @return  : the id of the nearest city
(define (mini graph start l-assoc cities dist)
  (letrec([aux
           (lambda(min id l-assoc cities dist)
             (cond
               [(null? cities) id]
               [(< min (car(cdr(vector-ref dist (get-index l-assoc (car cities)))))) (aux min id l-assoc (cdr cities) dist)]
               [else (aux (car (vector-ref dist (get-index l-assoc (car cities)))) (car cities) l-assoc (cdr cities) dist)]))])
    (aux (vector-ref dist (get-index l-assoc (car cities))) (car cities) l-assoc (cdr cities) dist)))

;(define (nearest-insertion graph)
;  )

;(define (closest-cities graph cities)
 ; ( letrec([aux
  ;          (lambda (graph cities min))
   ;;         (cond
     ;         [(null? cities) min]
      ;        [(< (mini Dijkstra (car cities)) (car min)) ])
       ;     ])
        ;(aux graph cities (list +inf.0 ))))


(define data-graph (osm-to-full-sorted data))
(way-number data-graph)
(nearest-neighbour data-graph '("3924371555" "345772925" "283653048" "3924371764" "345772340"))


