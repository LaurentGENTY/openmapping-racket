#lang racket
(require racket/trace)
(require "graph.rkt")
(require "gps.rkt")
(require "route.rkt")
(require "graph_construction.rkt")
(require "Dijkstra.rkt")
(require xml)

(provide nearest)

#| ----- nearest ----- |#
  ;; @brief   : given a graph and a list of city to visit return a path between the different cities following the algorithm of nearest cities
  ;; @return  : list of cities
(define (nearest graph cities)
  (if (null? cities )
      '()
      (trip-nearest graph (car cities) (cdr cities))))

#| ----- trip-nearest ----- |#
  ;; @brief   : given a start point and cities compute recusively the nearest way 
  ;; @return  : list of cities
(define (trip-nearest graph start cities)
  (letrec ([aux
           (lambda (start cities visited start-rem vect-visited)
           (cond
             [(null? cities) (let ([l (sort-Dijkstra graph start (list start-rem) vect-visited)]) (append l visited))]
             [else (auxiliary-function-nearest graph start cities vect-visited visited start-rem aux)]))])
    (aux start cities (list start) start (make-vector (length (list-of-key graph)) 0))))

#| ----- auxiliary-function-nearest ----- |#
  ;; @brief   : Function that permit to avoid two computation of Dijkstra algorithm
  ;; @return  : list of cities
(define (auxiliary-function-nearest graph start cities vect-visited visited start-rem aux)
  (let* ([l (sort-Dijkstra graph start cities vect-visited)][u-vect (update-visited-call graph vect-visited (cons start l))])
               (if (null? l)
                   '()
                   (aux (car l) (remv (car l) cities) (append l visited) start-rem u-vect)
             )))

#| ----- update-visited-call ----- |#
  ;; @brief   : update vect-visited by putting the new cities given in parameter to 1
  ;; @return  : updated vector
(define (update-visited-call graph vect-visited l)
  (letrec ([aux
            (lambda (vect-visited l l-assoc)
              (cond
                [(null? (cdr l)) vect-visited]
                [else (and (vector-set! vect-visited (get-index l-assoc (car l) )1) (aux vect-visited (cdr l) l-assoc))]))])
    (aux vect-visited l (list-assoc graph))))
    
#| ----- find-path-nearest ----- |#
  ;; @brief   : update vect-visited by putting the new cities given in parameter to 1
  ;; @return  : updated vector
(define (find-path-nearest graph start end l-assoc vect-dist)
  (letrec ([aux
            (lambda(graph start end l-assoc vect-dist p)
              (cond
                [(equal? start end) (reverse p)]
                [else (aux graph start  (car (cdr (vector-ref vect-dist (get-index l-assoc end)))) l-assoc vect-dist (cons end p))]))])
    (aux graph start end l-assoc vect-dist '())))

#| ----- sort-Dijkstra ----- |#
  ;; @brief   : call Dijkstra function with a list of association and return the nearest city to start city 
  ;; @return  : the id of the nearest city
(define (sort-Dijkstra graph start list vect-visited)
  (let ([vect-visited-copy (make-vector (vector-length vect-visited) 0)]) 
  (vector-copy! vect-visited-copy 0 vect-visited)
  (let* ([l (list-assoc graph)][vect-dist (Dijkstra-nearest graph start l vect-visited-copy)][end (mini graph start l list vect-dist)])
    (if (equal? (car (vector-ref vect-dist (get-index l end))) +inf.0)
        '()
        (find-path-nearest graph start end l vect-dist)))))




#| ----- mini ----- |#
  ;; @brief   : return the nearest city to the city start given the computed distance vector 
  ;; @return  : the id of the nearest city
(define (mini graph start l-assoc cities dist)
  (letrec([aux
           (lambda(min id l-assoc cities dist)
             (cond
               [(null? cities) id]
               [(< min (car (vector-ref dist (get-index l-assoc (car cities))))) (aux min id l-assoc (cdr cities) dist)]
               [else (aux (car (vector-ref dist (get-index l-assoc (car cities)))) (car cities) l-assoc (cdr cities) dist)]))])
    (aux (car (vector-ref dist (get-index l-assoc (car cities)))) (car cities) l-assoc (cdr cities) dist)))


#| The rest of the code is the beginning of the implemntation of nearest-insertion |#

(define (nearest-insertion graph cities)
  (let ([tour (initial-tour graph cities)]
        [l (list-assoc graph)])
    (successiv-insertion graph tour (remove (cadr tour) (remove (car tour) tour)) l)))

(define (initial-tour graph cities)
  (letrec ([aux
            (lambda(graph cities degenerate cities-remember city)
              (cond
                [(null? (cdr cities-remember)) degenerate]
                [(null? cities) (aux graph (cdr cities-remember) degenerate (cdr cities-remember) (car cities-remember))]
                [(> (car degenerate) (distance graph city (car cities))) (aux graph (cdr cities) (list (distance graph city (car cities)) city (car cities)) cities-remember city)]
                [else (aux graph (cdr cities) degenerate cities-remember city)]))])
    (aux graph (cdr cities) (list (distance graph (car cities) (car (cdr cities))) (car cities) (car (cdr cities))) (cdr cities) (car cities))))


(define (successiv-insertion graph tour cities l-assoc)
 (cond
   [(null? cities) tour]
   [else (successiv-insertion graph (insert-and-found graph tour cities) cities l-assoc)]))


(define (insert-and-found graph tour cities l-assoc)
 0 )

(define (geq-found minim graph l-assoc cities start)
  (let([m (mini graph start l-assoc cities (Dijkstra graph start l-assoc))])
  (if (> minim (distance graph start m))
      m
      #f)))

;;; Attention appeler la fonction avec tour et cities non vide.
(define (found graph tour cities l-assoc)
  (letrec ([aux
            (lambda (graph tour cities mini)
              (cond
                [(null? tour) mini]
                [(geq-found (cadr mini) graph l-assoc cities (car tour))=> (lambda(new) (aux graph (cdr tour) cities new))]
                [else (aux graph (cdr tour) cities mini )]))])
    (aux graph tour cities (list (car cities) (distance graph (car tour) (car cities))))))


;(define (insert graph tour to-insert len))
;(initial-tour data-graph '("3924371555" "345772925" "283653048" "3924371764" "345772340"))


;(define data (xml->xexpr (document-element
 ;                            (read-xml (open-input-file "../maps/map2.osm")))))
;(define data-graph (osm-to-full-sorted data))
;(nearest data-graph '("3924371555" "345772925" "283653048" "3924371764" "345772340"))
;(nearest data-graph '("2140390961" "2140390959" "2140390960"))