#lang racket

;; ================================================================================
;; Tests on a real map : Bordeaux city centre (maps/bordeaux.osm, clipped with demo/clip-osm.py)
(require xml)
(require "../src/graph.rkt")
(require "../src/graph_construction.rkt")
(require "../src/gps.rkt")
(require "../src/route.rkt")
(require "../src/Dijkstra.rkt")
(require "../src/travelling.rkt")

(define g (osm-to-full-sorted (xml->xexpr (document-element (read-xml (open-input-file "maps/bordeaux.osm"))))))

;; Landmarks : nearest crossing of each place
(define pey-berland "7266323279")
(define parlement "1320554415")
(define saint-pierre "35258541")
(define camille-jullian "6581917877")
(define lafargue "251542437")
(define palais "35258447")
(define vieille-tour "13125256537")
(define quai-bourgeois "35258525")
(define hotel-de-ville "2047703799") ;; in a small component cut by the clipping

(define passed 0)
(define total 0)
(define (check name ok)
  (set! total (add1 total))
  (when ok (set! passed (add1 passed)))
  (fprintf (current-output-port) "~a ~a\n" (if ok "SUCCESS" "FAILED ") name))

;; consecutive nodes of a path are linked by a street
(define (follows-streets? p)
  (for/and ([a p] [b (cdr p)])
    (and (member b (n-neighbour (get-graph g a))) #t)))

(define (close? a b) (< (abs (- a b)) 1e-6))

;; ---- Graph
(check "graph : 1299 crossings and street nodes" (= (number-of-nodes g) 1299))

;; ---- Shortest path (Dijkstra)
(define short (find-my-way g camille-jullian saint-pierre))
(check "short route starts and ends on the right nodes" (and (equal? (first short) camille-jullian) (equal? (last short) saint-pierre)))
(check "short route follows the streets" (follows-streets? short))

;; Expected lengths computed independently (heapq Dijkstra in Python on the same .osm, haversine edges)
(for ([c (list (list camille-jullian saint-pierre 233.7045331273774)
               (list pey-berland parlement 647.6353131768801)
               (list vieille-tour quai-bourgeois 1015.8239193742608)
               (list camille-jullian lafargue 266.1716276042507)
               (list pey-berland lafargue 512.4270759835923)
               (list lafargue parlement 385.2059203087593))])
  (check (format "shortest ~a -> ~a is ~a m" (first c) (second c) (round (third c)))
         (close? (path-length g (find-my-way g (first c) (second c))) (third c))))

(define long (find-my-way g vieille-tour quai-bourgeois))
(check "west to east route follows the streets" (follows-streets? long))
(check "west to east route is longer than the straight line" (>= (path-length g long) (distance-m (get-graph g vieille-tour) (get-graph g quai-bourgeois))))

(check "a route has the same length both ways" (close? (distance g pey-berland parlement) (distance g parlement pey-berland)))
(check "triangle inequality" (<= (distance g pey-berland parlement) (+ (distance g pey-berland lafargue) (distance g lafargue parlement))))
(check "distance is the length of the drawn path" (close? (distance g pey-berland parlement) (path-length g (find-my-way g pey-berland parlement))))
(check "no route to another connected component" (null? (find-my-way g pey-berland hotel-de-ville)))

;; ---- Greedy search (find_path) : valid but never shorter than Dijkstra
(for ([pair (list (list pey-berland parlement) (list vieille-tour quai-bourgeois) (list camille-jullian lafargue))])
  (let ([greedy (find_path g (first pair) (second pair))]
        [best (find-my-way g (first pair) (second pair))])
    (check (format "greedy path ~a -> ~a follows the streets" (first pair) (second pair)) (follows-streets? greedy))
    (check (format "Dijkstra ~a -> ~a is not longer than greedy" (first pair) (second pair)) (<= (path-length g best) (+ (path-length g greedy) 1e-6)))))

;; ---- Travelling salesman (nearest neighbour)
(define (valid-cycle? cities)
  (let ([c (nearest g cities)])
    (and (pair? c)
         (equal? (first c) (last c))
         (follows-streets? c)
         (for/and ([city cities]) (and (member city c) #t))
         ;; no street crossing is used twice, apart from the start that closes the loop
         (= (length (remove-duplicates c)) (sub1 (length c))))))

(check "cycle through 3 places" (valid-cycle? (list saint-pierre lafargue camille-jullian)))
(check "cycle through 5 places" (valid-cycle? (list pey-berland saint-pierre lafargue palais camille-jullian)))
(check "no cycle with a place in another component" (null? (nearest g (list pey-berland hotel-de-ville saint-pierre))))

(fprintf (current-output-port) "~a = ~a / ~a\n" (if (= passed total) "SUCCESS" "FAILURE") passed total)
(unless (= passed total) (exit 1))
