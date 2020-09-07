#lang racket

;; ================================================================================
;; test on dijkstra and A_star
(require xml)
(require "../src/graph.rkt")
(require "../src/graph_construction.rkt")
(require "../src/parsing.rkt")
(require "../src/route.rkt")
(require "../src/Dijkstra.rkt")
;;(require "../..src/graph_construction.rkt")
;;(require xml)

(define data (xml->xexpr (document-element (read-xml (open-input-file "maps/map2.osm")))))
(define gr (osm-to-full-sorted data))

(define (test-path graph fun-find-path start end expected-path)
  (let ([ls (fun-find-path gr start end)])
    (print ls)
    (if (equal? ls expected-path)
      (fprintf (current-output-port) "~a ""SUCCESS\n")
      (fprintf (current-output-port) "~a ""FAILED\n"))))

(fprintf (current-output-port) "~a ""Test find path\n")
(define start "3924371770")
(define end "345772340")
(define end2 "3924371763")
(define expected '("3924371770" "3924371764" "6069094601" "3924371759" "283653049" "283653048" "6069094602" "345772724" "345772722" "345772720" "345772925" "345772924" "345772340"))
(test-path gr find-my-way start end expected)
(test-path gr find_path start end expected)
(test-path gr find-my-way start end2 '("3924371770" "3924371774" "3924371763"))
(test-path gr find_path start end2 '("3924371770" "3924371774" "3924371763"))