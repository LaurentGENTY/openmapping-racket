#lang racket
(require racket/include)

(require "graph.rkt")
(require "parsing.rkt")

(provide fully-added-neighbours sort-2 fully-converted osm-to-full-sorted add-successor)

;; Fonction permettant d'ajouter tous les voisins des noeuds
(define (fully-added-neighbours m graph)
  (cond
    [(null? m) graph]
    [(equal? 'way (caar m)) (fully-added-neighbours (cdr m) (add-successor graph (car m)))]
    [else (fully-added-neighbours (cdr m) graph)]))

;; appel la fonction permettant d'ajouter les voisins pour chaque noeud présent dans way
(define (add-successor graph way)
  (add_successor_edge graph (filter (lambda(x) (string=? (symbol->string (car x)) "ref")) (cdr way))))

;; Appel la fonction qui lie deux voisins pour chaque sommet présent dans la liste edges
(define (add_successor_edge graph edges)
  (cond
        [(null? (cdr edges)) graph]
        [else (add_successor_edge (add_table_2 graph (car edges) (car (cdr edges))) (cdr edges))]))

;; modifie le graphe de sorte que les deux noeud voisins fournis en entrée aient dans leur champs neighbour le voisin correspondant
(define (add_table_2 graph e e1)
  (modif-node-neighbour (get-graph graph (cadr e)) (cons (cadr e1) (n-neighbour (get-graph graph (cadr e) ))))
  (modif-node-neighbour  (get-graph graph (cadr e1)) (cons (cadr e) (n-neighbour (get-graph graph (cadr e1)))))
   graph)

;;----------------------    Tests    ---------------------
;(set! g (add-successor g '(way (id "248059998") (ref "515330686") (ref "415330686") (ref "315330686") (v "residential"))))
;(n-neighbour(get-graph g "415330686"))
;;---------------------  Fin des tests  --------------------

;; Fonction permettant de nettoyer le graphe des sommets de degré deux
(define (sort-2 graph)
  (for ([i (list-of-key graph)])
      (when (= (length (n-neighbour (get-graph graph i))) 2)
        (set! graph (graph-remove graph i))
        (set! graph (hash-remove graph i))))
  graph)

;; Fonction permettant de nettoyer le graphe des sommets de degré zéro
(define (filter-zero graph)
  (letrec ([aux (lambda (l g)
                  (if (null? l)
                  g
                  (let ([n (get-graph graph (car l))])
                    (if (zero? (length (n-neighbour n)))
                        (aux (cdr l) (hash-remove g (car l)))
                        (aux (cdr l) g)))))])
    (aux (list-of-key graph) graph)))
                        

;; Fonction qui retire au graphe le sommet de degré 2 fourni en paramètre et actualise ses sommets voisins
(define (graph-remove graph i)
  (let ([l (n-neighbour (get-graph graph i))])
    (let ([ n (make-node (n-id (get-graph graph (car l)))
                         (n-lat (get-graph graph (car l)))
                         (n-lon (get-graph graph (car l)))
                         (new-list (n-neighbour (get-graph graph (car l))) (car(cdr l)) i ))])
      (set! graph (add-graph (hash-remove graph (car l)) n)))
    (let ([ n1 (make-node (n-id (get-graph graph (car(cdr l))))
                          (n-lat (get-graph graph (car(cdr l))))
                          (n-lon (get-graph graph (car(cdr l))))
                          (new-list (n-neighbour (get-graph graph (car(cdr l)))) (car l) i ))])
      (set! graph (add-graph (hash-remove graph (cdr l)) n1))))
  graph)

;; Fonction créant une nouvelle liste où add remplace erase
(define (new-list list add erase)
  (cond
      [(equal? (car list) erase) (cons add (cdr list))]
      [else (cons (car list) (new-list (cdr list) add erase))]))


;; Fonction qui permet d'initialiser le graphe avec les noeuds sans leurs voisins
(define (fully-converted m graph)
  (cond
    [(null? m) graph]
    [(equal? 'node (caar m)) (fully-converted (cdr m) (add-graph graph (convert-to-node (car m)))) ]
    [else (fully-converted (cdr m) graph)]))


#| ----- osm-to-full-sorted ----- |#
  ;; @brief           : does the whole converting process : convert it to XML, filter empty fields, generate an empty graph,
  ;;                    fill it with file's nodes, add the neighbours, remove nodes with degree zero
  ;; @param maps      : list?
  ;; @return          : graph? (full of node with their neighbours)
(define (osm-to-full-sorted maps)
  (let* ([mappy (foldl filt2 '() (cddr maps))]
  ;(let* ([mappy (filter not-empty (map filt (cddr maps)))]
        [g (filter-zero (fully-added-neighbours mappy (fully-converted mappy (make-graph))))])
    g))

#|(define (osm-to-full-sorted maps)
  (let ([mappy (filter not-empty (map filt (cddr maps)))]
        [g (make-graph)])
    (set! g (fully-converted mappy g))
    (fprintf (current-output-port) "Fully converted\n")
    (set! g (fully-added-neighbours mappy g))
    (fprintf (current-output-port) "Add neighbours\n")
    ;(set! g (filter-zero g))
    (fprintf (current-output-port) "Filter 0\n")
    ;;(set! g (sort-2 g))
    g))|#

;;(require xml)
;;(define data (xml->xexpr (document-element
;;(read-xml (open-input-file "../maps/map2.osm")))))
;;(define d (osm-to-full-sorted data))
;;(map (lambda (id) (length (n-neighbour (get-graph d id)))) (list-of-key d))
  
