#lang racket

(require "../src/graph.rkt")
(require "../src/graph_construction.rkt")
(require "../src/parsing.rkt")
(require xml)
;; fichier test

(define maps (xml->xexpr (document-element
    (read-xml (open-input-file "maps/map.osm")))))

;;Load osm file : convert it to XML, filter empty fields, generate an empty graph, fill it with file's nodes, add the neighbours, sort nodes with degree 2
(define g (osm-to-full-sorted maps))

;; Account the successing test number
(define (test cond l)
  (if cond
      (list (add1 (car l)) (add1 (cadr l)))
      (list (car l) (add1 (cadr l)))
      ))
;; Print if the test success and how many test passed
(define (printf l)
  (fprintf (current-output-port)
                           "~a = ~a / ~a\n"
                           (if(= (car l) (cadr l))
                              "SUCCESS"
                              "FAILURE"
                              )
                           (car l)
                           (cadr l)
                           ))

;;Predicate identifying the possesion of x by the list (returns true if the argument is in the list)
(define (contain list x)
    (cond
        ((null? list) #f)
        ((eq? (car list) x) #t)
        (else (contain (cdr list) x))))

;; Test node? (We suppose that node? recognize a node)
(define test_node?
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (set! l (test (not (node? 5)) l)) ;; Reconizes if it's not a node
                  (fprintf (current-output-port) "~a ""Test node? :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test graph?
(define test_graph?
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (set! l (test (not (graph? 5)) l)) ;; Test not a hash
                  (set! l (test (graph? (hash)) l)) ;; Test empty hash
                  (define h (hash-set (hash) 4 1))
                  (set! l (test (not (graph? h)) l)) ;; Test hash without node
                  (define n (convert-to-node '(node (id 1750141945) (lat 44.9253650) (lon -0.4874640))))
                  (define ha (hash-set (hash) 5 n))
                  (set! l (test (graph? ha) l)) ;; Test hash with node
                  (fprintf (current-output-port) "~a ""Test graph? :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test in-graph?
(define test_in-graph?
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define graph (make-graph))
                  (define node (convert-to-node '(node (id "186254") (lat "10") (lon "15"))))
                  (set! l (test (not (in-graph? graph "186254")) l)) ;; Test if in-graph? return #f if the node isn't in the graph
                  (set! graph (add-graph graph node))
                  (set! l (test (in-graph? graph "186254") l)) ;; Test if in-graph? return #t if the node is in the graph
                  (fprintf (current-output-port) "~a ""Test in-graph? :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test make-node
(define test_make-node
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (make-node "415330686" "44.7754213" "-0.8587464" '("-12.46435" "4.523984")))
                  (set! l (test (node? n) l)) ;; Test if make-node create a node
                  (fprintf (current-output-port) "~a ""Test make-node :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test convert-to-node
(define test_convert-to-node
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (convert-to-node '(node (id "415330686") (lat "44.7754213") (lon "-0.8587464"))))
                  (set! l (test (node? n) l)) ;; Test if make-node create a node
                  (set! l (test (and
                                 (string=? (n-lon n) "-0.8587464")
                                 (string=? (n-lat n)"44.7754213")
                                 (string=? (n-id n) "415330686")) l)) ;; Test if make-node create a node with
                                                                    ;;the right data
                  (define n2 (convert-to-node '(node  (lon "1.742758") (id "518234823") (lat "23.896125"))))
                  (set! l (test (and
                                 (string=? (n-lon n2) "1.742758")
                                 (string=? (n-lat n2) "23.896125")
                                 (string=? (n-id n2) "518234823")) l)) ;; Test if make-node create a node with the right
                                                                     ;;data even if we give the data in the wrong order
                  (fprintf (current-output-port) "~a ""Test convert-to-node :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test n-lon
(define test_n-lon
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (make-node "415330686" "44.7754213" "-0.8587464" '("8463162")))
                  (set! l (test (string=? "-0.8587464" (n-lon n)) l)) ;; Test if make-node create a node
                  (fprintf (current-output-port) "~a ""Test n-lon :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test n-lat
(define test_n-lat
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (make-node "415330686" "44.7754213" "-0.8587464" '("8463162")))
                  (set! l (test (string=? "44.7754213" (n-lat n)) l)) ;; Test if make-node create a node
                  (fprintf (current-output-port) "~a ""Test n-lat :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test n-lon
(define test_n-id
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (make-node "415330686" "44.7754213" "-0.8587464" '("8463162")))
                  (set! l (test (string=? "415330686" (n-id n)) l)) ;; Test if make-node create a node
                  (fprintf (current-output-port) "~a ""Test n-id :")
                  (printf l)
                  )))
    (cpt '(0 0))))

(define test_neighbour_empty
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (make-node "1" "44.7754213" "-0.8587464" '()))
                  (set! l (test (= 0 (length (n-neighbour n))) l)) ;; test if n has no neighbours
                  (fprintf (current-output-port) "~a ""Test n-empty-neighbour :")
                  (printf l)
                  )))
    (cpt '(0 0))))

(define test_n-neighbour
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (make-node "415330686" "44.7754213" "-0.8587464" '("8463162" "184623")))
                  (set! l (test (and (string=? "8463162" (car (n-neighbour n)))
                                     (string=? "184623" (cadr (n-neighbour n)))) l)) ;; Test if make-node create a node
                  (fprintf (current-output-port) "~a ""Test n-neighbour :")
                  (printf l)
                  )))
    (cpt '(0 0))))


;; Test make-graph
(define test_make-graph
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define graph (make-graph))
                  (set! l (test (hash-empty? graph) l)) ;; Test if graph is empty (without node)
                  (fprintf (current-output-port) "~a ""Test make-graph :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test add-graph
(define test_add-graph
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define graph (make-graph))
                  (define node (convert-to-node '(node (id "1") (lat "10") (lon "15"))))
                  (set! graph (add-graph graph node))
                  (set! l (test (not (hash-empty? graph)) l)) ;; Test if graph is not empty
                  (set! l (test (hash-ref graph "1") l)) ;; Test if the node is in the graph
                  (define node2 (convert-to-node '(node (id "1") (lat "2") (lon "3"))))
                  (set! graph (add-graph graph node2))
                  (set! l (test (string=? "2" (n-lat (get-graph graph "1"))) l)) ;; Test if we had a second time a node with the same id that change
                                                                                 ;; its lat,lon and neigh
                  (define node_3 (convert-to-node '(node (id "2") (lat "5") (lon "-10"))))
                  (set! graph (add-graph graph node_3))
                  (set! l (test (hash-ref graph "1") l)) ;; Test if the node is still in the graph
                  (set! l (test (hash-ref graph "2") l)) ;; Test if the second node is in the graph
                  (fprintf (current-output-port) "~a ""Test add-graph :")
                  (printf l)
                  )))
    (cpt '(0 0))))


;; Test get-graph
(define test_get-graph
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define graph (make-graph))
                  (define node (convert-to-node '(node (id "186254") (lat "10") (lon "15"))))
                  (set! graph (add-graph graph node))
                  (define n (get-graph graph "186254"))
                  (set! l (test (and (node? n)
                                     (string=? "186254" (n-id n))
                                     (string=? "10" (n-lat n))
                                     (string=? "15" (n-lon n))) l)) ;; Test if get-graph return the right node in a one-node-graph
                  (define node2 (convert-to-node '(node (id "286254") (lat "54") (lon "-2"))))
                  (set! graph (add-graph graph node2))
                  (define n_new (get-graph graph "186254"))
                  (define n_new2 (get-graph graph "286254"))
                  (set! graph (add-graph graph node2))
                                    (set! l (test (and (node? n_new)
                                     (string=? "186254" (n-id n_new))
                                     (string=? "10" (n-lat n_new))
                                     (string=? "15" (n-lon n_new))) l)) ;; Test if get-graph still return the node
                  (set! graph (add-graph graph node2))
                                    (set! l (test (and (node? n_new2)
                                     (string=? "286254" (n-id n_new2))
                                     (string=? "54" (n-lat n_new2))
                                     (string=? "-2" (n-lon n_new2))) l)) ;; Test if get-graph return the last added node
                  (fprintf (current-output-port) "~a ""Test get-graph :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test list-of-key
(define test_list-of-key
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define graph (make-graph))
                  (define node (convert-to-node '(node (id "4") (lat "5") (lon "15"))))
                  (define node2 (convert-to-node '(node (id "2") (lat "-10") (lon "42"))))
                  (define node3 (convert-to-node '(node (id "3") (lat "1") (lon "8"))))
                  (set! l (test (equal? '() (list-of-key graph)) l)) ;; Test if return a empty list for a empty graph
                  (set! graph (add-graph graph node))
                  (set! graph (add-graph graph node2))
                  (set! graph (add-graph graph node3))
                  (define list (list-of-key graph))
                  (set! l (test (and (contain list "4")
                                     (contain list "2")
                                     (contain list "3")) l)) ;; Test if return the node keys list of the graph
                  (fprintf (current-output-port) "~a ""Test list-of-key :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test list-of-key
(define test_number-of-nodes
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define graph (make-graph))
                  (define node (convert-to-node '(node (id "4") (lat "5") (lon "15"))))
                  (define node2 (convert-to-node '(node (id "2") (lat "-10") (lon "42"))))
                  (define node3 (convert-to-node '(node (id "3") (lat "1") (lon "8"))))
                  (set! l (test (zero? (number-of-nodes graph)) l)) ;; Test if return the node number for a empty graph
                  (set! graph (add-graph graph node))
                  (set! graph (add-graph graph node2))
                  (set! graph (add-graph graph node3))
                  (set! l (test (= 3 (number-of-nodes graph)) l)) ;; Test if return the node number a empty graph
                  (fprintf (current-output-port) "~a ""Test number-of-nodes :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test modif-node-neighbour
(define test_modif-node-neighbour
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define node (convert-to-node '(node (id "4") (lat "5") (lon "15"))))
                  (modif-node-neighbour node '("2" "3"))
                  (define list (n-neighbour node))
                  (set! l (test (and (contain list "2")
                                     (contain list "3")) l)) ;; Test if return the neighbour of the node is correct
                  (modif-node-neighbour node '("4"))
                  (define list2 (n-neighbour node))
                  (set! l (test (and (not (contain list2 "2"))
                                     (not (contain list2 "3"))
                                     (contain list2 "4")) l)) ;; Test if return the neighbours of the node has changed
                  (fprintf (current-output-port) "~a ""Test modif-node-neighbour :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test equal-nodes
(define test_equal-nodes
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  (define n (make-node "1" "2" "3" '("4")))
                  (define n1 (make-node "2" "2" "3" '("4")))
                  (define n2 (make-node "1" "-2" "3" '("4")))
                  (define n3 (make-node "1" "2" "0" '("4")))
                  (define n4 (make-node "1" "2" "3" '("4" "5")))
                  (define n5 (make-node "1" "2" "3" '("4")))
                  (set! l (test (and (not (equal-nodes n n1))
                                     (not (equal-nodes n n2))
                                     (not (equal-nodes n n3))
                                     (not (equal-nodes n n4))
                                     (equal-nodes n n5)) l)) ;; Test if node are egual only if the have the same
                                                             ;; id,lat,lon and neighbour
                  (fprintf (current-output-port) "~a ""Test equal-nodes  :")
                  (printf l)
                  )))
    (cpt '(0 0))))

;; Test sort-2
(define test_sort2
  (letrec ((cpt (lambda(l) ;; l = '(success,tot)
                  ;; creating nodes without neighbour
                  (define n1 (convert-to-node '(node (id "1") (lat "44.7754213") (lon "-0.8587464") '("2" "3"))))
                  (define n2 (convert-to-node '(node (id "2") (lat "44.7754213") (lon "-0.8587464") '("1" "3"))))
                  (define n3 (convert-to-node '(node (id "3") (lat "44.7754213") (lon "-0.8587464") '("1" "2" "4" "5" "6"))))
                  (define n4 (convert-to-node '(node (id "4") (lat "44.7754213") (lon "-0.8587464") '("3"))))
                  (define n5 (convert-to-node '(node (id "5") (lat "44.7754213") (lon "-0.8587464") '("3"))))
                  (define n6 (convert-to-node '(node (id "6") (lat "44.7754213") (lon "-0.8587464") '("3"))))
                  ;; creating the graph
                  (define h (make-graph))
                  ;; adding nodes into the graph
                  (set! h (add-graph h n1))
                  (set! h (add-graph h n2))
                  (set! h (add-graph h n3))
                  (set! h (add-graph h n4))
                  (set! h (add-graph h n5))
                  (set! h (add-graph h n6))
                  ;;adding ways for neighbours
                  (set! h (add-successor h '(way (id "10") (ref "1") (ref "2") (v "residential"))))
                  (set! h (add-successor h '(way (id "11") (ref "1") (ref "3") (v "residential"))))
                  (set! h (add-successor h '(way (id "11") (ref "2") (ref "3") (v "residential"))))
                  (set! h (add-successor h '(way (id "12") (ref "3") (ref "4") (v "residential"))))
                  (set! h (add-successor h '(way (id "13") (ref "3") (ref "5") (v "residential"))))
                  (set! h (add-successor h '(way (id "14") (ref "3") (ref "6") (v "residential"))))
                  ;; remove all nodes with degree 2 of neighbours
                  (set! h (sort-2 h))
                  ;; verifying that n1 and n2 are removed : 4 nodes left (3,4,5,6)
                  (set! l (test (and (= (number-of-nodes h) 4))
                                l))
                  (fprintf (current-output-port) "~a ""Test sort-2 :")
                  (printf l)
                  )))
    (cpt '(0 0))))


