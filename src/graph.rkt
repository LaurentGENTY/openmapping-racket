#lang racket

#| ----- Requirement ----- |#
(require racket/contract)


#| ----- Functions provided anywhere -----
    *  node?
    *  graph?
    *  in-graph?
    *  make-node
    *  convert-to-node
    *  n-neighbour
    *  n-id
    *  n-lat
    *  n-lon
    *  make-graph
    *  add-graph
    *  get-graph
    *  list-of-key
    *  number-of-nodes
    *  modif-node-neighbour
    *  print-node
    *  equal-nodes
    *  min-lat
    *  max-lat
    *  min-lon
    *  max-lon
    *  is-inside
    *  nodes-inside
|#


#| ----- Contract for Graph abstract data ----- |#
(provide (contract-out
          ;; Predicate identifying the node (returns true iff the argument is a node)
          ;; @param  n : any/c
          ;; @return   : boolean?
          [node? (-> any/c boolean?)]
          ;; Predicate identifying the graph (returns true iff the argument is a graph)
          ;; @param  g : any/c
          ;; @return   : boolean?     
          [graph? (-> any/c boolean?)]
          ;; Predicate identifying the possesion of the node by the graph (returns true if the argument is in the graph)
          ;; @param graph       : graph?
          ;; @param string      : string?
          ;; @return   : boolean?     
          [in-graph? (-> graph? string? boolean?)]
          ;; Constructor for nodes
          ;; @param id       : string?
          ;; @param lat      : string?
          ;; @param lon      : string?
          ;; @param neighbour: listof string?
          ;; @return   : node? 
          [make-node (-> string? string? string? (listof string?) node?)]
          ;; Convert a list into a node
          ;; @param list       : listof any/c
          ;; @return           : node? 
          [convert-to-node (-> (listof any/c) node?)]
          ;; Return the longitude of the given node
          ;; @param node       : node?
          ;; @return           : number? 
          [n-lon (-> node? string?)]
          ;; Return the latitude of the given node
          ;; @param node       : node?
          ;; @return           : number? 
          [n-lat (-> node? string?)]
          ;; Return the id of the given node
          ;; @param node       : node?
          ;; @return           : number? 
          [n-id (-> node? string?)]
          ;; Return the list of neighbour of the given node
          ;; @param node       : node?
          ;; @return           : number? 
          [n-neighbour (-> node? (listof string?))]
          ;; Return an empty graph
          ;; @return           : graph? 
          [make-graph (-> graph?)]
          ;; Add a node to the given graph (Replace the current node if the id is already in the graph)
          ;; @param node        : node?
          ;; @param graph       : graph?
          ;; @return            : graph? 
          [add-graph (-> graph? node? graph?)]
          ;; Return the node matching the given key
          ;; @param graph       : graph?
          ;; @param string      : string?
          ;; @return            : any/c
          [get-graph (-> graph? string? any/c)]
          ;; Return graph key list
          ;; @param graph       : graph?
          ;; @return            : listof string? 
          [list-of-key (-> graph? (listof string?))]
          ;; Return the number of nodes in the given graph
          ;; @param graph       : graph?
          ;; @return            : integer? 
          [number-of-nodes (-> graph? number?)]
          ;; Change the current neighbours of the node for the list
          ;; @param node       : node?
          ;; @param list       : listof string?
          ;; @return           : void? 
          [modif-node-neighbour (-> node? (listof string?) void?)]
          ;; Print the id lat long of the node
          ;; @param node       : node?
          ;; @return           : void? 
          [print-node (-> node? void?)]
          ;; Test the equality between two nodes
          ;; @param node       : node?
          ;; @param node       : node?
          ;; @return           : boolean? 
          [equal-nodes (-> node? node? boolean?)]
          ;; Count the number of way in a graph
          ;; @param graph      : graph?
          ;; @return           : number? 
          [number-of-ways (-> graph? number?)]
          ;; Check if a node is inside without returning the node if so
          ;; @param graph      : graph?
          ;; @param n          : string?
          ;; @return           : boolean? 
          [is-inside (-> graph? string? boolean?)]
          ;; Check if nodes are inside the graph
          ;; @param graph      : graph?
          ;; @param n          : list?
          ;; @return           : list? 
          [nodes-inside (-> graph? list? list?)]))


#| ----- Node ----- |#

#| ----- Node structure ----- |#
  ;; @param id       : string?
  ;; @param lat      : string?
  ;; @param lon      : string?
  ;; @param neighbour: listof string?
(struct node (id lat lon neighbour) #:mutable)

#| ----- Make-node ----- |#
  ;; @brief           : create a node
  ;; @param id        : string?
  ;; @param lat       : string?
  ;; @param lon       : string?
  ;; @param neighbour : listof string?
  ;; @return          : string?
(define (make-node id lat lon neighbour)
  (node id lat lon neighbour))

#| ----- print-node ----- |#
  ;; @brief           : print informations of a node
  ;; @param n   : node?
  ;; @return    : void?
(define (print-node n)
  (cond
     [(node? n) (and (display "id : ")
                     (print (n-id n))
                     (display "\nlat : ")
                     (print (n-lat n))
                     (display "\nlon : ")
                     (print (n-lon n))
                     (display "\n"))]
     [else (raise "Not a node")]))

#| ----- modif-node-neighbour ----- |#
  ;; @brief      : affect the new list of neighbour
  ;; @param n    : node?
  ;; @param l    : listof node?
  ;; @return     : string?
(define (modif-node-neighbour n l)
  (set-node-neighbour! n l))

#| ----- Convertion from osm format to node ----- |#
  ;; @brief   : convert a string (list) into a node. For example, given (node (id "1750142302") (lat "44.9254560") (lon "-0.4875690")) from osm
  ;; Returns node { id = 1750142302 , lat = 44.9254560, lon = -0.4875690}
  ;; @param l : listof any/c
(define (convert-to-node l)
  ;; Create a node with next parameters
  (node
   ;; node-id
   (car(cdr (assoc 'id (cdr l))))
   ;; node-lat
   (car(cdr (assoc 'lat (cdr l))))
   ;; node-lon
   (car(cdr (assoc 'lon (cdr l))))
   ;; node-neighbour
   '()))

#| ----- Graph ----- |#

#| ----- graph? ----- |#
  ;; @brief      : verify if the parameter is a graph
  ;; @param g    : graph?
  ;; @return     : boolean?
(define (graph? g)
  ;; If the param if a hash table
  (if(hash? g)
     ;; if it's empty then it's a graph
     (if (hash-empty? g)
         #t
         (loop g))
     #f))

#| ----- loop ----- |#
  ;; @brief      : verify if the parameter is a graph
  ;; @param g    : graph?
  ;; @return     : boolean?
(define (loop g)
  (let ([b #t])
    ;; list all supposed node in the hash table : if one of those is not a node then false, otherwise true
  (for ([i (hash-keys g)])
    (when (not(node? (hash-ref g i)))
      (set! b #f )));; @param node       : node?
  b))

#| ----- in-graph? ----- |#
  ;; @brief      : verify if the parameter is in a graph
  ;; @param g : graph?
  ;; @param id: string?
  ;; @return     : boolean?
(define (in-graph? g id)
  (node? (hash-ref g id #f)))

  
#| ----- Make-graph ----- |#
  ;; @brief   : give an empty graph (without node)
  ;; @return  : graph?
(define (make-graph)
  (hash))

#| ----- Add-graph ----- |#
  ;; @brief   : add a node in a graph
  ;; @param g : graph?
  ;; @param id: string?
  ;; @return  : graph?
(define (add-graph g nd)
  (hash-set g (node-id nd) nd))

#| ----- Get-graph ----- |#
  ;; @brief   : give the node depending on id given
  ;; @param g : graph?
  ;; @param id: string?
  ;; @return  : node?
(define (get-graph g id)
  (hash-ref g id))

#| ----- Getters ----- |#

#| ----- n-neighbour ----- |#
  ;; @brief      : give the list of neighbours of a node
  ;; @param node : node?
  ;; @return     : listof node?
(define (n-neighbour node)
  (node-neighbour node))

#| ----- n-id ----- |#
  ;; @brief      : give the id of a node
  ;; @param node : node?
  ;; @return     : string?
(define (n-id node)
  (node-id node))

#| ----- n-lat ----- |#
  ;; @brief      : give the latitude of a node
  ;; @param node : node?
  ;; @return     : string?
(define (n-lat node)
  (node-lat node))

#| ----- n-lon ----- |#
  ;; @brief      : give the longitude of a node
  ;; @param node : node?
  ;; @return     : string?
(define (n-lon node)
  (node-lon node))

#| ----- list-of-key ----- |#
  ;; @brief          : give a list of nodes in a graph
  ;; @param graph    : graph?
  ;; @return         : listof string?
(define (list-of-key graph)
  (hash-keys graph))

#| ----- number-of-nodes ----- |#
  ;; @brief          : give the number of nodes in the graph
  ;; @param graph    : graph?
  ;; @return         : integer?
(define (number-of-nodes g)
  (length (hash-keys g)))


#| ----- equal-nodes ----- |#
  ;; @brief         : test the equality of two nodes
  ;; @param node    : node?
  ;; @param node    : node?
  ;; @return        : bool?
(define (equal-nodes n1 n2)
  (and (equal? (n-id n1) (n-id n2))
       (equal? (n-lat n1) (n-lat n2))
       (equal? (n-lon n1) (n-lon n2))
       (equal? (length (n-neighbour n1)) (length (n-neighbour n2)))
       (car (map equal? (n-neighbour n1) (n-neighbour n1)))))

#| ----- number-of-ways ----- |#
  ;; @brief         : count the way number 
  ;; @param graph   : graph
  ;; @return        : number?
(define (number-of-ways graph)
  (letrec ([aux
            (lambda (l n g)
              (cond
                [(null? l) 0]
                [else (+ (length (node-neighbour (get-graph g (car l))))
                         (aux (cdr l) (length (node-neighbour (get-graph g (car l)))) graph))]))])
    (/ (aux (list-of-key graph) 0 graph) 2)))

#| ----- is-inside ----- |#
  ;; @brief         : return if a node is inside a graph
  ;; @param graph   : graph?
  ;; @param n       : string?
  ;; @return        : boolean?
(define (is-inside g n)
  (hash-has-key? g n))


#| ----- nodes-inside ----- |#
  ;; @brief         : check if all nodes are inside the graph
  ;; @param graph   : graph?
  ;; @param l       : list?
  ;; @return        : list?
(define (nodes-inside g l)
  (cond
    [(null? l) (list #t)]
    [else (cons (hash-has-key? g (car l)) (nodes-inside g (cdr l)))]))