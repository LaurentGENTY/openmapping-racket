#lang racket

#| ----- Requirement ----- |#
(require "parsing.rkt")
(require "graph.rkt")
(require "graph_construction.rkt")
(require "svg.rkt")
(require "route.rkt")
(require "travelling.rkt")
(require "Dijkstra.rkt")

(require xml)
(require web-server/servlet
         web-server/servlet-env)

;; get the bounds for each latitude and longitude
;; global variables : useful for displaying infos of the grpah without re-calculating bounds
(define max-lat
  "-90")
(define max-lon
  "-180")
(define min-lat
  "90")
(define min-lon
  "180")

;; ---------------- Utils -----------------------------

#| ----- get-bounds ----- |#
  ;; @brief       : for all nodes in the graph, create lists for latitudes and longitudes and retrieve max and min for both latitude and longitude
  ;;                complexity of O(2n) + 4*O(n)
  ;; @param graph : graph?
  ;; @return      : list of bounds (maxLat minLat maxLon minLon) AND top level bounds retrieved (optional... maybe it will be removed after)
(define (get-bounds graph)
  (let* ([llat (get-lats (list-of-key graph))]
        [llon (get-lons (list-of-key graph))]
        [la-max (apply max llat)]
        [la-min (apply min llat)]
        [lo-max (apply max llon)]
        [lo-min (apply min llon)])
    ;; optionnal set!
    (set! max-lat la-max)
    (set! min-lat la-min)
    (set! max-lon lo-max)
    (set! min-lon lo-min)
    (list la-max la-min lo-max lo-min)))

#| ----- get-bounds-lat ----- |#
  ;; @brief     : fill the list of latitude in order to get the maximum and minimum latitude
  ;; @param  l  : list? of latitudes
  ;; @return    : lats filled
(define (get-lats l)
  (cond
    [(empty? l) l]
    [else (cons (string->number(n-lat (get-graph g (car l))))
                (get-lats (cdr l)))]))

#| ----- get-bounds-lon ----- |#
  ;; @brief     : fill the list of longitude in order to get the maximum and minimum longitude
  ;; @param  l  : list? of longitudes
  ;; @return    : lons filled
(define (get-lons l)
  (cond
    [(empty? l) l]
    [else (cons (string->number(n-lon (get-graph g (car l))))
                (get-lons (cdr l)))]))

#| ----- infos ----- |#
  ;; @brief     : show infos about the graph in the terminal
  ;; @param  g  : graph?
  ;; @return    : fprintf in the terminal
(define (infos g)
    (fprintf (current-output-port)
             "File : ~s\n"
             src-file)
  (fprintf (current-output-port)
           "Number of nodes : ~s\n"
           (number-of-nodes g))
  (fprintf (current-output-port)
           "Number of ways : ~s\n"
           (number-of-ways g))
  (fprintf (current-output-port)
           "Bounds are : \nmax latitude ~s \nmin latitude ~s \nmax longitude ~s \nmin longitude ~s \n"
           max-lat
           min-lat
           max-lon
           min-lon))

#| ----- infos-html ----- |#
  ;; @brief     : show infos about the graph in the html page
  ;; @param  g  : graph?
  ;; @return    : list? for xeprx
(define (infos-html g)
  (append `(ul ((id "liste")) , "Graph's infos"
               (li ((id "file")) , (~a "File : " src-file))
               (li ((id "nb-nodes")) , (~a "Number of nodes : " (number-of-nodes g)))
               (li ((id "nb-ways")) , (~a "Number of ways : " (number-of-ways g)))
               (ul ((id "bounds")) , (~a "Bounds are : ")
                   (li ((id "max-lat")) , (~a "Maximum latitude : " max-lat))
                   (li ((id "min-lat")) , (~a "Minimum latitude : " min-lat))
                   (li ((id "max-lon")) , (~a "Maximum longitude : " max-lon))
                   (li ((id "min-lon")) , (~a "Minimum longitude : " min-lon)))
               (p ((id "nodes")) , (~a "List of nodes : " (list-of-key g))))))
  
#| ----- get-cities ----- |#
  ;; @brief     : retrieve all id nodes in URL for cycle
  ;; @param  l  : list?
  ;; @return    : list? of string? for cycle
(define (get-cities l)
  (cond
    [(null? l) '()]
    [else (string-split (cdar l) ",")]))

;; Get the file from command line and create the graph (with OSM file)
;; ------------------------------------

;; Get command line argument : osm file
(define src-file (vector-ref (current-command-line-arguments) 0))
;; retrieve the xml (list) data from src
(define maps (xml->xexpr (document-element
                              (read-xml (open-input-file src-file)))))

;; graph full of nodes (without degree 0)
(define g (osm-to-full-sorted maps))

;; retrieve real bounds
;; get the final svg container as a list
(define html-svg (graph-to-svg g (get-bounds g)))

;; Print infos of the graph in terminal
(infos g)

;; html-svg is the SVG we created
;; ------------------------------------

;; -------------------------------------------------
;; Forms and buttons for pages

(define button-node
  (append `(form ((action "http://localhost:9000/node") (method "get")) (list `(button ((type "submit")) "Node page")))))

(define button-route
  (append `(form ((action "http://localhost:9000/route") (method "get")) (list `(button ((type "submit")) "Route page")))))

(define button-distance
  (append `(form ((action "http://localhost:9000/distance") (method "get")) (list `(button ((type "submit")) "Distance page")))))

(define button-main
  (append `(form ((action "http://localhost:9000") (method "get")) (list `(button ((type "submit")) "Main page")))))

(define button-cycle
  (append `(form ((action "http://localhost:9000/cycle?") (method "get")) (list `(button ((type "submit")) "Cycle page")))))

(define form-node
    `(form ((action "/node") (method "get"))
           (p , "Node") (input ((type "text") (name "node")))
           (input ((type "submit") (value "Submit")))))

(define form-route
    `(form ((action "/route") (method "get"))
           (p , "Start") (input ((type "text") (name "start")))
           (p , "End:") (input ((type "text") (name "end")))
           (input ((type "submit") (value "Submit")))))

(define form-distance
    `(form ((action "/distance") (method "get"))
           (p , "Start") (input ((type "text") (name "start")))
           (p , "End:") (input ((type "text") (name "end")))
           (input ((type "submit") (value "Distance")))))


(define form-cycle
    `(form ((action "/cycle") (method "get"))
           (p , "Nodes:") (input ((type "text") (name "nodes")))
           (input ((type "submit") (value "Submit")))))

;; ---------------- PAGES -----------------------------

;; -------------------------------------------------
;; Error pages

#| ----- error-no-node ----- |#
  ;; @brief      : when the node doesnt exist
  ;; @return     : xexpr?
(define (error-no-node)
  (response/xexpr
   `(html (head (title "Node page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
          (body
           ,button-node
           ,button-route
           ,button-distance
           ,button-cycle
           ,button-main
           (h1 , "Good format for arguments in page http://localhost:9000/node?node=<id>")
           (h2 , "Node does not exist in the graph !")
           ,(infos-html g)
           ,form-node
           ,html-svg))))

#| ----- error-args ----- |#
  ;; @brief      : when there is not the right number of argument in route-page (2 nodes in the graph)
  ;; @return     : xexpr?
(define (error-args)
  (response/xexpr
   `(html (head (title "Route page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
          (body
           ,button-node
           ,button-route
           ,button-distance
           ,button-cycle
           ,button-main
           (h1 , "Good format for arguments in page http://localhost:9000/route?start=<id>&end=<id>")
           (h2 , "Both nodes need to be different and need to exist in the graph")
           ,(infos-html g)
           ,form-route
           ,html-svg))))

#| ----- error-dist ----- |#
  ;; @brief      : when there is not the right number of argument in distance-page (2 nodes all different)
  ;; @return     : xexpr?
(define (error-dist)
  (response/xexpr
   `(html (head (title "Distance page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
          (body
           ,button-node
           ,button-route
           ,button-distance
           ,button-cycle
           ,button-main
           (h1 , "Good format for arguments in page http://localhost:9000/route?start=<id>&end=<id>")
           (h2 , "Both nodes need to be different and need to exist in the graph")
           ,(infos-html g)
           ,form-distance
           ,html-svg))))

#| ----- error-cycle ----- |#
  ;; @brief      : when there is not the right number of argument in cycle-page (at least 2 nodes all different)
  ;; @return     : xexpr?
(define (error-cycle)
  (response/xexpr
   `(html (head (title "Cycle page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
          (bod
           ,button-node
           ,button-route
           ,button-distance
           ,button-cycle
           ,button-main
           (h1 , "Good format for arguments in page http://localhost:9000/cycle?nodes=<id>,...,<id>")
           (h2 , "All nodes need to be different and need to exist in the graph")
           ,(infos-html g)
           ,form-cycle
           ,html-svg
           ))))

#| ----- error-no-path ----- |#
  ;; @brief      : when there is not path between 2 correct nodes in a graph
  ;; @return     : xexpr?
(define (error-no-path req)
  (response/xexpr
   `(html (head (title "Distance page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
          (body
           ,button-node
           ,button-route
           ,button-distance
           ,button-cycle
           ,button-main
           (h1 , "Disconnected Universe Error")
           (h2 , (~a "No route between node : " (cdar (request-bindings req)) " and node : " (cdadr (request-bindings req))))
           ,(infos-html g)
           ,form-distance
           ;; SVG filled with the data in the graph
           ,html-svg
           ))))

#| ----- error-no-cycle ----- |#
  ;; @brief      : when there is no cycle
  ;; @return     : xexpr?
(define (error-no-cycle cities)
  (response/xexpr
   `(html (head (title "Cycle page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
          (body
           ,button-node
           ,button-route
           ,button-distance
           ,button-cycle
           ,button-main
           (h1 , "Disconnected Universe Error")
           (h2 , (~a "No cycle between nodes : " cities))
           ,(infos-html g)
           ,form-cycle
           ;; SVG filled with the data in the graph
           ,html-svg
           ))))

;; -------------------------------------------------
;; Working pages

#| ----- main-page ----- |#
  ;; @brief      : main page of the html
  ;; @param req  : requests given in the url
  ;; @return     : xexpr?
(define (main-page req)
  (response/xexpr
   `(html (head (title "Main page") (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
          (body
           ,button-node
           ,button-route
           ,button-distance
           ,button-cycle
           ,button-main
           (h1 ((id "titre")) , "Graph")
           ,(infos-html g)
           ;; SVG filled with the data in the graph
           ,html-svg))))

#| ----- node-page ----- |#
  ;; @brief      : displaying a specific node
  ;; @param req  : requests given in the url
  ;; @return     : xexpr?
(define (node-page req)
  (let* ([bindings (request-bindings req)])  
    (if
     ;; too many arguments
     (or (< (length bindings) 1)
         (>= (length bindings) 2))
     (error-no-node)
     (let* ([node (cdar bindings)]
            [inside (is-inside g node)])
       (cond
         ;; wrong args : same nodes start and end
         [(equal? #f inside) (error-no-node)]
         [else
          (let* ([n-graph (get-graph g node)]
                [node-html-svg (node-html-svg g n-graph html-svg)])
            ;; retrieve the path with a_star
                 ;; display the path
                 (response/xexpr
                  `(html (head (title "Node page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
                         (body
                          ,button-node
                          ,button-route
                          ,button-distance
                          ,button-cycle
                          ,button-main
                          (h1 ((id "titre")) , (~a "Node : " node))
                          ,(infos-html g)
                          ,form-node
                          ;; SVG filled with the data in the graph
                          ,node-html-svg
                          ))))])))))


#| ----- route-page ----- |#
  ;; @brief      : route between two id given in parameter of the page
  ;; @param req  : requests given in the url
  ;; @return     : xexpr?
(define (route-page req)
  (let* ([bindings (request-bindings req)])  
    (if
     ;; too many arguments
     (or (<= (length bindings) 1)
         (>= (length bindings) 3))
     (error-args)
     (let* ([start (cdar bindings)]
            [end (cdadr bindings)]
            [inside (and (is-inside g start) (is-inside g end))])
       (cond
         [(equal? #f inside) (error-args)]
         ;; wrong args : same nodes start and end
         [(string=? start end) (error-args)]
         [else
          (let ([path (find-my-way g start end)])
            (cond
              ;; no path between both ID
              [(zero? (length path)) (error-no-path req)]
              ;; retrieve the path with a_star
              [else (let ([path-html-svg (add-path-svg g path html-svg)])
                      ;; display the path
                      (response/xexpr
                       `(html (head (title "Route page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
                              (body
                               ,button-node
                               ,button-route
                               ,button-distance
                               ,button-cycle
                               ,button-main
                               (h1 ((id "titre")) , (~a "Route between node : " start " and node : " end))
                               (h1 ((id "path")) , (~a "Path : " path))
                               (h1 ((id "nb-path")) , (~a "Length of the path : " (length path)))
                               ,(infos-html g)
                               ,form-route
                               ;; SVG filled with the data in the graph
                               ,path-html-svg))))]))])))))

#| ----- distance-page ----- |#
  ;; @brief      : display the distance between 2 nodes in a connected component
  ;; @param req  : requests given in the url
  ;; @return     : xexpr?
(define (distance-page req)
  (let* ([bindings (request-bindings req)])  
    (if
     ;; too many arguments
     (or (<= (length bindings) 1)
         (>= (length bindings) 3))
     (error-dist)
     (let* ([start (cdar bindings)]
            [end (cdadr bindings)]
            [inside (and (is-inside g start) (is-inside g end))])
       (cond
         [(equal? #f inside) (error-dist)]
         ;; wrong args : same nodes start and end
         [(string=? start end) (error-dist)]
         [else
          (let ([path (find-my-way g start end)]
                [d (distance g start end)])
            (cond
              ;; wrong args : same nodes start and end
              [(string=? start end) (error-args)]
              ;; no path between both ID
              [(zero? (length path)) (error-no-path req)]
              [else (let ([path-html-svg (add-path-svg g path html-svg)])
                      ;; display the path
                      (response/xexpr
                       `(html (head (title "Distance page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
                              (body
                               ,button-node
                               ,button-route
                               ,button-distance
                               ,button-cycle
                               ,button-main
                               (h1 ((id "titre")) , (~a "Route between node : " start " and node : " end))
                               (h2 ((id "path")) , (~a "Path : " path))
                               (h2 ((id "nb-path")) , (~a "Length of the path : " (length path)))
                               (h2 ((id "distance")) , (~a "Distance : " d " meters"))
                               ,(infos-html g)
                               ,form-distance
                               ;; SVG filled with the data in the graph
                               ,path-html-svg))))]))])))))

#| ----- cycle-page ----- |#
  ;; @brief      : route between two id given in parameter of the page
  ;; @param req  : requests given in the url
  ;; @return     : xexpr?
(define (cycle-page req)
  (let* ([bindings (request-bindings req)]
         [cities (remove-duplicates (get-cities bindings))]
         [all-inside (foldl (lambda(x y) (and x y)) #t (nodes-inside g cities))])
    (cond
      [(null? cities) (error-cycle)]
      [(equal? #f all-inside) (error-cycle)]
      [else (let ([cycle (nearest g cities)])
              (cond
                ;; no path between both ID
                [(null? cycle) (error-no-cycle cities)]
                ;; retrieve the path with a_star
                [else (let ([path-html-svg (add-path-svg g cycle html-svg)])
                        ;; display the path
                        (response/xexpr
                         `(html (head (title "Route page")  (link ((rel "icon") (href "https://upload.wikimedia.org/wikipedia/commons/7/71/Earth_icon_2.png") (type "image/png"))))
                            (body
                             ,button-node
                             ,button-route
                             ,button-distance
                             ,button-cycle
                             ,button-main
                             (h1 ((id "titre")) , (~a "Cycle between nodes : " cities))
                             (h1 ((id "path")) , (~a "Cycle : " cycle))
                             ,(infos-html g)
                             ,form-cycle
                             ;; SVG filled with the data in the graph
                             ,path-html-svg
                             ))))]))])))
    
;; -------------------------------------------------
;; Table of association between pages and functions

;; Routing function
;;     /route            --->   road-page
;;     /node             --->   node-page
;;     /distance         --->   distance-page
;;     /cycle            --->   cycle-page
;;     everything else   --->   main-page
(define-values (server-dispatch server-url)
    (dispatch-rules
     [("node") node-page]
     [("route") route-page]
     [("distance") distance-page]
     [("cycle") cycle-page]
     [else main-page]))

;; -------------------------------------------------

;; Starting the server
(serve/servlet server-dispatch
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)

