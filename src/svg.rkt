#lang racket

#| ----- Requirement ----- |#
(require "parsing.rkt")
(require "graph.rkt")
(require "graph_construction.rkt")

(require xml)

#| ----- Provided functions ----- |#
(provide graph-to-svg add-path-svg node-html-svg)

#| ----- set-bounds ----- |#
  ;; @brief          : set bounds for top level using
  ;; @param bounds   : list? of bounds in order : (maxLat minLat maxLon minLon)
(define (set-bounds bounds)
  (begin
    (set! max-lat (car bounds))
    (set! min-lat (cadr bounds))
    (set! max-lon (caddr bounds))
    (set! min-lon (cadddr bounds))))

;; Define box size (BE CAREFUL : IF YOU WANT TO CHANGE, YOU MUST CHANGE VIEWBOX IN SVG FORMAT TOO
(define width_box
  "1280")
(define height_box
  "720")
(define box
  " 0 0 1280 720")

;; Create empty hash
(define g (make-graph))

;; Top level bounds
;; get the bounds for each latitude and longitude
(define max-lat
  "-90")
(define max-lon
  "-180")
(define min-lat
  "90")
(define min-lon
  "180")

;; Resize formulas :
;;(coefWidthBox - [(x-lat - latMin) / (latMax - latMin) * (coefBoxWidth - offsetWidth)] ) + (- offsetWidth / 2)

;;(y-lon - lonMin) / (lonMax - lonMin) * coefBoxHeight

;; Offset for the visualization
(define offset-width (* (string->number width_box) 0.05))
(define offset-height (* (string->number height_box) 0.05))
#| ----- lat-scaled ----- |#
  ;; @brief     : resize latitude in bounds
  ;; @param n   : node?
  ;; @return    : string?
(define (lat-scaled n)
  (number->string (+ (- 0 (/ offset-width 2))
                     (- (string->number height_box)
                        (* (/ (- (string->number (n-lat n)) min-lat)
                              (- max-lat min-lat))
                           (- (string->number height_box)
                              offset-width))))))
  
#| ----- lon-scaled ----- |#
  ;; @brief     : resize latitude in bounds
  ;; @param n   : node?
  ;; @return    : string?
(define (lon-scaled n)
  (number->string  (+ (- 0 (/ offset-height 2))
                      (- (string->number width_box)
                         (* (/ (- (- max-lon min-lon)
                                  (- (string->number (n-lon n)) min-lon))
                               (- max-lon min-lon))
                            (- (string->number width_box)
                               offset-height))))))


#| ----- graph-to-svg ----- |#
  ;; @brief       : create the SVG container (list) for the server and format it by adding circles and lines
  ;; @param graph : graph?
  ;; @param max-lat_ : string?
  ;; @param min-lat_ : string?
  ;; @param max-lon_ : string?
  ;; @param min-lon_ : string?
  ;; @return      : list?
(define (graph-to-svg graph bounds)
  (begin
    ;; set the top level bounds
    (set-bounds bounds)
    ;; set the graph top level
    (set! g graph)
    (let*
        ;; add all nodes from the graph to the list
        ([circles (hash-map graph create-circle)]
         ;; add lines (and clean empty ones)
         [lines (reformat-lines (sort-empty-lines (hash-map graph create-line)))]
         ;; fill the svg with circles and lines
         [circles-lines (append circles lines)]
         ;; add the format for the svg
         [final-svg (append svg-format circles-lines)])
    final-svg)))

#| ----- create-circle ----- |#
  ;; @brief     : return a circle formatted for svg container (with all attributes)
  ;; @param id  : string? id of the node
  ;; @param n   : node?
  ;; @return    : list?
(define (create-circle id n)
  `(circle ((cx ,(lon-scaled n))
            (cy ,(lat-scaled n))
            (r "3")
            (fill "#8b904b"))))

#| ----- create-line ----- |#
;; @brief     : return a line formatted for svg container (with all attributes)
;; @param n   : node?
;; @return    : list?
(define (create-line id n)
  (cond
    [(empty? (n-neighbour n)) '()]
    [else (create-line-rec n (n-neighbour n))]))

#| ----- create-line-rec ----- |#
  ;; @brief     : for each neighbours of a given node, create line for SVG container
  ;; @param n   : node?
  ;; @param l   : listof node?
  ;; @return    : list?
(define (create-line-rec n l)
  (cond
    ;; ending case
    [(empty? (cdr l)) `((line ((x1 ,(lon-scaled n))
                               (y1 ,(lat-scaled n))
                               (x2 ,(lon-scaled (get-graph g (car l))))
                               (y2 ,(lat-scaled (get-graph g (car l))))
                               (style "stroke:rgb(112,128,144);stroke-width:1.5"))))]
    ;; recursive on neighbours
    [else (append`((line ((x1 ,(lon-scaled n))
                          (y1 ,(lat-scaled n))
                          (x2 ,(lon-scaled (get-graph g (car l))))
                          (y2 ,(lat-scaled (get-graph g (car l))))
                          (style "stroke:rgb(112,128,144);stroke-width:1.5"))))
                 (create-line-rec n (cdr l)))]))


#| ----- svg-format ----- |#
  ;; @brief     : add the format for svg container
  ;; @param l   : list?
  ;; @return    : list?
(define svg-format
    `(svg ((width ,width_box) (height ,height_box) (viewBox ,box)(xmlns "http://www.w3.org/2000/svg"))))

#| ----- sort-empty-lines ----- |#
  ;; @brief     : remove all empty elements in lines list
  ;; @param l   : list?
  ;; @return    : list?
(define (sort-empty-lines l)
  (cond
    [(empty? l) l]
    [(empty? (car l)) (sort-empty-lines (cdr l))]
    [else (cons (car l) (sort-empty-lines (cdr l)))]))

#| ----- reformat-lines ----- |#
  ;; @brief     : transform lines into a good format for SVG
  ;; @param l   : list?
  ;; @return    : list?
(define (reformat-lines l)
  (cond
    [(empty? l) l]
    [else (append (car l) (reformat-lines (cdr l)))]))

#| ----- add-path-svg ----- |#
  ;; @brief     : return a list of lines for svg container in order to add it in SVG container to display the path
  ;; @param g   : graph?
  ;; @param p   : listof? string path with nodes id
  ;; @param html: previous html-svg container
  ;; @return    : list? svg container with the path in red
(define (add-path-svg g p html)
  (let ([lines (lines-from-path-rec g (get-graph g (car p)) (cdr p))])
    (append html lines)))

#| ----- node-html-svg ----- |#
  ;; @brief     : add a specific node into the existing svg
  ;; @param g   : graph?
  ;; @param n   : node?
  ;; @param html: previous html-svg container
  ;; @return    : list? svg container with the path in red
(define (node-html-svg g n html)
    (append html (list`(circle ((cx ,(lon-scaled n))
                           (cy ,(lat-scaled n))
                           (r "6")
                           (fill "#FF0000"))))))

#| ----- lines-from-path-rec ----- |#
  ;; @brief     : create a listof? string for svg container for the path
  ;; @param g   : graph?
  ;; @param n   : node? 
  ;; @param p   : list? of string path
  ;; @return    : list? of lines (string)
(define (lines-from-path-rec g n p)
  (cond
    ;; ending case
    [(empty? (cdr p)) `((line ((x1 ,(lon-scaled n))
                               (y1 ,(lat-scaled n))
                               (x2 ,(lon-scaled (get-graph g (car p))))
                               (y2 ,(lat-scaled (get-graph g (car p))))
                               (style "stroke:rgb(100,41,38);stroke-width:2.5"))))]
    [else (append`((line ((x1 ,(lon-scaled n))
                          (y1 ,(lat-scaled n))
                          (x2 ,(lon-scaled (get-graph g (car p))))
                          (y2 ,(lat-scaled (get-graph g (car p))))
                          (style "stroke:rgb(100,41,38);stroke-width:2.5"))))
                 (lines-from-path-rec g (get-graph g (car p)) (cdr p)))]))


#|
;; FOR TESTS
(define maps (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/little.osm")))))
(define gr (osm-to-full-sorted maps))
(graph-to-svg gr '(44.7754213 44.7216967 -0.7988466 -0.8587464))

circles
(svg-format)
(append  (svg-format)circles)
|#
