#lang racket

#| ----- Functions provided anywhere -----
    *  nodes
    *  id_way
    *  way
    *  key
    *  filt
    *  filt2
    *  not-empty
|#

(provide nodes id_way way key filt filt2 filt2-all-way not-empty)

#| ----- nodes ----- |#
  ;; @brief           : ..
  ;; @param a         : listof string?
  ;; @return          : boolean?
(define (nodes a)
  (if (list? a)
      ;; a is a list
      (cond
        ;; a has node param : true
        [(string=? (symbol->string (car a)) "id") #t]
        [(string=? (symbol->string (car a)) "lat") #t]
        [(string=? (symbol->string (car a)) "lon") #t]
        ;; else : false
        [else #f])
      ;; If the argument is not a list
      (string=? (symbol->string a) "node")))

#| ----- id_way ----- |#
  ;; @brief           : verify the id_way
  ;; @param l         : listof string?
  ;; @return          : boolean?
(define (id_way l)
  (if (list? l)
      ;; l is a list
      (cond
        ;; if the first tags of l is the id then its a id_way
        [(string=? (symbol->string (car l)) "id") #t]
        [else #f])
      #f))

#| ----- way ----- |#
  ;; @brief           : verify if l is a way
  ;; @param l         : listof string?
  ;; @return          : boolean?
(define (way l)
  (if (list? l)
      ;; l is a list
      (cond
        ;; If the first tag of l is a node (which means the first linked node in the way) then its a way
        [(string=? (symbol->string (car l)) "nd") #t]
        [else #f])
      #f))

#| ----- key ----- |#
  ;; @brief           : verify if the key of l is good (highway?)
  ;; @param l         : listof string?
  ;; @return          : boolean?
(define (key l)
  (if (and (list? l)
           (list? (cdr l)))
      ;; If l is a list
      (cond
        ;; If the first tag is "tag" AND its an highway (at the moment, we only use highways
        [(and (string=? (symbol->string (car l)) "tag")
              (string=? (cadr (caadr l)) "highway")) #t]
        [else #f])
     #f))

#| ----- kway ----- |#
  ;; @brief           : add node ref and type of highway
  ;; @param l         : listof string?
  ;; @param l1        : listof string?
  ;; @return          : listof string?
(define (kway l l1)
  (if (and (list? l)
           (list? (cdr l)))
      ;; If l is a list
      (cond
        ;; If the first tag of l is a node (which means the first linked node in the way) then its a way
        [(string=? (symbol->string (car l)) "nd") (append l1 (list (caadr l)))]
        ;; If the first tag is "tag" AND its an highway (at the moment, we only use highways
        [(and (string=? (symbol->string (car l)) "tag")
              (string=? (cadr (caadr l)) "highway")) (append l1 (list (cadadr l)))]
        [else l1])
     l1))



#| ----- filt ----- |#
  ;; @brief           : filter l and get back the list without useless infos
  ;; @param l         : listof string?
  ;; @return          : boolean?
(define (filt l)
  (if (list? l)
      ;; If l is a list
      (cond
        ;; If the first element of l is :
        ;; node
        [(string=? (symbol->string (car l)) "node") (append '(node) (filter nodes (cadr l)))]
        ;; way
        [(and (string=? (symbol->string (car l)) "way")
              (not (empty? (filter key (cddr l))))) (append '(way)
                                                            (filter id_way (cadr l))
                                                            (map caadr (filter way (cddr l)))
                                                            (map cadadr (filter key (cddr l))))]
        ;; anything else (for now only treating nodes and ways)
        [else '()])
      ;; l is not a list
      '()))

#| ----- filt2 ----- |#
  ;; @brief           : filter l and get back the list without useless infos
  ;; @param l         : listof string?
  ;; @return          : listof string?
(define (filt2 l l1)
  (if (list? l)
      ;; If l is a list
      (cond
        ;; If the first element of l is :
        ;; node
        [(string=? (symbol->string (car l)) "node") (append l1 (list (append '(node) (filter nodes (cadr l)))))]
        ;; way
        [(and (string=? (symbol->string (car l)) "way")
              (not (empty? (filter key (cddr l))))) (append l1 (list (append '(way)
                                                            (filter id_way (cadr l))
                                                            (foldl kway '() (cddr l)))))]
        ;; anything else (for now only treating nodes and ways)
        [else l1])
      ;; l is not a list
      l1))

#| ----- filt2-all-way ----- |#
  ;; @brief           : filter l and get back the list without useless infos but with every way
  ;; @param l         : listof string?
  ;; @param l1        : listof string?
  ;; @return          : listof string?
(define (filt2-all-way l l1)
  (if (list? l)
      ;; If l is a list
      (cond
        ;; If the first element of l is :
        ;; node
        [(string=? (symbol->string (car l)) "node") (append l1 (list (append '(node) (filter nodes (cadr l)))))]
        ;; way
        [(and (string=? (symbol->string (car l)) "way"))
         (append l1 (list (append '(way)
                                  (filter id_way (cadr l))
                                  (foldl kway '() (cddr l)))))]
        ;; anything else (for now only treating nodes and ways)
        [else l1])
      ;; l is not a list
      l1))
      

#| ----- not-empty ----- |#
  ;; @brief           : filter l and get back the list without useless infos
  ;; @param list      : listof string?
  ;; @return          : boolean?
(define (not-empty list)
  (not (empty? list)))
