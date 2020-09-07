#lang racket

(require "graph.rkt" )
(require racket/contract)

(provide (contract-out
          [distance-cal (-> integer? (-> node? node? number?))]
          [distance-m (-> node? node? number?)]
          [distance-km (-> node?  node? number?)] ;; Faut parler de list
          ))


;;fonction qui retourne une lambda qui calcule la distance suivant une échelle
;;avec la formule de Haversine entre 2 noeuds 
(define distance-cal
  (lambda(ref_in_m)
    (lambda(node1 node2)
      (/
       (* 2
          6371000
          (asin
           (sqrt
            (+
             (sqr (sin (/ (- (degrees->radians (string->number (n-lat node2))) (degrees->radians (string->number (n-lat node1)))) 2)))
             (* (cos (degrees->radians (string->number (n-lat node1))))
                (cos (degrees->radians (string->number (n-lat node2))))
                (sqr (sin (/ (- (degrees->radians (string->number (n-lon node2))) (degrees->radians (string->number (n-lon node1)))) 2))))))))
      ref_in_m))))

(define distance-m ((curry distance-cal) 1));;fonction qui retourne la distance en m entre 2 noeuds
(define distance-km ((curry distance-cal) 1000));;fonction qui retourne la distance en km entre 2 noeuds

;(define test1 (convert-to-node '(node (id "415330686") (lat "44.779116860189234") (lon "-0.6379185809690853"))))
;(define test2 (convert-to-node '(node (id "315330686") (lat "44.76741807828804") (lon "-0.5720006122190853"))))
;(distance-m test1 test2)
;(distance-km test1 test2)
