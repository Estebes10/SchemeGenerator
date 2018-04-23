#lang racket
(require "get-values.rkt")
(provide search-element)
(provide random-element)
(provide get-content)
(provide get-path)

;; Build path from input readed
(define (build-paths langu name)
  (string-append langu "/" name))

(define (get-languaje-catalog langu type)
  (cond
    [(string=? (symbol->string langu) "es") "es"]
    [else "en"]))


(define (select-folder langu type)
  (cond
    [(string=? type "bank-name") "global"]
    [(string=? type "car-brand") "global"]
    [else (get-languaje-catalog langu type)]))

;; get english catalog paths
(define (get-english-catalog type)
  (cond
    [(string=? type "animal-name") "en/animals.txt"]
    [(string=? type "men-name") "en/men-name.txt"]
    [(string=? type "women-name") "en/women-name.txt"]
    [(string=? type "lastname") "en/lastname.txt"]))

;; Get spanish catalog paths
(define (get-spanish-catalog type)
  (cond
    [(string=?  type "animal-name") "es/animals.txt"]
    [(string=?  type "men-name") "es/men-name.txt"]
    [(string=?  type "women-name") "es/women-name.txt"]
    [(string=?  type "lastname") "es/lastname.txt"]))

;; Method to use to separate languajes catalog with global catalog
(define (get-path langu type)
  (cond
    [(string=?  type "bank-name") "global/bank-names.txt"]
    [(string=?  type "car-brand") "global/car-brand.txt"]
    [(string=? langu "es")(get-spanish-catalog type)]
    [(string=? langu "en")(get-english-catalog type)]))


;; find a elemento in list
(define (search-element element list)
  (let* ( (x list)) element))

;; return content of catalogs
(define (get-content file)
(let ((p (open-input-file file)))
  (let f ((x (read p)))
    (if (eof-object? x)
        (begin
          (close-input-port p)
          '())
        (cons x (f (read p)))))))

;; get a random element on the catalog
(define (random-element list)
  (symbol->string (list-ref list (random (length list)))))
