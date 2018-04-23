#lang racket
(require "generator.rkt")
(require "get-catalogs.rkt")

;; open file to write
(define out (open-output-file (symbol->string filename)))

;;(display "Problem?" out)
;; (newline out)
;;(close-output-port out)

;; get atribute
(define (get-attribute list)
  (cond
    [(empty? list) ""]
    [else (car list)]))

;; method to write column headers
(define (write-header-csv cols)
  (cond
    [(empty? cols) (newline out)]
    [else (display (string-append "\"" (symbol->string (car cols)) "\"" ",") out)
          (write-header-csv (cdr cols))]))

(define (write-columns-csv cols)
  (cond
    [(empty? cols) (newline out)  (close-output-port out)]
    [else (display (string-append "\"" (symbol->string (car cols)) "\"" ",") out)
          (write-columns-csv (cdr cols))]))

;; method to fill csv data
(define (fill cols rowss)
  (cond
    [(= 0 rowss) (close-output-port out)]
    [(empty? cols)(newline out)(fill types-names (- rowss 1))]
    [else (display (string-append "\""
                                  (random-element (get-content (get-path (symbol->string languaje)(symbol->string (car cols)))))
                                  "\""
                                  ",") out)
          (fill (cdr cols) rowss)]))

;; method to write data headers
(define write-headers (write-header-csv column-names))
(define write-columns (fill types-names rows))

;(define attribute (get-attribute list))

;; find column
(define (find-column attribute list)
  (search-element attribute list))

;; save column in type of columns
;(define column (find-column attribute types-names))

;; return pathname of catalogs
;;(define pathname (get-path (symbol->string languaje) (symbol->string column)))

;; save catalog from directories
;;(define catalog (get-content pathname))
