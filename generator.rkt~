#lang racket
(require "get-values.rkt")
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
                                  (random-element
                                   (get-content
                                    (get-path
                                     (symbol->string languaje)
                                     (symbol->string (car cols)))))
                                  "\""
                                  ",") out)
          (fill (cdr cols) rowss)]))

;; Define messages to print after execution
(define (successful file)
  (printf "Creating file...\n")
  (display (string-append "The file: " file " was created successfully\n")))

(define (wrong file)
  (printf "Creating file...\n")
  (display (string-append "The file: " file " could not be created, try again\n")))

;; Check execution
(define execute
  (cond
    [(and (write-header-csv column-names)(fill types-names rows)) (successful (symbol->string filename))]
    [else (wrong (symbol->string filename))]))

;; find column
(define (find-column attribute list)
  (search-element attribute list))

;; save column in type of columns
;(define column (find-column attribute types-names))

;; return pathname of catalogs
;;(define pathname (get-path (symbol->string languaje) (symbol->string column)))

;; save catalog from directories
;;(define catalog (get-content pathname))
