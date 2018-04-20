#lang racket

(provide languaje)
(provide columns)
(provide column-names)
(provide types-names)

;; Receive from user:
;; input: ((column-name type, column-name2 type), languaje, number, filename)

;; Example
;; ((name animals, lastname lastname), es-mx, 10, animals.csv)
;; Output -> csv file called "animals.csv"


;; Method to display how to use this program
(define how-to-use
  (display
   (string-append "How to use: \n"
                  "(((column-name type)(column-name2 type)) languaje rows csv-output-name))\n\n"
                  "Example: \n"
                  "(((name men-name)(lastname lastname)) es 10 user.csv)\n\n"
                  "(((col1 type1)(col2 type2)(col3 type3)) es 10 user.csv)\n\n")))

;; Get the input given by user
(define input (read))

;; save parameters that will be used to generate columns in csv file
(define columns (car input))

;; save parameters of languaje, rows and filename
(define parameters (cdr input))

;; save languaje
(define languaje (car parameters))

;; save rows
(define rows (car (cdr parameters)))

;; save filename
(define filename (car (cdr (cdr parameters))))

;; method to count if a list contains pairs (column-name type)
(define (two-in-list? list)
  (cond
    [(symbol? list) #f]
    [(empty? list) #f]
    [(empty? (cdr list)) #f]
    [(empty? (cdr (cdr list))) #t]
    [else #f]))

;; get total of columns for csv
(define total-colums (length columns))

;; method to get names
(define (get-names list)
  (cond
    [(empty? list) empty]
    [(and (symbol? (car list))(not (empty? (cdr list))))(cons (car list) empty)]
    [else (append (get-names (car list))(get-names (cdr list)))]))

;; method to get types
(define (get-types list)
  (cond
    [(empty? list) empty]
    [(and (symbol? (car list))(empty? (cdr list)))(cons (car list) empty)]
    [(and (symbol? (car list))(not (empty? (cdr list))))(get-types (cdr list))]
    [else (append (get-types (car list))(get-types (cdr list)))]))

;; get column names
(define column-names (get-names columns))

;; get types names
(define types-names (get-types columns))
