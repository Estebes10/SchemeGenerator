#lang racket
;;    SchemeGenerator is a small program to generate data to populate databases
;;    Copyright (C) 2018  Juan Carlos Estebes González

;;    This program is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;   You should have received a copy of the GNU General Public License
;;   along with this program.  If not, see <https://www.gnu.org/licenses/>.


(provide languaje)
(provide filename)
(provide columns)
(provide column-names)
(provide types-names)
(provide total-columns)
(provide rows)

;; Receive from user:
;; input: ((column-name type, column-name2 type), languaje, number, filename)

;; EXAMPLE
;; ((name animals, lastname lastname), es-mx, 10, animals.csv)
;; Output -> csv file called "animals.csv"

;; get list of languajes available for SchemeGenerator
(define available-languajes
  (directory-list "languajes"))

(define (display-languajes list)
  (cond
    [(empty? list) (display "\n\n")]
    [else (display (string-append (path->string (car list)) ", "))(display-languajes (cdr list))]))

;; Method to display how to use this program
(define how-to-use
  (display
   (string-append "SchemeGenerator is a small program to generate data to populate databases
    Copyright (C) 2018  Juan Carlos Estebes González

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.\n\n"
                  "How to use: \n"
                  "(((column-name type)(column-name2 type)) languaje rows csv-output-name))\n\n"
                  "Where:\n"
                  "((column-name type)(column-name2 type)): are the columns and datatypes for csv\n"
                  "headers and types of data to fill the rows in csv file.\n"
                  "languaje: is the languaje selected to get data in catalogs.\n"
                  "rows: is the number of rows that you want to fill in csv.\n"
                  "csv-output-name: is the name of the csv file containing the data generated\n\n"
                  "Check that the parenthesis are correctly used\n\n"
                  "Example: \n"
                  "(((name men-name)(lastname lastname)) es 10 user.csv)\n\n"
                  "(((man men-name)(women women-name)(car car-brand)) es 10 user.csv)\n\n"
                  "Valid types: men-name, women-name, car-brand, lastname, animal-name, bank-name\n\n"
                  "Available languajes: ")))

;; Display guide
(define guide-languajes (display-languajes available-languajes))

;; Get the input given by user
(define input (read))

;; save parameters that will be used to generate columns in csv file
(define columns (car input))

;; save parameters of languaje, rows and filename
(define parameters (cdr input))

;; save languaje if exists catalogs in that languaje
(define languaje
  (cond
    [(directory-exists? (string-append "./languajes/" (symbol->string (car parameters))))(car parameters)]
    [else (raise "Languaje typed does not exist, try with valid values")(exit)]))

;; save rows
(define rows (car (cdr parameters)))

;; save filename if does not exists in current directory
(define filename
  (cond
    [(file-exists? (symbol->string (car (cdr (cdr parameters)))))(raise "The filename exists in your current directory, try with another name for output file")(exit)]
    [else (car (cdr (cdr parameters)))]))

;; method to count if a list contains pairs (column-name type)
(define (two-in-list? list)
  (cond
    [(symbol? list) #f]
    [(empty? list) #f]
    [(empty? (cdr list)) #f]
    [(empty? (cdr (cdr list))) #t]
    [else #f]))

;; get total of columns for csv
(define total-columns (length columns))

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

;; method to get length of lists
(define (my-length lst)
  (cond
   [(empty? lst) 0]
   [else (+ 1 (my-length (rest lst)))]))