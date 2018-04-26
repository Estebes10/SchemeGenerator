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
    [(string=? type "Animal::Name") "languages/en/animals.txt"]
    [(string=? type "Name::Men") "languages/en/men-name.txt"]
    [(string=? type "Name::Women") "languages/en/women-name.txt"]
    [(string=? type "Name::Lastname") "languages/en/lastname.txt"]))

;; Get spanish catalog paths
(define (get-spanish-catalog type)
  (cond
    [(string=?  type "Animal::Name") "languages/es/animals.txt"]
    [(string=?  type "Name::Men") "languages/es/men-name.txt"]
    [(string=?  type "Name::Women") "languages/es/women-name.txt"]
    [(string=?  type "Name::Lastname") "languages/es/lastname.txt"]
    [(string=?  type "Mexico::Address::State-Code") "languages/es/mexico/address/state-code.txt"]
    [(string=?  type "Mexico::Address::State") "languages/es/mexico/address/state.txt"]
    [(string=?  type "Mexico::Address::City") "languages/es/mexico/address/city.txt"]))

;; Method to use to separate languajes catalog with global catalog
(define (get-path langu type)
  (cond
    [(string=?  type "Bank::Name") "global/bank-names.txt"]
    [(string=?  type "Car::Brand") "global/car-brand.txt"]
    [(string=?  type "Famous::Name") "global/famous-people.txt"]
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
  (list-ref list (random (length list))))
