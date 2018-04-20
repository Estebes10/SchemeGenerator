#lang racket

(let ((p (open-input-file "es/animals.txt")))
  (let f ((x (read p)))
    (if (eof-object? x)
        (begin
          (close-input-port p)
          '())
        (cons x (f (read p))))))



