#lang racket

(define less-than-ten
  (list "zero" "um" "dois" "tres" "quatro" "cinco" "seis" "sete" "oito" "nove"))

(define ten-to-nineteen
  (list "dez" "onze" "doze" "treze" "quatorze" "quinze" "dezesseis" "dezesete" "dezoito" "dezenove")) 

(define dozens
  (list "vinte" "trinta" "quarenta" "cinquenta" "sessenta" "setenta" "oitenta" "noventa"))

(define hundreds
  (list "cem" "cento" "duzentos" "trezentos" "quatrocentos" "quinhentos" "seiscentos" "setecentos" "oitocentos" "novecentos"))

(define thousands
  (list (list "" "")
        (list "mil" "mil")
        (list "milhão" "milhões")
        (list "bilhão" "bilhões")
        (list "trilhão" "trilhões")
        (list "quatrilhão" "quatrilhões")
        (list "quintilhão" "quintilhões")
        (list "sextilhão" "sextilhões")
        (list "septilhão" "septilhões")
        (list "octilhão" "octilhões")
        (list "nonilhão" "nonilhões")
        (list "decilhão" "decilhões")))

(define (get-string-until-99 n)
  (cond [(> 10 n) (list-ref less-than-ten n)]
        [(> 20 n) (list-ref ten-to-nineteen (- n 10))]
        [(let* ([unit (remainder n 10)]
                [unit-string (list-ref less-than-ten unit)]
                [dozen (- n unit)]
                [dozen-string (list-ref dozens (- (/ dozen 10) 2))])
           (if (zero? unit)
               dozen-string
               (string-append dozen-string " e " unit-string)))]))

(define (get-string-less-than-thousand n)
  (cond [(> 100 n) (get-string-until-99 n)]
        [(= n 100) (car hundreds)]
        [#t (let* ([dozens-and-units (remainder n 100)]
                [hundreds-number (- n dozens-and-units)]
                [hundreds-string (list-ref hundreds (/ hundreds-number 100))])
              (if (zero? dozens-and-units)
                  hundreds-string
                  (string-append hundreds-string
                                 " e "
                                 (get-string-until-99 dozens-and-units))))]))

(define (get-string-for-number n)
  (letrec ([generate-thousand-string (lambda (n x)
                                       (let ([thousand-complement (if (= n 1)
                                                                      (car (list-ref thousands x))
                                                                      (car (cdr (list-ref thousands x))))])
                                         (cond [(zero? n) ""]
                                               [#t (string-append (get-string-less-than-thousand n)
                                                                       " "
                                                                       thousand-complement)])))]
           [f (lambda (n x)
                (let* ([less-than-thousand (remainder n 1000)]
                       [next-thousand (- n less-than-thousand)])
                          (cond [(zero? next-thousand) (generate-thousand-string less-than-thousand x)]
                                [(zero? less-than-thousand) (f (/ next-thousand 1000) (+ x 1))]
                                [#t (string-append (f (/ next-thousand 1000) (+ x 1))
                                                   ", "
                                                   (generate-thousand-string less-than-thousand x))])))])
           (f n 0)))