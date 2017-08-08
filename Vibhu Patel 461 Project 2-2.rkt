;Vibhu Patel
;CMPSC 461
;vxc5074

;1a
(define (maxInt_helper number x)
  (if (null? x) number
  (if(> number (car x)) (maxInt_helper number (cdr x))
  (maxInt_helper (car x) (cdr x)))))
;1b
(define (maxInt x)
  (if (null? x) 0
      (maxInt_helper (car x) (cdr x))))
;2
(define (even a)
  (if (null? a) 0
  (+ 1 (even (cdr a)))))                                    

(define (zip a b)
(if (= (even a) (even b))
      (if (null? a) '()  (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))) 'error))

;3
(define (compute l a)
  (if (null? l) 0
      (* (+ (/ (car l) a) (compute  (cdr l) a)) a)))

;4

(define (input a)
  (cond ((= 1 a) 'one)
        ((= 2 a) 'two)
        ((= 3 a) 'three)
        ((= 4 a) 'four)
        ((= 5 a) 'five)
        ((= 6 a) 'six)
        ((= 7 a) 'seven)
        ((= 8 a) 'eight)
        ((= 9 a) 'nine)
        ((= 0 a) 'zero)))

(define (upper a)
(cond ((= 2 a) 'twenty)        
        ((= 3 a) 'thirty)
        ((= 4 a) 'fourty)
        ((= 5 a) 'fifty)
        ((= 6 a) 'sixty)
        ((= 7 a) 'seventy)
        ((= 8 a) 'eighty)
        ((= 9 a) 'ninety)))

(define (tens a)
  (cond ((= 1 a) 'eleven)
        ((= 2 a) 'twelve)
        ((= 3 a) 'thirteen)
        ((= 4 a) 'fourteen)
        ((= 5 a) 'fifteen)
        ((= 6 a) 'sixteen)
        ((= 7 a) 'seventeen)
        ((= 8 a) 'eightenn)
        ((= 9 a) 'nineteen)
        ((= 0 a) 'ten)))
        
(define (int-to-words a)
  (cond ((= (quotient a 10) 0) (list (input a)))
        ((= (quotient a 10) 1) (list (tens (remainder a 10))))
        ((> (quotient a 10) 1) (if (= (remainder a (* 10 (quotient a 10))) 0) (list (upper (quotient a 10))) (list (upper (quotient a 10))(input (remainder a (* 10 (quotient a 10)))))))))

; 5a
(define (initialWCList l) 
  (if (null? l) '()
      (cons (list (car l) 1) (initialWCList (cdr l)))))

; 5b
(define (mergeWC k l) 
  (if (null? l) (list k) 
      (if (null? k) '() 
          (if (equal? (car k) (caar l)) (cons ( list (caar l) (+ (cadar l) (cadr k)) ) (cdr l)) 
              (cons(car l) (mergeWC k (cdr l))) 
              ))))
            
 ;5c
(define (mergeByWord l)
 (if (null? l) '()
     (mergeWC (car l) (mergeByWord (cdr l)))))

; 5d
(define (wordcount l)
  (if (null? l) 'EmptyList
      (mergeByWord (initialWCList l))))

