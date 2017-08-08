;Vibhu Patel
;CMPSC 461
;vxc5074

;The maxInt function takes a list of numbers, and returns its
;maximum element. For example, (maxInt ’(7 3 6 2)) should return 7.
;If the input list is empty, then it should return 0. Write the maxInt
;function in Scheme in two steps:


;(A) Write a maxInt helper function; it takes a number k
;and a list of numbers, x, as arguments, and returns the number
;which is the largest among k and numbers in x. For example,
;(maxInt helper 5 ’(4 5 6)) should return 6:


(define (maxInt_helper number x)
  (if (null? x) number
  (if(> number (car x)) (maxInt_helper number (cdr x))
  (maxInt_helper (car x) (cdr x)))))
  
  
  
  
;(b) (2 points) Write the maxInt function based on maxInt helper:



(define (maxInt x)
  (if (null? x) 0
      (maxInt_helper (car x) (cdr x))))
      
      
      
;Define a zip function in Scheme. It takes two lists and
;returns a list of pairs of items in the two input lists. For instance,
;(zip ’(1 3 9) ’(2 4 8)) should return the list ((1 2) (3 4) (9
;8)). Your function should return ’error when the two input lists are
;not of the same length:


(define (even a)
  (if (null? a) 0
  (+ 1 (even (cdr a)))))                                    

(define (zip a b)
(if (= (even a) (even b))
      (if (null? a) '()  (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))) 'error))
      
      
      
      
;A single-variable polynomial of degree n is written as
;a0 + a1x + a2x 2 + . . . + anxn
;where an, ..., a0 are coefficients. Suppose we represent such a polynomial
;as a list (a0, a1, a2, . . . , an).
;Write a Scheme function compute, which takes a list that represents a
;polynomial and a value for x and computes the value for the polynomial.
;For instance, when given input polynomial (1, 2, 3) and 2, then
;the function should return 17, because 1 + 2 ∗ 2 + 3 ∗ 2^2 = 17
;Hint: You should figure out how to calculate the result of (compute l x) from combining “car l” and “(compute (cdr l) x)”

(define (compute l a)
  (if (null? l) 0
      (* (+ (/ (car l) a) (compute  (cdr l) a)) a)))


;Write a Scheme function int-to-words that takes a nonnegative
;integer between 0 and 99, inclusive, as a parameter and returns
;a list of words corresponding to the integers reading in English. Make
;your function as short as possible; i.e., don’t write a COND statement
;with 100 tests. Example calls to your function are given below:
;(int-to-words 13) ; should return (thirteen)
;(int-to-words 42) ; should return (forty two)
;Note, in order to simplify things, we are not expecting dashes in the
;output (i.e., 42 is output as forty two not forty-two). Hint: You
;may want to use the built-in integer division functions quotient and
;remainder

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


;In this question, we are going to write a Scheme program
;that calculates word counts. The input is a list of words and the output
;should be counts for words that appear in the input list. For example,
;if the input is
;’(time is long but life is short)
;Then one output can be
;((short 1) (is 2) (life 1) (but 1) (long 1) (time 1))
;Note that you are not asked to sort the words in the output. Therefore,
;the output is correct as long as the counts are correct.
;Do the following steps to implement the word-count program. In our
;discussion, we call a word with a count a word-count pair, for example,
;(short 1) and (is 2) are word-count pairs. We call a list of wordcount
;pairs a word-count list


; (a) Write a function initialWCList that takes a list of
;words and creates a word-count list. The resulting word-count
;list should have the word count 1 for every word. Use the map
;function we discussed in class to implement initialWCList. For
;instance, 2 (initialWCList ’(time is long but life is short))
;should generate
;((time 1) (is 1) (long 1) (but 1) (life 1) (is 1) (short 1))



(define (initialWCList l) 
  (if (null? l) '()
      (cons (list (car l) 1) (initialWCList (cdr l)))))
      
      
;(B) Write a function mergeWC. It takes two inputs. The
;first is a word-count pair and the second is a word-count list.
;This function generates a new word-count list. If the input wordcount
;pair has a corresponding pair in the word-count list with
;the same word, then the counts should be merged; otherwise, the
;output word-count list should have the input word-count list with
;the word-count pair at the end of the list. For instance,
;(mergeWC ’(is 1) ’((time 1) (is 1))) should generate ((time 1) (is 2)) As another example
;(mergeWC ’(life 1) ’((time 1) (is 2))) should generate ((time 1) (is 2) (life 1))



(define (mergeWC k l) 
  (if (null? l) (list k) 
      (if (null? k) '() 
          (if (equal? (car k) (caar l)) (cons ( list (caar l) (+ (cadar l) (cadr k)) ) (cdr l)) 
              (cons(car l) (mergeWC k (cdr l))) 
              ))))
              
              
;(C) Write a function mergeByWord, which takes a wordcount list and produces a new word-count list; the output wordcount
;list should have one word-count pair for each word that appears in the input list and the count should be the sum of all
;counts for that word in the input list. For instance, if the input list is ((time 1) (is 1) (long 1) (but 1) (life 1) (is 1) (short 1))
;then the output should be ((short 1) (is 2) (life 1) (but 1) (long 1) (time 1)) 3 Write mergeByWord based on the reduce function we discussed in
;class and mergeWC. The reduce function is not built-in in Scheme; you can type it in yourself: (define (reduce f l v) (if (null? l) v
;(f (car l) (reduce f (cdr l) v))))
              
              
              
(define (mergeByWord l)
 (if (null? l) '()
     (mergeWC (car l) (mergeByWord (cdr l)))))


;(D) Write a wordcount function that takes in a
;list of words and outputs the right word-count list. Write this
;function based on initialWCList and mergeByWord.


(define (wordcount l)
  (if (null? l) 'EmptyList
      (mergeByWord (initialWCList l))))

