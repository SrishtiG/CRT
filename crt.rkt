;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname crt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;;************************************
;;This progam is meant to solve any given system of simultaneous congruence equations 
;;using the Chinese Remainder Theorem. 

;;first, with coefficients and powers of 1: 

;; Data definition for a System: 
;; A System-1 is a (listof AL) 
;; where the keys will be the mods and the values are the remainders. 
;; For instance, if we have a system where an unknown nummber is congruent 
;; to 2(mod 3) and 7(mod 10), then we write the input as '((3 2) (10 7))
;; required that the input system has at least two elements in it. 

;; (crt-1 system) : computes the value of an unknown number given properties
;;                  as described by the system, or the string "no solution"
;;                  if there is no solution.
;;crt-1: (listof AL) -> (anyof Num or Sym)
;;
(define (crt system)
  (local 
    [;; has-solution? : checks if a given system has a solution 
     (define list-mods (map (lambda (x) (first x)) system))
     
     (define rem-list (map (lambda (x) (second x)) system))
     
     (define no-solution "no solution")
     
     (define has-solution? 
       (local [  (define (is-coprime? num numlist)
                   (cond 
                     [(empty? numlist) true]
                     [(not (= 1 (gcd num (first numlist)))) false]
                     [else (is-coprime? num (rest numlist))]))
                 
                 (define (has-soln/acc mod_list/acc)
                   (cond [(empty? mod_list/acc) true]
                         [(false? 
                           (is-coprime? (first mod_list/acc) (rest mod_list/acc)))
                          no-solution]
                         [else (has-soln/acc (rest mod_list/acc))]))]
         (has-soln/acc list-mods)))
     ;; N is the product of all mods in the system 
     (define N (foldr * 1 list-mods))
     
     (define raw-addlist (map (lambda (x) (/ N x)) list-mods))
     
     (define mod-addlist (map remainder raw-addlist list-mods))
     ;;now, we need to check the raw-addlist and alter it. comparator+corrector
     ;;compares the addlist value to the corresponding AL for the same remainder
     ;; if it is in the same modular class, the same number is spit out, else it 
     ;; is altered using properties of mods. 
     (define (comparator+corrector al num)
       (cond [(= (second al) (remainder num (first al))) num]
             [(not (zero? (remainder (second al) (gcd num (first al))))) no-solution] 
             [else (* num (lcs num (second al) (first al)))]))
     ;;lcs is a linear congruence solver. Its inputs are the coefficient of the 
     ;; unknown, the remainder and the mod, in that order. 
     (define (lcs n m rem)
       (local [
               (define a (remainder n m))
               
               (define (eea lst)
                 (match lst
                   [(list (list x r q) (list X R Q)) 
                    (cond
                      [(= 0 R) (list x r q)] ;the answer row
                      [else 
                       (let* ([quot (quotient r R)] ;new q
                              [nextx (- x (* quot X))] 
                              [nextr (- r (* quot R))])
                         (eea (list (list X R Q)(list nextx nextr quot))))])]))
               
               (define p (first (eea (list (list 1 m 0) (list 0 n 0)))))
               
               (define ans (* a p))]
         
         (cond [(and (< 0 ans) (> m ans)) ans]
               [(< m ans) (remainder ans m)]
               [else (+ ans m)])))]
    
    (cond [(equal? no-solution has-solution?) no-solution] 
          [else (foldr + 0 (map comparator+corrector system raw-addlist))]))) 


(crt '((5 1) (3 2) (4 2)))

