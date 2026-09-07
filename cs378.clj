; cs378.clj      Gordon S. Novak Jr.       ; 28 Aug 26

; 29 Jan 19; 04 Dec 19; 30 Aug 20; 02 Mar 22

; (load-file "/u/novak/cs378/cs378.clj")
;                    tst.xyz.core
(ns user
  (:use clojure.test)
  (:require [clojure.string :as str]))
;            [clojure.math.numeric-tower :as math]

(def lstnum '(76 85 71 83 84 89 96 84 98 97 75 85 92 64 89 87 90 65 100))

; test whether x is a cons
(defn consb? [x] (and (seq? x) (not (empty? x))))

; test whether x is a cons
(defmacro cons? [x]
  (if (seq? x)
    (list 'consb? x)
    (list 'and (list 'seq? x) (list 'not (list 'empty? x)))))

; test whether v is a variable for pattern matching,
; i.e. v is a symbol that begins with ?, such as ?x
(defn varp [v]
  (and (symbol? v)
       (str/starts-with? (name v) "?")))

; look up a symbol in an association list, same as assoc in lisp.
; (assocl 'ut '((rice owl) (ut bevo) (a&m dog)))  =  (ut bevo)
(defn assocl [key lst]
  (if (empty? lst)
    nil
    (if (= (first (first lst)) key)
      (first lst)
      (assocl key (rest lst)))))

; test whether two list structures (trees) are equal
; (equal '((a b) c) '((a b) c))  =  true
(defn equal [x y]
  (if (cons? x)
    (and (cons? y)
         (equal (first x) (first y))
         (equal (rest x) (rest y)))
    (= x y)))

; test for empty list.  Similar to empty? but okay for non-list
(defn null? [x] (or (= x nil) (= x '())))

; pattern matching function
(defn matchb [pat inp bindings]
  (if (and (seq? bindings) (not (empty? bindings)))
    (if (cons? pat)
      (and (cons? inp)
           (matchb (rest pat)
                   (rest inp)
                   (matchb (first pat)
                           (first inp) bindings)))
      (if (varp pat)
        (if (assocl pat bindings)
          (and (equal inp
                      (second (assocl pat bindings)))
               bindings)
          (cons (list pat
                      (if (vector? inp)
                        (apply list inp) ; convert vector to list
                        inp))
                bindings))
        (and (= pat inp) bindings)))))

; pattern matching
(defn match [pat inp]  (matchb pat inp '((t t))))

; substitute old for new in form
; (subst 'fish 'beef '(beef taco))  =  (fish taco)
(defn subst [new old form]
  (if (cons? form)
    (cons (subst new old (first form))
          (subst new old (rest form)))
    (if (= form old)
      new
      form)))

; substitute from an association list into form
; (sublis '((rose peach) (smell taste))
;         '(a rose by any other name would smell as sweet))
;    =  (a peach by any other name would taste as sweet)
(defn sublis [alist form]
  (if (cons? form)
    (cons (sublis alist (first form))
          (sublis alist (rest form)))
    (let [binding (assocl form alist)]
      (if binding
        (second binding)
        form))))

; make a copy of a tree structure: compare to subst and sublis
(defn copy-tree [form]
  (if (cons? form)
    (cons (copy-tree (first form))
          (copy-tree (rest form)))
    form))

; transform an input according to a pattern-pair
; (transform '((i aint got no ?x) (i do not have any ?x))
;            '(i aint got no bananas))
;      =   (i do not have any bananas)
(defn transform [pattern-pair input]
  (let [bindings (match (first pattern-pair) input)]
    (if bindings
      (sublis bindings (second pattern-pair)))))

; parts of an expression: operator, left-hand side, right-hand side
(defn op [e] (first e))
(defn lhs [e] (second e))
(defn rhs [e] (first (rest (rest e))))

; append two lists: copies the first, reuses the second
; (append '(a b c) '(d e))  =  (a b c d e)
(defn append [x y]
  (if (empty? x)
    y
    (cons (first x)
          (append (rest x) y))))

; test if item is in list; returns rest of list starting with item
; (member 'dick '(tom dick harry))  =  (dick harry)
(defn member [item lst]
  (if (empty? lst)
    nil
    (if (= item (first lst))
      lst
      (member item (rest lst)))))

; intersection of two sets
; (intersection '(a b c) '(a c e))  =  (a c)     or  (c a)
(defn intersection [x y]
  (if (empty? x)
    '()
    (if (member (first x) y)
      (cons (first x)
            (intersection (rest x) y))
      (intersection (rest x) y))))

(defn trrevb [lst answer]
  (if (empty? lst)
    answer
    (trrevb (rest lst)
            (cons (first lst) answer))))

; tail-recursive reverse
; (trrev '(a b c))  =  (c b a)

(defn trrev [lst] (trrevb lst '()))

(defn lengthb [lst answer]
  (if (empty? lst)         ; test for base case
    answer               ; answer for base case
    (lengthb (rest lst)  ; recursive call
             (+ answer 1))))   ; update answer

; length of a list
; (length '(a b c))  =  3
(defn length [lst]
  (if (cons? lst)
    (lengthb lst 0)         ; init extra variable
    0))

(defn square [x] (* x x))

; (defn abs [x] (if (< x 0) (- x) x) )  ; now part of Clojure

(defn sqrt [x] (Math/sqrt x))
(defn exp [x] (Math/exp x))
(defn log [x] (Math/log x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))
(defn atan2 [y x] (Math/atan2 y x))

; exponent, x to the nth power
(defn expt [x n]
  (if (or (float? x) (double? x))
    (Math/pow x n)
    (if (= n 0)
      1
      (if (> n 0)
        (* x (expt x (- n 1)))
        (/ 1 (expt x (- n)))))))



; test whether the predicate pred is true of every item in lst
;   (every (fn [x] (> x 3)) '(4 5 6))  =  true
(defn every [pred lst]
  (if (empty? lst)
    true
    (and (pred (first lst))
         (every pred (rest lst)))))

; It turns out that Clojure has every, but with a ?
(defmacro every [fn lst] (list 'every? fn lst))

; test if list x is a subset of list y
(defn subset? [x y] (every (fn [z] (member z y)) x))

; test if list se4t x equals list set y
(defn set= [x y] (and (subset? x y) (subset? y x)))

; third thing in a list.
; (third '(a b c d))  =  c
(defn third [lst] (first (rest (rest lst))))

; cause something to be quoted, unless it is self-quoting.
; (kwote 'foo)  =  (quote foo)
; (kwote '(quote foo))  =  (quote foo)
; (kwote 3)  =  3
(defn kwote [x]
  (if (or (number? x)
          (and (cons? x)
               (= (first x) 'quote)))
    x
    (list 'quote x)))

; (mapcat fn lst) is like mapcan in Lisp.

(defn exit [] (System/exit 0))
(defn quit [] (System/exit 0))


;; PROBLEM SOLUTIONS

;;2a. recursive function that adds up the numbers in a list
(defn sum [lst]
  (if (empty? lst)
    0
    (+ (first lst) (sum (rest lst)))))

;;"2b. sumtr in tail-recursive style"
(defn sumtr
  ([lst] (sumtr lst 0))
  ([lst acc]
   (if (empty? lst)
     acc
     (recur (rest lst) (+ acc (first lst))))))

;;"2c. sumr using reduce"
(defn sumr [lst]
  (reduce + 0 lst))

;;"3a. recursive function that adds up the squares of numbers in lst"
(defn sumsq [lst]
  (if (empty? lst)
    0
    (+ (* (first lst) (first lst)) (sumsq (rest lst)))))

;;"3b. sumsqtr in tail-recursive style"
(defn sumsqtr
  ([lst] (sumsqtr lst 0))
  ([lst acc]
   (if (empty? lst)
     acc
     (recur (rest lst) (+ acc (* (first lst) (first lst)))))))


;;"3c. sumsqmr using map and reduce"
(defn sumsqmr [lst]
  (reduce + 0 (map #(* % %) lst)))


;;"4 find the standard deviation of a list of numbers"

;;"helper functions"
(defn mean [lst]
  (/ (sum lst) (count lst)))

(defn mean-square [lst]
  (/ (sumsqtr lst) (count lst)))

(defn variance [lst]
  (- (mean-square lst) (Math/pow (mean lst) 2)))

(defn stddev [lst]
  (Math/sqrt (variance lst)))

(def lstnum '(76 85 71 83 84 89 96 84 98 97 75 85 92 64 89 87 90 65 100))

(stddev lstnum)

;; 5. union and set difference of two srts functions

(defn union [x y]
  (if (empty? x)
    y
    (if (member (first x) y)
      (union (rest x) y)
      (cons (first x) (union (rest x) y)))))

(defn set-difference [x y]
  (if (empty? x)
    (quote ())
    (if (member (first x) y)
      (set-difference (rest x) y)
      (cons (first x) (set-difference (rest x) y)))))

;;problem 6
;;helper function to create a new row from existing row
(defn next-row [row]
  (concat [1]
          (map + row (rest row))
          [1]))

;;not a recursive function yet, only calls next row after base case with n - 1
(defn binomial [n]
  (if (= n 0)
    '(1)
    (next-row (binomial (dec n)))))

;;recursive function to add elements in a row
;;base case if list is less than two elements, no adjacent pairs exist
(defn add-adjacent [row]
  (if (< (count row) 2)
    '()
    (cons (+ (first row) (second row))
          (add-adjacent (rest row)))))


;;problem 7
;;recursive function to find the max in a tree using first and rest as subtrees
;;base caseL if tree is empty, return nil
(defn maxbt [tree]
  (cond
    (number? tree)     tree
    (sequential? tree) (if (empty? tree)
                         Double/NEGATIVE_INFINITY
                         (max (maxbt (first tree)) ;;if empty then return a small negative number since every number is larger than -999999 
                              (maxbt (rest tree)))) ;;recursively call on the left and right subtree to find the max
    :else              Double/NEGATIVE_INFINITY)) ;;if tree is not a number or a sequence, return a small negative number

;;problem 8

;;returns a set of veriables in an expressions
(defn vars [expr]
  (cond
    (number? expr) (quote {}) ;;number contributes no variables
    (symbol? expr) (list expr) ;; otherwise create a list ofthe symbol so we recurse
    :else (union (vars (lhs expr)) (vars (rhs expr)))))

;;problem 9
;;tests whether the item occurs anywhere in expression tree
;;recursively searches the left and right subtrees of the expression tree

(defn occurs [item tree]
  (cond
    (= item tree) true ;;if the item is equal to the tree, return true
    (sequential? tree) (if (empty? tree)
                         false ;;if the tree is empty, return false
                         (or (occurs item (first tree)) ;;recursively call on the left and right subtrees
                             (occurs item (rest tree))))
    :else false))

;;problem 10
;;evalute an expression tree where the leaves are numbers and the internal nodes are operators

(defn myeval [tree]
  (if (number? tree)
    tree ;; if the tree is a number, return the number
    (case (op tree)
      - (if (nil? (rhs tree))
          (- (myeval (lhs tree))) ;; if the right-hand side is nil, return the negation
          (- (myeval (lhs tree)) (myeval (rhs tree)))) ;; otherwise, return the difference
      + (+ (myeval (lhs tree)) (myeval (rhs tree)))
      * (* (myeval (lhs tree)) (myeval (rhs tree))) ;; Note: Changed + to * for multiplication
      / (/ (myeval (lhs tree)) (myeval (rhs tree)))
      expt (Math/pow (myeval (lhs tree)) (myeval (rhs tree)))
      sqrt (Math/sqrt (myeval (lhs tree)))
      exp (Math/exp (myeval (lhs tree)))
      log (Math/log (myeval (lhs tree))))))

;;problem 11
;;evaluate an expression tree where the leaves could be numberd or variables whose values are in a list called bindings (an association list)
(defn lookup [key list]
  (cond
    (empty? list) nil ;;if the list is empty, return nil
    (= key (first (first list))) (first list) ;;if the key is equal to the first element of the first pair, return the second element of the first pair
    :else (lookup key (rest list)))) ;;otherwise, recursively call on the rest of the list

;;myevalb function using the lookup function
(defn myevalb [tree bindings]
  (cond
    (number? tree) tree ;;if the tree is a number, return the number
    (symbol? tree) (second (lookup tree bindings)) ;; if the tree is a variable call the lookup function
    :else (case (op tree)
            - (if (nil? (rhs tree))
                (- (myevalb (lhs tree) bindings))
                (- (myevalb (lhs tree) bindings) (myevalb (rhs tree) bindings)))
            + (+ (myevalb (lhs tree) bindings) (myevalb (rhs tree) bindings))
            * (* (myevalb (lhs tree) bindings) (myevalb (rhs tree) bindings))
            / (/ (myevalb (lhs tree) bindings) (myevalb (rhs tree) bindings))
            expt (Math/pow (myevalb (lhs tree) bindings) (myevalb (rhs tree) bindings))
            sqrt (Math/sqrt (myevalb (lhs tree) bindings))
            exp (Math/exp (myevalb (lhs tree) bindings))
            log (Math/log (myevalb (lhs tree) bindings)))))

;;problem 12
(defn precedence [o]
  (case o
    = 1
    (+ -) 5
    (* /) 6))

(defn operator? [o]
  (contains? #{'+ '- '* '/ '= 'expt 'sqrt 'exp 'log} o))

(defn unaryminus? [e]
  (and (= (op e) '-) (nil? (rhs e)))) ;;unary minus is when the operator is - and the right-hand side is nil

(defn tojava
  ([tree] (str (tojava tree 0) ";"))
  ([n context]
   (cond
     (number? n) (str n)
     (symbol? n) (str n)
     (unaryminus? n) (str "-(" (tojava (lhs n) 0) ")")
     (operator? (op n))
     (let [p (precedence (op n))
           left (tojava (lhs n) p)
           right (tojava (rhs n) p)
           expr (str left (op n) right)]
       (if (<= p context) (str "(" expr ")") expr))
     (= (op n) 'expt)
     (str "Math.pow(" (tojava (lhs n) 0) ", " (tojava (rhs n) 0) ")")
     :else
     (str "Math." (name (op n)) "(" (tojava (lhs n) 0) ")"))))