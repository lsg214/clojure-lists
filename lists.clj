"control shift enter and place cursor before last parenthesis"
"1a. recursive function that adds up the numbers in a list"
(defn sum [list]
    (if (empty? list)
        0
        (+ (first list) (sum (rest list)))))

"1b. sumtr in tail-recursive style"
(defn sumtr
  ([list] (sumtr list 0))
  ([list acc]
    (if (empty? list)
      acc
      (recur (rest list) (+ acc (first list))))))

"1c. sumr using reduce"
(defn sumr [list]
  (reduce + 0 list))

