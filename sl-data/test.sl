(defun fib (n)
  (if (or (= n 0) (= n 1))
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
)
(print "fib 10 is : ")
(putInt32 (fib 10))
(print "end fib")
(defun b (c d) (/ c d (- c d)))
(+ 1123 (b 3000 4002) (/ 23 30) (* 7 8 9 1 ) (+ 1) )
(if true (print "in if: \"test sdf") (print "in else: sdf"))
(putInt32 (/ 2 3))
(+ 3 4)
