(define n 10)

(defun fib (n)
  (if (= n 0)
      1
    (if (= n 1)
        1
      (+ (fib (- n 1)) (fib (- n 2)))
      )
    )
  )

(putInt32 (fib n))
