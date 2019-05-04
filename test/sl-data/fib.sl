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

(putInt32 (plus1 100))

(define plus1 (lambda (a) (+ a 1)))
