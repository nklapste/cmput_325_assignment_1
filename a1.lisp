(defun issorted (L)
  (if (nth 0 L)
    (if (nth 1 L)
      (if (< (first L) (nth 1 L))
        (issorted (cdr L))
        ())
      T)
    T))

(defun numbers(N)
  (if (= 1 N)
    (cons 1 ())
    (append (numbers (- N 1)) (cons N ()))
  )
)
