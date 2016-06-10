(define silent-test #f)

(define (print-test-result expression expect res)
  (if (equal? res expect)
      (if silent-test
          (display "")
          (for-each display (list "Success: " expression " -> " res "\n")))
      (for-each display (list "ERROR  : " expression " -> " res
                              ", but expected " expect "\n"))))

(define-syntax test
  (syntax-rules ()
    ((test expression expect)
     (print-test-result 'expression expect expression))))

;; Test-data
(define sample-tree-2 '(((((leaf g 1) ((leaf i 1) (leaf h 1) (i h) 2) (g i h) 3)
                          (leaf d 4) (g i h d) 7)
                         (leaf b 8) (g i h d b)  15)
                        ((leaf a 10) (((leaf f 3) (leaf e 3) (f e) 6)
                                      (leaf c 7) (f e c) 13)
                                     (a f e c) 23)
                        (g i h d b a f e c) 38))

;; 1a
(newline) (display "Tester  p-car og p-cdr (1a):\n")
(test (p-car (p-cons "foo" "bar")) "foo")
(test (p-cdr (p-cons "foo" "bar")) "bar")
(test (p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar")))) "foo")
(test (p-car (p-cons #f #t)) #f)
(test (p-cdr (p-cons #f #t)) #t)

;; 2a
(newline) (display "Tester member? (2a):\n")
(test (member? 'forest '(lake river ocean)) #f)
(test (member? 'river '(lake river ocean)) #t)
(test (member? 1 '()) #f)
(test (member? 1 '(1)) #t)

;; 2c
;; endre evt decode til eget navn             
(newline) (display "Tester decode (2c):\n")
(test (decode '(0 1 1 0 1 1 1 1 1 0) sample-tree) '(ninjas night by night))
(test (decode '(0) sample-tree) '(ninjas))
(test (decode '() sample-tree) '())

;; 2e
(newline) (display "Tester encode (2e):\n")
(test (decode (encode '(ninjas fight ninjas) sample-tree) sample-tree) '(ninjas fight ninjas))
(test (encode '(ninjas fight by night) sample-tree) '(0 1 0 1 1 1 1 1 0))
(test (encode '(a e b c a g) sample-tree-2) '(1 0 1 1 0 1 0 1 1 1 1 1 0 0 0 0 0))

;; 2f
(newline) (display "Tester grow-huffman-tree (2f):\n")
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(display "Anvender grow-huffman-tree: (define codebook (grow-huffman-tree freqs)\n")
(define codebook (grow-huffman-tree freqs))
(test (decode (encode '(a b c) codebook) codebook) '(a b c))
(test (decode (encode '(a b c d e f b b d) codebook) codebook) '(a b c d e f b b d))
(test (weight codebook) 15)
(test (and (member? 'a (symbols codebook)) (member? 'f (symbols codebook))) #t)

;; 2h
(newline) (display "Tester huffman-leaves (2h):\n")
(display "Merk: Det er OK om testene feiler fordi liste-elementene kommer i en annen rekkefølge.\n")
(test (huffman-leaves sample-tree) '((ninjas 8) (fight 5) (night 1) (by 1)))
(test (huffman-leaves codebook) '((f 3) (d 3) (a 2) (e 1) (c 1) (b 5)))

;; 2i
(newline) (display "Tester expected-code-length (2i):\n")
(test (expected-code-length sample-tree) 8/5)
(test (expected-code-length sample-tree-2) 107/38)
