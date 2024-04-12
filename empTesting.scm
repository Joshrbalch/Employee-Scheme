(define (addSalaried empList firstName lastName salary)
  (cons (list "salaried" firstName lastName salary) empList))

(define (addHourly empList firstName lastName hoursWorked hourlyRate)
  (cons (list "hourly" firstName lastName hoursWorked hourlyRate) empList))

(define (addCommissioned empList firstName lastName basePay sales commissionRate)
  (cons (list "commission" firstName lastName basePay sales commissionRate) empList))

(define (readEmployee name)
  (let ((port (open-input-file name)))
    (define empList '())
    (read-file port empList)
    (close-input-port port)
    'done))

(define (str-split str ch)
  (let ((len (string-length str)))
    letrec
      ((split
        (lambda (a b)
          (cond
            ((>=b len) (if (= a b) '() (cons (substring str a b)'())))
              ((char=? ch(string-ref str b)) (if(= a b)
                (split (+ 1 a) (+ 1 b))
                (cons (substring str a b)(split b b))))
                (else (split a (+ 1 b))))))
                (split 0 0))))

(define (read-file file empList)
  (let loop ((line (read-line file)))
    (if (eof-object? line)
        empList
        (begin
          (display "Read line: ")
          (display line)
          (newline)
          (set! empList (string-split line))))))

(define employees '()) ; Define an empty list of employees

(read-file "employees.dat" employees)

(display employees)
(newline)
