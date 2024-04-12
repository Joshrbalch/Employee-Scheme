(define (check value op threshold)
  (cond ((string=? op "eq") (equal? value threshold))
        ((string=? op "ne") (not (equal? value threshold)))
        ((string=? op "ge") (>= value threshold))
        ((string=? op "le") (<= value threshold))
        ((string=? op "gt") (> value threshold))
        ((string=? op "lt") (< value threshold))
        (else #f)))

(define (readEmployee name)
  (let ((port (open-input-file name)))
    (define empList '())
    (read-file port empList)
    (close-input-port port)
    'done))

(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
            ((char=? ch (string-ref str b)) (if (= a b)
              (split (+ 1 a) (+ 1 b))
              (cons (substring str a b) (split b b))))
            (else (split a (+ 1 b)))))))
    (split 0 0))))

(define (read-file file empList)
  (let ((port (open-input-file file)))
    (let loop ((line (read-line port)))
      (if (eof-object? line)
          (begin
            (close-input-port port)
            empList)
          (begin
            (set! empList (append empList (list (str-split line #\space))))
            (loop (read-line port)))))))

(define display-salaried (lambda (emp)
  (let ((salary (cadr (cddr emp)))
        (firstName (cadr emp))
        (lastName (caddr emp)))
    (display "Salaried employee: ")
    (display firstName)
    (display " ")
    (display lastName)
    (newline)
    (display "weekly salary: ")
    (display salary)
    (display "")
    (newline)
    (display "earned $")
    (display salary)
    (display "")
    (newline)
    (newline))))

(define (calc-hourly-earned emp)
  (let ((hoursWorked (string->number (cadr (cddr emp))))
        (hourlyRate (string->number (caddr (cddr emp)))))
    (cond
      ((<= hoursWorked 40) (* hoursWorked hourlyRate)) ; If hours worked is less than or equal to 40
      ((<= hoursWorked 50) (+ (* 40 hourlyRate) (* 1.5 hourlyRate (- hoursWorked 40)))) ; If hours worked is greater than 40 and less than or equal to 50
      (else (+ (* 40 hourlyRate) (* 1.5 hourlyRate 10) (* 2 hourlyRate (- hoursWorked 50))))))) ; If hours worked is greater than 50


(define display-hourly
  (lambda (emp)
    (let ((hoursWorked (cadr (cddr emp)))
          (hourlyRate (caddr (cddr emp)))
          (firstName (cadr emp))
          (lastName (caddr emp))
          (earned (calc-hourly-earned emp)))
      (display "Hourly employee: ")
      (display firstName)
      (display " ")
      (display lastName)
      (newline)
      (display "hours worked: ")
      (display hoursWorked)
      (display ", hourly rate: ")
      (display hourlyRate)
      (newline)
      (display "earned $")
      (display earned) ; Display the calculated earnings directly
      (newline)
      (newline))))

(define (calc-commissioned-earned emp)
  (define basePay (cadr (cddr emp)))
  (define newBasePay (string->number basePay))
  (define newSales (string->number (caddr (cddr emp))))
  (define newCommissionRate (string->number (cadddr (cddr emp))))
  (if (> newBasePay (* newSales newCommissionRate))
      newBasePay
      (+ newBasePay (* newSales newCommissionRate))))

(define display-commissioned
  (lambda (emp)
    (let ((basePay (cadr (cddr emp)))
          (sales (caddr (cddr emp)))
          (commissionRate (cadddr (cddr emp)))
          (firstName (cadr emp))
          (lastName (caddr emp))
          (earned (calc-commissioned-earned emp)))
      (display "Commissioned employee: ")
      (display firstName)
      (display " ")
      (display lastName)
      (newline)
      (display "base pay: ")
      (display basePay)
      (display ", sales: ")
      (display sales)
      (display ", commission rate: ")
      (display commissionRate)
      (newline)
      (display "earned $")
      (display earned)
      (newline)
      (newline))))

(define (display-employees empList)
  (for-each
    (lambda (emp)
      (if (not (null? emp))
          (let ((type (car emp)))
            (cond
              ((string=? type "salaried")
               (display-salaried emp))
              ((string=? type "hourly")
               (display-hourly emp))
              ((string=? type "commission")
               (display-commissioned emp))))))
    empList))

(define (count lst)
  (if (null? lst)
      0
      (+ 1 (count (cdr lst)))))

(define empList '()) ;; Initialize the employee list
(define empList (read-file "employees.dat" empList))
(display "Number of employees: ")
(display (count empList)) ;; Display the number of employees
(newline)
(newline)
(display-employees empList) ;; Display the resulting employee list

