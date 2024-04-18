(define op "ge")
(define threshold 0)

(define (check value op threshold)
  (if (number? value)
      (let ((num-value value)
            (num-threshold (if (number? threshold) threshold (string->number threshold))))
        (cond ((string=? op "eq") (= num-value num-threshold))
              ((string=? op "ne") (not (= num-value num-threshold)))
              ((string=? op "ge") (>= num-value num-threshold))
              ((string=? op "le") (<= num-value num-threshold))
              ((string=? op "gt") (> num-value num-threshold))
              ((string=? op "lt") (< num-value num-threshold))
              (else #f)))
      #f))

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

(define calculateSalary
  (lambda (emp)
    (let ((type (car emp)))
      (cond
        ((string=? type "salaried") (calc-salaried-earned emp))
        ((string=? type "hourly") (calc-hourly-earned emp))
        ((string=? type "commission") (calc-commissioned-earned emp))))))

(define (calc-salaried-earned emp)
  (define oldSalary (string-trim (cadr (cddr emp))))
  (string->number oldSalary))

(define display-salaried 
  (lambda (emp)
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
  (define oldhourlyRate (string-trim (caddr (cddr emp))))
  (let ((hoursWorked (string->number (cadr (cddr emp))))
        (hourlyRate (string->number oldhourlyRate)))
    (cond
      ((<= hoursWorked 40) (* hoursWorked hourlyRate))
      ((<= hoursWorked 50) (+ (* 40 hourlyRate) (* 1.5 hourlyRate (- hoursWorked 40))))
      (else (+ (* 40 hourlyRate) (* 1.5 hourlyRate 10) (* 2 hourlyRate (- hoursWorked 50)))))))

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
      (display earned)
      (newline)
      (newline))))

(define (calc-commissioned-earned emp)
  (define basePay (cadr (cddr emp)))
  (define newBasePay (string->number basePay))
  (define newSales (string->number (caddr (cddr emp))))
  (define oldcommissionRate (string-trim (cadddr (cddr emp))))
  (define newCommissionRate (string->number oldcommissionRate))
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
      (display commissionRate) ; Display the commission rate as percentage
      (display "%")
      (newline)
      (display "earned $")
      (display earned)
      (newline)
      (newline))))

(define (display-employees empList)
  (for-each
    (lambda (emp)
      (if (not (null? emp))
          (let ((salary (calculateSalary emp)))
            (if (number? salary)
                (if (check salary op threshold)
                    (let ((type (car emp)))
                      (cond
                        ((string=? type "salaried")
                         (display-salaried emp))
                        ((string=? type "hourly")
                         (display-hourly emp))
                        ((string=? type "commission")
                         (display-commissioned emp))))))
            ))
      )
    empList))

(define (count lst)
  (if (null? lst)
      0
      (let ((salary (calculateSalary (car lst))))
        (if (number? salary)
            (if (check salary op threshold)
                (+ 1 (count (cdr lst)))
                (count (cdr lst)))
            (count (cdr lst))))))

(define countEmployees
  (lambda (empList)
    (let ((numEmployees (count empList)))
      (display "There are ")
      (display numEmployees)
      (display " employees")
      (newline)
      (newline)
      numEmployees)))

(define (getAverage lst)
  (let ((sum 0)
        (count 0))
    (for-each
      (lambda (emp)
        (if (not (null? emp))
            (let ((salary (calculateSalary emp)))
              (if (check salary op threshold)
                  (if (number? salary)
                      (begin
                        (set! sum (+ sum salary))
                        (set! count (+ count 1))))))))
      lst)
    (if (= count 0)
        0
        (/ sum count))))

(define average 
  (lambda (empList)
    (display "Average salary is $")
    (display (getAverage empList))
    (newline)
    (newline)
    'done))

(define (getMin lst)
  (let ((min-salary 1000000)
        (min-emp '()))
    (for-each
     (lambda (emp)
       (if (not (null? emp))
           (let ((salary (calculateSalary emp)))
             (if (check salary op threshold)
                 (if (number? salary)
                     (if (< salary min-salary)
                         (begin
                           (set! min-salary salary)
                           (set! min-emp emp))))))))
     lst)
    min-emp))

    


(define (getMax lst)
  (let ((max-salary -1000000) ; Initialize max-salary to a very low value
        (max-emp '()))
    (for-each
     (lambda (emp)
       (if (not (null? emp))
           (let ((salary (calculateSalary emp)))
             (if (check salary op threshold)
                 (if (number? salary)
                     (if (> salary max-salary)
                         (begin
                           (set! max-salary salary)
                           (set! max-emp emp))))))))
     lst)
    max-emp))

(define totalSalary 0) ; Define totalSalary as a global variable

(define (totalRec lst)
  (if (null? lst)
      0
      (let ((current-salary (calculateSalary (car lst))))
        (if (check current-salary op threshold)
            (+ current-salary (totalRec (cdr lst)))
            (totalRec (cdr lst))))))

(define (total lst)
  (set! totalSalary (totalRec lst))
  (display "Total payment is $")
  (display totalSalary)
  (newline))

(define (perform filename function . args)
  (define empList '()) ;; Initialize the employee list
  (define empList (read-file "employees.dat" empList))

  (let ((op "ge")  ; Default value for op
        (threshold 0))  ; Default value for threshold
    (if (not (null? args))
        (begin
        (display "DEGBUG")
        (newline)
          (if (string? (car args))
              (set! op (cadr args)))
          (if (string=? (car args) "threshold")
              (set! threshold (string->number (cadr args))))))
    (cond
      ((string=? function "print") (display-employees empList))
      ((string=? function "count") (countEmployees empList))
      ((string=? function "avg") (average empList))
      ((string=? function "min") (display-employees (list (getMin empList))))
      ((string=? function "max") (display-employees (list (getMax empList))))
      ((string=? function "total") (total empList))
      (else #f))))

