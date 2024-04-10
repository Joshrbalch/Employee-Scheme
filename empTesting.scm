(define (addSalaried empList firstName lastName salary)
  (cons (list "salaried" firstName lastName salary) empList))

(define (addHourly empList firstName lastName hoursWorked hourlyRate)
  (cons (list "hourly" firstName lastName hoursWorked hourlyRate) empList))

(define (addCommissioned empList firstName lastName basePay sales commissionRate)
  (cons (list "commission" firstName lastName basePay sales commissionRate) empList))

(define employees '()) ; Define an empty list of employees

; Example usage:
(set! employees (addSalaried employees "John" "Doe" 3000))
(set! employees (addHourly employees "Jane" "Smith" 30 7.6))
(set! employees (addCommissioned employees "Alice" "Johnson" 2000 5000 0.05))
(display employees)
