
(define (print-file name)
  (let ((port (open-input-file name)))
    (print-file-helper port)
    (close-input-port port)
    'done))

(define (print-file-helper port)
  (let ((stuff (read-line port)))
    (if (eof-object? stuff)
	'done
	(begin (display stuff)
               (newline)
	       (print-file-helper port)))))

(print-file "employees.dat")
