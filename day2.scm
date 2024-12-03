#!/bin/env -S csi -script
(module (aoc day2) ()
  (import scheme
          (chicken base)
          (chicken format)
          (chicken io)
          (chicken port)
          (chicken process-context)
          (srfi 1))

  (when (null? (command-line-arguments))
    (display "Please pass the input file as the first argument" (current-error-port))
    (newline (current-error-port))
    (exit 1))

  (define reports
    (with-input-from-file (car (command-line-arguments))
      (lambda ()
        (let loop ((current-report (read-line))
                   (reports '()))
          (if (or (eof-object? current-report)
                  (string=? current-report ""))
              reports
              (loop (read-line)
                    (cons (with-input-from-string current-report
                            (lambda ()
                              (let loop ((number (read))
                                         (report '()))
                                (if (eof-object? number)
                                    report
                                    (loop (read)
                                          (cons number
                                                report))))))
                          reports)))))))

  (define (test-report report)
    (cond
      ((not (or (apply < report) (apply > report))) 0)
      ((< (length report) 2) 1)
      (else (let loop ((current-level (car report))
                       (rest (cdr report)))
              (cond
                ((null? rest) 1)
                ((> (abs (- current-level (car rest))) 3) 0)
                (else (loop (car rest)
                            (cdr rest))))))))

  ;; Part One
  (display (format "Number of safe reports: ~A"
                   (apply + (map test-report
                                 reports))))
  (newline)

  ;; Part Two
  ;; This function returns the same list minus the element at the given index
  (define (list-rem lst index)
    (append (take lst index)
            (drop lst (+ index 1))))

  (display (format "Number of safe reports: ~A"
                   (apply + (map (lambda (report)
                                   (let ((result (test-report report)))
                                    (if (= result 0)
                                        (let loop ((i 0))
                                         (if (< i (length report))
                                             (let ((result (test-report (list-rem report i))))
                                              (if (= result 0)
                                                  (loop (+ i 1))
                                                  result))
                                             0))
                                        result)))
                                 reports))))
  (newline))

