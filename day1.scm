#!/bin/env -S csi -script
(module (aoc day1) ()
  (import scheme
          (chicken base)
          (chicken format)
          (chicken sort)
          (chicken process-context))

  (when (null? (command-line-arguments))
    (display "Please pass the input file as the first argument" (current-error-port))
    (newline (current-error-port))
    (exit 1))

  ;; Part One
  (define locations1 '())
  (define locations2 '())

  (with-input-from-file (car (command-line-arguments))
    (lambda ()
      (let loop ((in1 (read))
                 (in2 (read)))
        (unless (or (eof-object? in1)
                    (eof-object? in2))
          (set! locations1 (cons in1 locations1))
          (set! locations2 (cons in2 locations2))
          (loop (read) (read))))))

  (display (format "The total distance between the two lists is: ~A"
                   (apply + (map (compose abs -)
                                 (sort locations1 <)
                                 (sort locations2 <)))))
  (newline)

  ;; Part Two
  (display (format "The similarity score is: ~A"
                   (let loop ((similarity 0)
                              (left locations1))
                     (if (null? left)
                         similarity
                         (loop (+ similarity
                                  (* (car left)
                                     (let loop ((count 0)
                                                (right locations2))
                                       (if (null? right)
                                           count
                                           (loop (if (= (car left)
                                                        (car right))
                                                     (+ count 1)
                                                     count)
                                                 (cdr right))))))
                               (cdr left))))))
  (newline))

