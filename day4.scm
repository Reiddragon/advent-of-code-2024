#!/bin/env -S csi -script
(module (aoc day4) ()
  (import scheme
          (chicken base)
          (chicken format)
          (chicken io)
          (chicken process-context)
          (chicken string))

  (when (null? (command-line-arguments))
    (display "Please pass the input file as the first argument" (current-error-port))
    (newline (current-error-port))
    (exit 1))

  (define input
    (list->vector
      (map (compose
             list->vector
             string->list)
           (string-split
             (with-input-from-file
               (car (command-line-arguments))
               read-string)
             "\n"))))

  ;; Extended vector-ref that allows multiple indices
  (define (vector-ref* vec . indices)
    (let loop ((vec vec)
               (indices indices))
      (if (null? indices)
          vec
          (loop (vector-ref vec (car indices))
                (cdr indices)))))

  ;; Part One
  (define found 0)

  (do ((i 0 (+ i 1)))
      ((not (< i (vector-length input))))

      (do ((j 0 (+ j 1)))
          ((not (< j (vector-length (vector-ref* input i)))))

          (when (char=? #\X (vector-ref* input i j))
            (for-each (lambda (di dj)
                        (let loop ((i (+ i di))
                                   (j (+ j dj))
                                   (letters '(#\M #\A #\S)))
                          (cond
                            ((null? letters) (set! found (+ found 1)))

                            ((or (< i 0) (>= i (vector-length input))
                                 (< j 0) (>= j (vector-length (vector-ref* input i)))))

                            ((char=? (car letters)
                                     (vector-ref* input i j))
                             (loop (+ i di)
                                   (+ j dj)
                                   (cdr letters))))))

                      '(-1 -1  0  1  1  1  0 -1)
                      '( 0  1  1  1  0 -1 -1 -1)))))

  (display (format "Found instances of the word 'XMAS': ~A"
                   found))
  (newline)

  ;; Part Two
  (define found 0)

  (do ((i 0 (+ i 1)))
      ((not (< i (vector-length input))))

      (do ((j 0 (+ j 1)))
          ((not (< j (vector-length (vector-ref* input i)))))

          ;; Boolean operator abuse
          (when (and (not (or (< (- i 1) 0)
                              (>= (+ i 1) (vector-length input))
                              (< (- j 1) 0)
                              (>= (+ j 1) (vector-length (vector-ref* input i)))))
                     (char=? #\A (vector-ref* input i j))
                     (or (and (char=? #\M (vector-ref* input (- i 1) (- j 1)))
                              (char=? #\S (vector-ref* input (+ i 1) (+ j 1))))
                         (and (char=? #\S (vector-ref* input (- i 1) (- j 1)))
                              (char=? #\M (vector-ref* input (+ i 1) (+ j 1)))))
                     (or (and (char=? #\M (vector-ref* input (- i 1) (+ j 1)))
                              (char=? #\S (vector-ref* input (+ i 1) (- j 1))))
                         (and (char=? #\S (vector-ref* input (- i 1) (+ j 1)))
                              (char=? #\M (vector-ref* input (+ i 1) (- j 1))))))
            (set! found (+ found 1)))))

  (display (format "Number of X-MASes found: ~A"
                   found))
  (newline))

