#!/bin/env -S csi -script
(module (aoc day3) ()
  (import scheme
          (chicken base)
          (chicken format)
          (chicken io)
          (chicken irregex)
          (chicken process-context)
          (chicken string)
          (srfi 1))

  (when (null? (command-line-arguments))
    (display "Please pass the input file as the first argument" (current-error-port))
    (newline (current-error-port))
    (exit 1))

  (define found-instructions
    (with-input-from-file (car (command-line-arguments))
      (lambda ()
        (irregex-extract
          `(seq (or (seq "mul(" (** 1 3 numeric) "," (** 1 3 numeric) ")")
                    "do()"
                    "don't()"))
          (read-string)))))

  ;; Part One
  (display (format "The sum of valid mul() instructions is: ~A"
                   (apply +
                          (map
                            (compose
                              (cut apply * <>)
                              (cut map string->number <>)
                              (cut irregex-extract '(** 1 3 numeric) <>))
                            (filter (cut substring=? <> "mul")
                                    found-instructions)))))
  (newline)

  ;; Part Two
  (display (format "The sum of valid mul() instructions is: ~A"
                   (apply +
                          (map
                            (compose
                              (cut apply * <>)
                              (cut map string->number <>)
                              (cut irregex-extract '(** 1 3 numeric) <>))
                            (let loop ((instructions found-instructions)
                                       (filtered '())
                                       (do-flag #t))
                              (cond
                                ((null? instructions) filtered)

                                ((string=? (car instructions) "do()")
                                 (loop (cdr instructions)
                                       filtered
                                       #t))

                                ((string=? (car instructions) "don't()")
                                 (loop (cdr instructions)
                                       filtered
                                       #f))

                                (do-flag (loop (cdr instructions)
                                               (cons (car instructions)
                                                     filtered)
                                               do-flag))

                                (else (loop (cdr instructions)
                                            filtered
                                            do-flag))))))))
  (newline))

