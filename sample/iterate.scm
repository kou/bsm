#!/usr/bin/env gosh

(use benchmark)
(use srfi-1)

(define n 500000)
(define 1..n (iota n))
(define a #f)

(bm (lambda (r)
      (let ((td (report r (lambda ()
                            (dotimes (i n #f)
                              (set! a "1")))
                        :label "dotimes:"))
            (tf (report r (lambda ()
                            (for-each (lambda (x)
                                        (set! a "1"))
                                      1..n))
                        :label "for-each:"))
            (tm (report r (lambda ()
                            (map (lambda (x)
                                   (set! a "1"))
                                 1..n))
                        :label "map:")))
        (list (fold benchmark-time-add td (list tf tm))
              (benchmark-time-div (fold benchmark-time-add td (list tf tm))
                                  3))))
    :label-width 10
    :labels '(">total:" ">avg:"))
