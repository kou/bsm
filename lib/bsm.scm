(define-module bsm
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use gauche.sequence)
  (export benchmark bm report
          benchmark-measure

          benchmark-time-add
          benchmark-time-sub
          benchmark-time-mul
          benchmark-time-div))
(select-module bsm)

(define *benchmark-format-string*
  "~4.5u ~4.5y ~4.5t ~4.5r\n")
(define *benchmark-caption*
  "      user     system      total       real\n")

(define-class <benchmark-time> ()
  ((utime :init-keyword :utime :accessor utime-of)
   (stime :init-keyword :stime :accessor stime-of)
   (cutime :init-keyword :cutime :accessor cutime-of)
   (cstime :init-keyword :cstime :accessor cstime-of)
   (real :init-keyword :real :accessor real-of)
   (total :accessor total-of)
   (label :init-keyword :label :accessor label-of)))

(define-method initialize ((self <benchmark-time>) args)
  (next-method)
  (set! (total-of self)
        (+ (utime-of self)
           (stime-of self)
           (cutime-of self)
           (cstime-of self)
           (real-of self))))

(define-method benchmark-time-add ((self <benchmark-time>) (num <number>))
  (update self + num))
(define-method benchmark-time-sub ((self <benchmark-time>) (num <number>))
  (update self - num))
(define-method benchmark-time-mul ((self <benchmark-time>) (num <number>))
  (update self * num))
(define-method benchmark-time-div ((self <benchmark-time>) (num <number>))
  (update self / num))


(define-method benchmark-time-add ((self <benchmark-time>)
                                   (other <benchmark-time>))
  (update self + other))
(define-method benchmark-time-sub ((self <benchmark-time>)
                                   (other <benchmark-time>))
  (update self - other))
(define-method benchmark-time-mul ((self <benchmark-time>)
                                   (other <benchmark-time>))
  (update self * other))
(define-method benchmark-time-div ((self <benchmark-time>)
                                   (other <benchmark-time>))
  (update self / other))

(define-method benchmark-time-add ((self <benchmark-time>) (thunk <procedure>))
  (benchmark-time-add self (benchmark-measure thunk)))
(define-method benchmark-time-sub ((self <benchmark-time>) (thunk <procedure>))
  (benchmark-time-sub self (benchmark-measure thunk)))
(define-method benchmark-time-mul ((self <benchmark-time>) (thunk <procedure>))
  (benchmark-time-mul self (benchmark-measure thunk)))
(define-method benchmark-time-div ((self <benchmark-time>) (thunk <procedure>))
  (benchmark-time-div self (benchmark-measure thunk)))


(define-method update ((self <benchmark-time>) proc (other <benchmark-time>))
  (make <benchmark-time>
    :utime (proc (utime-of self) (utime-of other))
    :stime (proc (stime-of self) (stime-of other))
    :cutime (proc (cutime-of self) (cutime-of other))
    :cstime (proc (cstime-of self) (cstime-of other))
    :real (proc (real-of self) (real-of other))
    :label (label-of self)))

(define-method update ((self <benchmark-time>) proc obj)
  (make <benchmark-time>
    :utime (proc (utime-of self) obj)
    :stime (proc (stime-of self) obj)
    :cutime (proc (cutime-of self) obj)
    :cstime (proc (cstime-of self) obj)
    :real (proc (real-of self) obj)
    :label (label-of self)))


(define-method update! ((self <benchmark-time>) proc obj)
  (let ((new (update self proc obj)))
    (set! (utime-of self) (utime-of new))
    (set! (stime-of self) (stime-of new))
    (set! (cutime-of self) (cutime-of new))
    (set! (cstime-of self) (cstime-of new))
    (set! (real-of self) (real-of new))
    (set! (total-of self) (total-of new))))


;; (define-method write-object ((self <benchmark-time>) out)
;;   (format out (benchmark-time-format self)))

(define-method x->string ((self <benchmark-time>))
  (benchmark-time-format self))

(define benchmark-time-format-subst-infos
  (map (lambda (fmt-str accessor)
         (cons (string->regexp #`"~(\\d*)(?:\\.(\\d+))?,|fmt-str|")
               accessor))
       '("u" "y" "U" "Y" "t" "r")
       (list utime-of stime-of cutime-of cstime-of total-of real-of)))

(define (benchmark-time-format bt . args)
  (define (format-bt prefix postfix accessor)
    (format #f #`",|prefix|,|postfix|" (accessor bt)))

  (define (format-bt-delta-time before-point-size after-point-size accessor)
    (let*-values (((frac sec) (modf (accessor bt)))
                  ((ignore ifrac) (modf (+ (* frac 1000) 0.5))))
      (format #f #`"~,|before-point-size|d.~,|after-point-size|,,'0d"
              (inexact->exact sec) (inexact->exact ifrac))))

  (define (subst-bt-format string)
    (fold (lambda (info str)
            (regexp-replace-all (car info)
                                str
                                (lambda (md)
                                  (format-bt-delta-time (md 1)
                                                        (md 2)
                                                        (cdr info)))))
          string
          benchmark-time-format-subst-infos))

  (let-keywords* args ((format-string #f)
                       (format-args '())
                       (have-user-format-string? (not (eq? #f format-string))))
    (let* ((str (or format-string *benchmark-format-string*))
           (str (regexp-replace-all #/(~[,\d]*)n/
                                    str
                                    (lambda (md)
                                      (format-bt (md 1) "a" label-of))))
           (str (subst-bt-format str)))
      (if have-user-format-string?
        (apply format #f str format-args)
        str))))

(define-class <benchmark-report> ()
  ((width :init-keyword :width :accessor width-of)
   (format-string :init-keyword :format-string :accessor format-string-of)))

(define-method initialize ((self <benchmark-report>) args)
  (next-method)
  (set! (width-of self) (x->integer (width-of self))))

(define-method report ((self <benchmark-report>) thunk . args)
  (let-keywords* args ((label "")
                       (format-args '()))
    (display (string-pad-right label (width-of self)))
    (let ((bt (benchmark-measure thunk)))
      (display (benchmark-time-format bt
                                      :format-string (format-string-of self)
                                      :format-args format-args))
      bt)))

(define (benchmark-measure thunk . args)
  (let-keywords* args ((label ""))
    (let*-values (((stimes) (sys-times))
                  ((sreal-sec sreal-msec) (sys-gettimeofday))
                  (r (thunk))
                  ((etimes) (sys-times))
                  ((ereal-sec ereal-msec) (sys-gettimeofday)))
      (make <benchmark-time>
        :label label
        :utime (/ (- (list-ref etimes 0) (list-ref stimes 0))
                  (list-ref stimes 4))
        :stime (/ (- (list-ref etimes 1) (list-ref stimes 1))
                  (list-ref stimes 4))
        :cutime (/ (- (list-ref etimes 2) (list-ref stimes 2))
                  (list-ref stimes 4))
        :cstime (/ (- (list-ref etimes 3) (list-ref stimes 3))
                   (list-ref stimes 4))
        :real (- (+ ereal-sec (/ ereal-msec 1000000))
                 (+ sreal-sec (/ sreal-msec 1000000)))))))

(define (benchmark proc . args)
  (let-keywords* args ((caption "")
                       (label-width #f)
                       (format-string #f)
                       (format-args #f)
                       (labels '()))
    (let* ((output (current-output-port))
           (buffering-mode (port-buffering output)))
      (if buffering-mode
        (set! (port-buffering output) :none))
      (display caption)
      (let* ((label-width (or label-width 0))
             (format-string (or format-string *benchmark-format-string*))
             (format-args (or format-args '()))
             (results (proc (make <benchmark-report>
                              :width label-width
                              :format-string format-string))))
        (if (list? results)
          (for-each-with-index
           (lambda (i bt)
             (display (string-pad-right (or (list-ref labels i #f)
                                            (label-of bt)
                                            "")
                                        label-width))
             (display (benchmark-time-format bt
                                             :format-string format-string
                                             :format-args format-args)))
           results)))
      (if buffering-mode
        (set! (port-buffering output) buffering-mode)))))

(define (bm proc . args)
  (let-keywords* args ((label-width 0)
                       (labels '()))
    (benchmark proc
               :caption (string-append (make-string label-width #\space)
                                       *benchmark-caption*)
               :label-width label-width
               :format-string *benchmark-format-string*
               :format-args '()
               :labels labels)))

;;; unused
(define-class <benchmark-job> ()
  ((width :init-keyword :width :accessor width-of)
   (jobs :init-keyword :jobs :accessor jobs-of)))

(define-method report ((self <benchmark-job>) thunk . args)
  (let-optionals* args ((label ""))
    (let* ((label (string-append label " "))
           (width (string-size label)))
      (when (< (width-of self) width)
        (set! (width-of self) width))
      (push! (jobs-of self) (list label thunk))
      self)))


(provide "bsm")
