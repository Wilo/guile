;; 2006 Per Bothner original.
;; 2012 Sunjoong Lee modified and rewrote to fit Guile 2.0.

;; Copyright (c) 2012 Sunjoong Lee
;; Copyright (c) 2005, 2006 Per Bothner
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-module (srfi srfi-64)
  :use-module (srfi srfi-9)             ; define-record-type
  :use-module (srfi srfi-35)            ; conditions
  :use-module (srfi srfi-39)            ; parameter objects
  :export (test-begin
           test-read-eval-string
           test-apply
           test-match-name
           test-match-nth
           test-match-any
           test-match-all
           test-skip
           test-expect-fail
           test-runner-group-path
           test-result-kind
           test-passed?
           test-result-ref
           test-result-set!
           test-result-remove
           test-result-clear
           test-log-to-file             ; not a part of the specification
           ;; Misc test-runner functions
           test-runner?
           test-runner-current
           test-runner-get
           test-runner-simple
           test-runner-null
           test-runner-create
           test-runner-factory
           test-runner-reset
           test-runner-test-name
           ;; test-runner field setter and getter functions
           ;; - see test-runner record definition:
           test-runner-pass-count
           test-runner-pass-count!      ; not a part of the specification
           test-runner-fail-count
           test-runner-fail-count!      ; not a part of the specification
           test-runner-xpass-count
           test-runner-xpass-count!     ; not a part of the specification
           test-runner-xfail-count
           test-runner-xfail-count!     ; not a part of the specification
           test-runner-skip-count
           test-runner-skip-count!      ; not a part of the specification
           test-runner-group-stack
           test-runner-group-stack!     ; not a part of the specification
           test-runner-on-test-begin
           test-runner-on-test-begin!
           test-runner-on-test-end
           test-runner-on-test-end!
           test-runner-on-group-begin
           test-runner-on-group-begin!
           test-runner-on-group-end
           test-runner-on-group-end!
           test-runner-on-final
           test-runner-on-final!
           test-runner-on-bad-count
           test-runner-on-bad-count!
           test-runner-on-bad-end-name
           test-runner-on-bad-end-name!
           test-result-alist
           test-result-alist!           ; not a part of the specification
           test-runner-aux-value
           test-runner-aux-value!
           ;; default/simple call-back functions,
           ;; used in default test-runner,
           ;; but can be called to construct more complex ones.
           test-on-test-begin-simple
           test-on-test-end-simple
           test-on-group-begin-simple
           test-on-group-end-simple
           test-on-bad-count-simple
           test-on-bad-end-name-simple
           test-on-final-simple)
  :export-syntax (test-end
                  test-group
                  test-group-with-cleanup
                  test-assert
                  test-equal
                  test-eqv
                  test-eq
                  test-approximate
                  test-error
                  test-with-runner))
(cond-expand-provide (current-module) '(srfi-64))

(define-record-type test-runner (%test-runner-alloc) test-runner?
  ;; Cumulate count of all tests that have passed and were expected to.
  (pass-count      test-runner-pass-count      test-runner-pass-count!)
  (fail-count      test-runner-fail-count      test-runner-fail-count!)
  (xpass-count     test-runner-xpass-count     test-runner-xpass-count!)
  (xfail-count     test-runner-xfail-count     test-runner-xfail-count!)
  (skip-count      test-runner-skip-count      test-runner-skip-count!)
  (skip-list       %test-runner-skip-list      %test-runner-skip-list!)
  (fail-list       %test-runner-fail-list      %test-runner-fail-list!)
  ;; Normally #t, except when in a test-apply.
  (run-list        %test-runner-run-list       %test-runner-run-list!)
  (skip-save       %test-runner-skip-save      %test-runner-skip-save!)
  (fail-save       %test-runner-fail-save      %test-runner-fail-save!)
  (group-stack     test-runner-group-stack     test-runner-group-stack!)
  (on-test-begin   test-runner-on-test-begin   test-runner-on-test-begin!)
  (on-test-end     test-runner-on-test-end     test-runner-on-test-end!)
  ;; Call-back when entering a group. Takes (runner suite-name count).
  (on-group-begin  test-runner-on-group-begin  test-runner-on-group-begin!)
  ;; Call-back when leaving a group.
  (on-group-end    test-runner-on-group-end    test-runner-on-group-end!)
  ;; Call-back when leaving the outermost group.
  (on-final        test-runner-on-final        test-runner-on-final!)
  ;; Call-back when expected number of tests was wrong.
  (on-bad-count    test-runner-on-bad-count    test-runner-on-bad-count!)
  ;; Call-back when name in test=end doesn't match test-begin.
  (on-bad-end-name test-runner-on-bad-end-name test-runner-on-bad-end-name!)
  ;; Cumulate count of all tests that have been done.
  (total-count     %test-runner-total-count    %test-runner-total-count!)
  ;; Stack (list) of (count-at-start . expected-count):
  (count-list      %test-runner-count-list     %test-runner-count-list!)
  (result-alist    test-result-alist           test-result-alist!)
  ;; Field can be used by test-runner for any purpose.
  ;; test-runner-simple uses it for a log file.
  (aux-value       test-runner-aux-value       test-runner-aux-value!))

(define (%source-line runner)
  (let ((result (test-result-alist runner)))
    (let ((file (assq 'source-file result))
          (line (assq 'source-line result)))
      (if line
          (string-append (or file "") ":" (number->string (cdr line)) ": ")
          ""))))

(define (%line form)
  (let ((source-form (datum->syntax form (syntax->datum form)))
        (source-file (assq-ref (syntax-source form) 'filename))
        (source-line (assq-ref (syntax-source form) 'line)))
    (cons (cons #'source-form source-form)
          (if source-file
              (cons (cons #'source-file (basename source-file))
                    (if source-line
                        ;; Line-number begins from 1 not 0.
                        (cons (cons #'source-line (1+ source-line)) '())
                        '()))
              '()))))

(define (test-match-name name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner))))

(define (test-match-nth n . count)
  (let ((i 0) (kount (if (null? count) 1 (car count))))
    (if (< 1 (length count))
        (let ((msg "Usage: (test-match-nth n) or (test-match-nth n count)"))
          (error msg))
        (lambda (runner)
          (set! i (+ i 1))
          (and (>= i n) (< i (+ n kount)))))))

(define test-match-any  'not-yet-defined)
(define test-match-all  'not-yet-defined)
(define test-skip       'not-yet-defined)
(define test-expect-fail'not-yet-defined)
(define %should-execute 'not-yet-defined)
(let ((%any
       (lambda ( . pred-list)
         (lambda (runner)
           (let ((result #f))
             (let loop ((lst pred-list))
               (if (null? lst)
                   result
                   (begin
                     (if ((car lst) runner)
                         (set! result #t))
                     (loop (cdr lst)))))))))
      (%all
       (lambda ( . pred-list)
         (lambda (runner)
           (let ((result #t))
             (let loop ((lst pred-list))
               (if (null? lst)
                   result
                   (begin
                     (if (not ((car lst) runner))
                         (set! result #f))
                     (loop (cdr lst)))))))))
      (%specifier
       (lambda (specifier)
         (cond ((procedure? specifier) specifier)
               ((integer? specifier) (test-match-nth 1 specifier))
               ((string? specifier) (test-match-name specifier))
               (else
                (error "not a valid test specifier"))))))
  (set!
   test-match-any
   (lambda (pred . args)
     (apply %any (%specifier pred) args)))
  (set!
   test-match-all
   (lambda (pred . args)
     (apply %all (%specifier pred) args)))
  (set!
   test-skip
   (lambda (pred . args)
     (let ((runner (test-runner-get)))
       (%test-runner-skip-list! runner
                                (cons (apply
                                       test-match-all (%specifier pred) args)
                                      (%test-runner-skip-list runner))))))
  (set!
   test-expect-fail
   (lambda (pred . args)
     (let ((runner (test-runner-get)))
       (%test-runner-fail-list! runner
                                (cons (apply
                                       test-match-all (%specifier pred) args)
                                      (%test-runner-fail-list runner))))))
  (set!
   ;; Returns #f, #t, or 'xfail.
   %should-execute
   (lambda (runner)
     (let ((run-list (%test-runner-run-list runner)))
       (cond ((or (not (or (eqv? run-list #t)
                           ((apply %any run-list) runner)))
                  ((apply %any (%test-runner-skip-list runner)) runner))
              (test-result-set! runner 'result-kind 'skip)
              #f)
             (((apply %any (%test-runner-fail-list runner)) runner)
              (test-result-set! runner 'result-kind 'xfail)
              'xfail)
             (else #t))))))

(define (test-result-ref runner pname . default)
  (let ((p (assq pname (test-result-alist runner))))
    (cond ((< 1 (length default))
           (let ((msg (string-append
                       "Usage: (test-result-ref runner pname) "
                       "or (test-result-ref runner pname default)")))
             (error msg)))
          (p (cdr p))
          ((not (null? default)) (car default))
          (else #f))))

(define (test-result-set! runner pname value)
  (let ((alist (test-result-alist runner)))
    (let ((p (assq pname alist)))
      (if p
          (set-cdr! p value)
          (test-result-alist! runner (cons (cons pname value) alist))))))

(define (test-result-remove runner pname)
  (let ((alist (test-result-alist runner)))
    (let ((p (assq pname alist)))
      (if p
          (test-result-alist! runner (let loop ((r alist))
                                       (if (eq? r p)
                                           (cdr r)
                                           (cons (car r) (loop (cdr r))))))))))

(define (test-result-clear runner)
  (test-result-alist! runner '()))

(define (test-runner-test-name runner)
  (test-result-ref runner 'test-name ""))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (test-runner-reset runner)
  (test-runner-pass-count!   runner 0)
  (test-runner-fail-count!   runner 0)
  (test-runner-xpass-count!  runner 0)
  (test-runner-xfail-count!  runner 0)
  (test-runner-skip-count!   runner 0)
  (%test-runner-total-count! runner 0)
  (%test-runner-count-list!  runner '())
  (%test-runner-run-list!    runner #t)
  (%test-runner-skip-list!   runner '())
  (%test-runner-fail-list!   runner '())
  (%test-runner-skip-save!   runner '())
  (%test-runner-fail-save!   runner '())
  (test-runner-group-stack!  runner '()))

;; Not part of the specification.  FIXME
;; Controls whether a log file is generated.
(define test-log-to-file (make-parameter #t))

(define test-on-test-begin-simple 'not-yet-defined)
(define test-on-test-end-simple   'not-yet-defined)
(let ((%display (lambda (pair port)
                  (display "  " port)
                  (display (car pair) port)
                  (display ": " port)
                  (write (cdr pair) port)
                  (newline port))))
  (set!
   test-on-test-begin-simple
   (lambda (runner)
     (let ((log (test-runner-aux-value runner)))
       (if (output-port? log)
           (let ((results (test-result-alist runner)))
             (let ((file (assq 'source-file results))
                   (line (assq 'source-line results))
                   (form (assq 'source-form results))
                   (name (assq 'test-name   results)))
               (display "Test begin:" log)
               (newline log)
               (if name (%display name log))
               (if file (%display file log))
               (if line (%display line log))
               (if form (%display form log))))))))
  (set!
   test-on-test-end-simple
   (lambda (runner)
     (let ((log (test-runner-aux-value runner))
           (kind (test-result-ref runner 'result-kind)))
       (if (output-port? log)
           (begin
             (display "Test end:" log)
             (newline log)
             (let loop ((alist (test-result-alist runner)))
               (if (pair? alist)
                   (let ((pair (car alist)))
                     ;; Write out properties not written out by on-test-begin.
                     (if (not
                          (memq
                           (car pair)
                           '(test-name source-file source-line source-form)))
                         (%display pair log))
                     (loop (cdr alist)))))))
       kind))))

(define (test-on-group-begin-simple runner suite-name count)
  (if (null? (test-runner-group-stack runner))
      (begin
        (display "%%%% Starting test ")
        (display suite-name)
        (let ((log-file (if (procedure? test-log-to-file)
                            (test-log-to-file)
                            test-log-to-file)))
          (if log-file
              (begin
                (if (not (output-port? log-file))
                    (let ((log-file-name (if (string? test-log-to-file)
                                             test-log-to-file
                                             (string-append
                                              suite-name ".log"))))
                      (set! log-file (open-output-file log-file-name))
                      (display "  (Writing full log to \"")
                      (display log-file-name)
                      (display "\")")))
                (test-runner-aux-value! runner log-file)
                (display "%%%% Starting test " log-file)
                (display suite-name log-file)
                (newline log-file))))
        (newline)))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
        (begin
          (display "Group begin: " log)
          (display suite-name log)
          (newline log)))))

(define (test-on-group-end-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
        (begin
          (display "Group end: " log)
          (display (car (test-runner-group-stack runner)) log)
          (newline log)))))

(define (test-on-final-simple runner)
  (let ((log (test-runner-aux-value runner))
        (pass-count (test-runner-pass-count runner))
        (xfail-count (test-runner-xfail-count runner))
        (xpass-count (test-runner-xpass-count runner))
        (fail-count (test-runner-fail-count runner))
        (skip-count (test-runner-skip-count runner)))
    (let ((%display
           (lambda (runner port)
             (let ((%display-if
                    (lambda (value label port)
                      (if (> value 0)
                          (begin
                            (display label port)
                            (display value port)
                            (newline port))))))
               (%display-if pass-count  "# of expected passes      " port)
               (%display-if xfail-count "# of expected failures    " port)
               (%display-if xpass-count "# of unexpected successes " port)
               (%display-if fail-count  "# of unexpected failures  " port)
               (%display-if skip-count  "# of skipped tests        " port)))))
      (if (output-port? log) (%display runner log))
      (%display runner (current-output-port))
      (list pass-count xfail-count xpass-count fail-count skip-count))))

(define (test-on-bad-count-simple runner count expected-count)
  (let ((log (test-runner-aux-value runner))
        (%display
         (lambda (count expected-count port)
           (display "*** Total number of tests was " port)
           (display count port)
           (display " but should be " port)
           (display expected-count port)
           (display ". ***" port)
           (display
            "*** Discrepancy indicates testsuite error or exceptions. ***" port)
           (newline port))))
    (if (output-port? log)
        (%display count expected-count log))
    (%display count expected-count (current-output-port))))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (let ((msg (string-append
              (%source-line runner) "test-end " begin-name
              " does not match test-begin " end-name)))
    (error msg)))

(define test-runner-current (make-parameter #f))
;; A safer wrapper to test-runner-current.
(define (test-runner-get)
  (let ((runner (test-runner-current)))
    (if (not runner)
        (error "test-runner not initialized - test-begin missing?"))
    runner))

(define (test-runner-simple)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-test-begin! runner test-on-test-begin-simple)
    (test-runner-on-test-end! runner test-on-test-end-simple)
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-bad-count! runner test-on-bad-count-simple)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

(define (test-runner-null)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-test-begin! runner (lambda (runner) #f))
    (test-runner-on-test-end! runner (lambda (runner) #f))
    (test-runner-on-group-begin! runner (lambda (runner name count) #f))
    (test-runner-on-group-end! runner (lambda (runner) #f))
    (test-runner-on-final! runner (lambda (runner) #f))
    (test-runner-on-bad-count! runner (lambda (runner count expected) #f))
    (test-runner-on-bad-end-name! runner (lambda (runner begin end) #f))
    runner))

(define test-runner-factory (make-parameter test-runner-simple))
(define (test-runner-create)
  ((test-runner-factory)))

(define (test-result-kind . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (test-result-ref runner 'result-kind)))

(define (test-passed? . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-get))))
    (memq (test-result-ref runner 'result-kind) '(pass xpass))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
           (lambda () (test-runner-current runner))
           (lambda () form ...)
           (lambda () (test-runner-current saved-runner)))))))

(define (test-apply first . rest)
  (if (test-runner? first)
      (test-with-runner first (apply test-apply rest))
      (let ((runner (test-runner-current)))
        (if runner
            (let ((run-list (%test-runner-run-list runner)))
              (cond ((null? rest)
                     (%test-runner-run-list! runner (reverse! run-list))
                     (first))           ; actually apply procedure thunk
                    (else
                     (%test-runner-run-list!
                      runner
                      (if (eq? run-list #t) (list first) (cons first run-list)))
                     (apply test-apply rest)
                     (%test-runner-run-list! runner run-list))))
            (let ((runner (test-runner-create)))
              (test-with-runner runner (apply test-apply first rest))
              ((test-runner-on-final runner) runner))))))

(define (test-begin suite-name . count)
  (if (not (test-runner-current)) (test-runner-current (test-runner-create)))
  (if (< 1 (length count))
      (let ((msg  (string-append
                   "Usage: (test-begin suite-name) "
                  "or (test-begin suite-name count)")))
        (error msg)))
  (let ((runner (test-runner-current))
        (kount (if (null? count) #f (car count))))
    ((test-runner-on-group-begin runner) runner suite-name kount)
    (%test-runner-skip-save! runner
                             (cons (%test-runner-skip-list runner)
                                   (%test-runner-skip-save runner)))
    (%test-runner-fail-save! runner
                             (cons (%test-runner-fail-list runner)
                                   (%test-runner-fail-save runner)))
    (%test-runner-count-list! runner
                              (cons (cons (%test-runner-total-count runner)
                                          kount)
                                    (%test-runner-count-list runner)))
    (test-runner-group-stack! runner
                              (cons suite-name
                                    (test-runner-group-stack runner)))))

(define (%test-end suite-name line)
  (let ((runner (test-runner-get)))
    (test-result-alist! runner line)
    (let ((groups (test-runner-group-stack runner))
          (count-list (%test-runner-count-list runner)))
      (if (null? groups)
          (let ((msg (string-append
                      (%source-line runner) "test-end not in a group")))
            (error msg)))
      (if (and suite-name (not (equal? suite-name (car groups))))
          ((test-runner-on-bad-end-name runner) runner suite-name (car groups)))
      (let ((expected-count (cdar count-list))
            (saved-count (caar count-list)))
        (let ((group-count (- (%test-runner-total-count runner) saved-count)))
          (if (and expected-count (not (= expected-count group-count)))
              ((test-runner-on-bad-count runner) runner
                                                 group-count expected-count))
          ((test-runner-on-group-end runner) runner)
          (test-runner-group-stack! runner
                                    (cdr (test-runner-group-stack runner)))
          (%test-runner-skip-list!  runner
                                    (car (%test-runner-skip-save runner)))
          (%test-runner-skip-save!  runner
                                    (cdr (%test-runner-skip-save runner)))
          (%test-runner-fail-list!  runner
                                    (car (%test-runner-fail-save runner)))
          (%test-runner-fail-save!  runner
                                    (cdr (%test-runner-fail-save runner)))
          (%test-runner-count-list! runner (cdr count-list))
          (if (null? (test-runner-group-stack runner))
              ((test-runner-on-final runner) runner)))))))

(define %test-assert 'not-yet-defined)
(define %test-comp   'not-yet-defined)
(define %test-error  'not-yet-defined)
(let ((%begin
       (lambda (runner)
         (%should-execute runner)
         ((test-runner-on-test-begin runner) runner)
         (not (eq? 'skip (test-result-ref runner 'result-kind)))))
      (%end
       (lambda (runner result)
         (test-result-set! runner
                           'result-kind
                           (if (eq? (test-result-ref runner 'result-kind)
                                    'xfail)
                               (if result 'xpass 'xfail)
                               (if result 'pass 'fail)))))
      (%report
       (lambda (runner kind)
         (case kind
           ((pass)
            (test-runner-pass-count! runner
                                     (1+ (test-runner-pass-count runner))))
           ((fail)
            (test-runner-fail-count! runner
                                     (1+ (test-runner-fail-count runner))))
           ((xpass)
            (test-runner-xpass-count! runner
                                      (1+ (test-runner-xpass-count runner))))
           ((xfail)
            (test-runner-xfail-count! runner
                                      (1+ (test-runner-xfail-count runner))))
           (else
            (test-runner-skip-count! runner
                                     (1+ (test-runner-skip-count runner)))))
         (%test-runner-total-count! runner
                                    (1+ (%test-runner-total-count runner)))
         ((test-runner-on-test-end runner) runner))))
  (set!
   %test-assert
   (lambda (eval-pair line)
     (let ((runner (test-runner-get))
           (value  (car eval-pair))
           (excpt  (cdr eval-pair)))
       (test-result-alist! runner line)
       (if (%begin runner)
           (if excpt
               (begin
                 (test-result-set! runner 'actual-error excpt)
                 (%end runner #f))
               (begin
                 (test-result-set! runner 'actual-value value)
                 (%end runner value))))
       (%report runner (test-result-ref runner 'result-kind)))))
  (set!
   %test-comp
   (lambda (pred-or-error expected eval-pair line)
     (let ((runner (test-runner-get))
           (value  (car eval-pair))
           (excpt  (cdr eval-pair)))
       (test-result-alist! runner line)
       (if (%begin runner)
           (begin
             (test-result-set! runner 'expected-value expected)
             (if excpt
                 (begin
                   (test-result-set! runner 'actual-error excpt)
                   (%end runner #f))
                 (begin
                   (test-result-set! runner 'actual-value value)
                   (%end runner (if (procedure? pred-or-error)
                                    (pred-or-error expected value)
                                    (and (>= value
                                             (- expected pred-or-error))
                                         (<= value
                                             (+ expected pred-or-error)))))))))
       (%report runner (test-result-ref runner 'result-kind)))))
  (set!
   %test-error
   (lambda (etype eval-pair line)
     (let ((runner (test-runner-get))
           (value  (car eval-pair))
           (excpt  (cdr eval-pair)))
       (test-result-alist! runner line)
       (if (%begin runner)
           (begin
             (test-result-set! runner 'expected-error etype)
             (if excpt
                 (begin
                   (test-result-set! runner 'actual-error excpt)
                   (%end runner (cond ((condition-type? etype)
                                       (and (condition? excpt)
                                            (condition-has-type? excpt etype)))
                                      ((procedure? etype) (etype excpt))
                                      ((symbol? etype) (equal? etype excpt))
                                      (else #t))))
                 (begin
                   (test-result-set! runner 'actual-value value)
                   (%end runner #f)))))
       (%report runner (test-result-ref runner 'result-kind))))))

(define (test-read-eval-string string)
  (let ((port (open-input-string string)))
    (let ((form (read port)))
      (if (eof-object? (read-char port))
          (primitive-eval form)
          (error "(not at eof)")))))

(define-syntax test-end
  (lambda (x)
    (syntax-case (list x (list (syntax quote) (%line x))) ()
      (((_) line)
       (syntax (%test-end #f line)))
      (((_ suite-name) line)
       (syntax (%test-end suite-name line))))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (let ((runner (test-runner-current)))
       ;; Ideally should also set line-number, if available.
       (test-result-alist! runner (list (cons 'test-name suite-name)))
       (if (%should-execute runner)
           (dynamic-wind
               (lambda () (test-begin suite-name))
               (lambda () . body)
               (lambda () (test-end  suite-name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
                 (dynamic-wind
                     (lambda () #f)
                     (lambda () form)
                     (lambda () cleanup-form))))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name #f cleanup-form))
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))

(define-syntax %eval-pair
  (syntax-rules ()
    ((_ expr)
     (let ((value #f) (excpt #f))
       (begin
         (set! value (catch #t
                            (lambda () expr)
                            (lambda (key . args) (set! excpt key))))
             (cons value excpt))))))

(define-syntax test-assert
  (lambda (x)
    (syntax-case (list x (list (syntax quote) (%line x))) ()
      (((_ tname expr) line)
       (syntax
        (%test-assert (%eval-pair expr) (cons (cons 'test-name tname) line))))
      (((_ expr) line)
       (syntax (%test-assert (%eval-pair expr) line))))))

(define-syntax test-equal
  (lambda (x)
    (syntax-case (list x (list (syntax quote) (%line x))) ()
      (((_ tname expected expr) line)
       (syntax
        (%test-comp equal? expected (%eval-pair expr)
                    (cons (cons 'test-name tname) line))))
      (((_ expected expr) line)
       (syntax (%test-comp equal? expected (%eval-pair expr) line))))))

(define-syntax test-eqv
  (lambda (x)
    (syntax-case (list x (list (syntax quote) (%line x))) ()
      (((_ tname expected expr) line)
       (syntax
        (%test-comp eqv? expected (%eval-pair expr)
                    (cons (cons 'test-name tname) line))))
      (((_ expected expr) line)
       (syntax (%test-comp eqv? expected (%eval-pair expr) line))))))

(define-syntax test-eq
  (lambda (x)
    (syntax-case (list x (list (syntax quote) (%line x))) ()
      (((_ tname expected expr) line)
       (syntax
        (%test-comp eq? expected (%eval-pair expr)
                    (cons (cons 'test-name tname) line))))
      (((_ expected expr) line)
       (syntax (%test-comp eq? expected (%eval-pair expr) line))))))

(define-syntax test-approximate
  (lambda (x)
    (syntax-case (list x (list (syntax quote) (%line x))) ()
      (((_ tname expected expr err) line)
       (syntax
        (%test-comp err expected (%eval-pair expr)
                    (cons (cons 'test-name tname) line))))
      (((_ expected expr err) line)
       (syntax (%test-comp err expected (%eval-pair expr) line))))))

(define-syntax test-error
  (lambda (x)
    (syntax-case (list x (list (syntax quote) (%line x))) ()
      (((_ tname etype expr) line)
       (syntax
        (%test-error etype (%eval-pair expr)
                     (cons (cons 'test-name tname) line))))
      (((_ etype expr) line)
       (syntax (%test-error etype (%eval-pair expr) line)))
      (((_ expr) line)
       (syntax (%test-error #t (%eval-pair expr) line))))))
