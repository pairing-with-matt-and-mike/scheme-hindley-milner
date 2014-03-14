(define (apply? node) (eq? (car node) 'apply))
(define (fn? node) (eq? (car node) 'fn))
(define variable? symbol?)
(define fn-type? list?)
(define free-type? number?)

(define (unify-types first-type second-type)
  (cond ((equal? first-type second-type)
         first-type)
        ((free-type? first-type) second-type)
        ((free-type? second-type) first-type)
        ((and (fn-type? first-type) (fn-type? second-type))
         `(,(unify-types (car first-type) (car second-type))
           ,(unify-types (cadr first-type) (cadr second-type))))
        (#t #f)))

(define (infer-type* env free-type-counter node)
  (cond ((number? node) 'Number)
        ((string? node) 'String)
        ((variable? node) (cadr (assoc node env)))
        ((fn? node)
         (let* ((formal-arg-name (cadr node))
                (body-env (cons `(,formal-arg-name ,free-type-counter) env)))
           `(,free-type-counter
             ,(infer-type* body-env (+ 1 free-type-counter) (caddr node)))))
        ((apply? node)
         (let* ((callee-type* (infer-type* env free-type-counter (cadr node)))
                (abstract-callee-type `(,free-type-counter ,(+ 1 free-type-counter)))
                (callee-type (unify-types callee-type* abstract-callee-type)))
           (if (fn-type? callee-type)
               (let* ((formal-arg-type (car callee-type))
                      (actual-arg-type (infer-type* env (+ 2 free-type-counter) (caddr node)))
                      (concrete-arg-type (unify-types formal-arg-type actual-arg-type))
                      (return-type (cadr callee-type)))
                 (if (free-type? return-type)
                     concrete-arg-type
                     return-type))
               #f)))))

(define (infer-type node) (infer-type* '() 0 node))

(define (assert-equal expected actual)
  (if (not (equal? expected actual))
      (printf "expected ~s but was ~s\n" expected actual)
      (print "OK " expected)))

(assert-equal 'Number          (infer-type 1))

(assert-equal 'String          (infer-type "blah"))

(assert-equal '(0 Number)      (infer-type '(fn x 1)))

(assert-equal '(0 (1 Number))  (infer-type '(fn x (fn y 1))))

(assert-equal 'Number          (infer-type '(apply (fn x 1) "asdf")))

(assert-equal #f               (infer-type '(apply 1 1)))

(assert-equal '(0 0)           (infer-type '(fn x x)))

(assert-equal 'Number          (infer-type '(apply (fn x x) 1)))

(assert-equal '((Number 0) 0)  (infer-type '(fn f (apply f 1))))

(assert-equal 'Number          (infer-type
                                '(apply
                                  (fn f (apply f 1))
                                  (fn x x))))
