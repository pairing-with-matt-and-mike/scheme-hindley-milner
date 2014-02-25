(define (apply? node) (eq? (car node) 'apply))
(define (fn? node) (eq? (car node) 'fn))
(define variable? symbol?)
(define fn-type? list?)

(define (infer-type* env node)
  (cond ((number? node) 'Number)
        ((string? node) 'String)
        ((variable? node) (cadr (assoc node env)))
        ((fn? node)
         (let* ((formal-arg      (cadr node))
                (formal-arg-type (car formal-arg))
                (formal-arg-name (cadr formal-arg))
                (body-env (cons `(,formal-arg-name ,formal-arg-type) env)))
           `(,(caadr node)
             ,(infer-type* body-env (caddr node)))))
        ((apply? node)
         (let ((callee-type (infer-type* env (cadr node))))
           (if (fn-type? callee-type)
               (if (equal? (infer-type* env (caddr node)) (car callee-type))
                   (cadr callee-type)
                   #f)
               #f)))))

(define (infer-type node) (infer-type* '() node))

(define (assert-equal expected actual)
  (if (not(equal? expected actual))
      (printf "expected ~s but was ~s\n" expected actual)
      (print "OK")))

(assert-equal 'Number (infer-type 1))

(assert-equal 'String (infer-type "blah"))

(assert-equal '(String Number) (infer-type '(fn (String x) 1)))

(assert-equal 'Number (infer-type '(apply (fn (String x) 1) "asdf")))

(assert-equal #f (infer-type '(apply (fn (String x) 1) 1)))

(assert-equal #f (infer-type '(apply 1 1)))

(assert-equal '(Number Number) (infer-type '(fn (Number x) x)))

(assert-equal 'Number (infer-type
                       '(apply
                         (fn ((Number Number) f) (apply f 1))
                         (fn (Number x) x))))
