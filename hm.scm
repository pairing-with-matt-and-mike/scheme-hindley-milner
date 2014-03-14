(define (assert-equal expected actual)
  (if (not (equal? expected actual))
      (printf "expected ~s but was ~s\n" expected actual)
      (print "OK " expected)))

(define (apply? node) (eq? (car node) 'apply))
(define (fn? node) (eq? (car node) 'fn))
(define variable? symbol?)
(define fn-type? list?)
(define free-type? number?)


(define (append-propogate-failure first second)
  (if (and first second) (append first second) #f))

(define (unify-types first-type second-type)
  (cond ((equal? first-type second-type) '())
        ((and (free-type? first-type) (free-type? second-type))
         `((,(max first-type second-type) ,(min first-type second-type))))
        ((free-type? first-type) `((,first-type ,second-type)))
        ((free-type? second-type) `((,second-type ,first-type)))
        ((and (fn-type? first-type) (fn-type? second-type))
         (append-propogate-failure
          (unify-types (car first-type) (car second-type))
          (unify-types (cadr first-type) (cadr second-type))))
        (#t #f)))

(define (assoc-value key list)
  (let ((pair (assoc key list)))
    (and pair (cadr pair))))

(define (resolve-types-once type-mapping type)
  (cond ((not type-mapping) #f)
        ((free-type? type) (or (assoc-value type type-mapping) type))
        ((fn-type? type)
         `(,(resolve-types type-mapping (car type))
           ,(resolve-types type-mapping (cadr type))))
        (#t type)))

(define (resolve-types type-mapping type)
  (let ((type* (resolve-types-once type-mapping type)))
    (if (equal? type type*) type (resolve-types type-mapping type*))))

(define (unified-types first-type second-type)
  (let ((type-mapping (unify-types first-type second-type)))
    (resolve-types type-mapping first-type)))

(assert-equal '() (unify-types 'Number 'Number))

(assert-equal '((0 Number)) (unify-types 0 'Number))

(assert-equal '((0 Number)) (unify-types 'Number 0))

(assert-equal '((1 0)) (unify-types 0 1))

(assert-equal '((1 0)) (unify-types 1 0))

(assert-equal #f (unify-types 'Number 'String))

(assert-equal '((0 String)) (unify-types '(0 Number) '(String Number)))

(assert-equal '((0 String)) (unify-types '(Number 0) '(Number String)))

(assert-equal '((0 Number) (1 String)) (unify-types '(0 1) '(Number String)))

(assert-equal #f (unify-types '(Number Number) '(String Number)))

(assert-equal #f (unify-types '(Number Number) 'Number))

(assert-equal #f (unify-types 'Number '(Number Number)))

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
                (callee-type-mapping (unify-types callee-type* abstract-callee-type))
                (callee-type (resolve-types callee-type-mapping callee-type*)))
           (if (fn-type? callee-type)
               (let* ((formal-arg-type (car callee-type))
                      (actual-arg-type (infer-type* env (+ 2 free-type-counter) (caddr node)))
                      (arg-type-mapping (unify-types formal-arg-type actual-arg-type))
                      (type-mapping (append callee-type-mapping arg-type-mapping))
                      (return-type (cadr callee-type)))
                 (resolve-types type-mapping return-type))
               #f)))))

(define (infer-type node) (infer-type* '() 0 node))

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
