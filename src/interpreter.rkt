#lang racket
(require eopl)

;; C-Minus Interpreter with Enhanced Error Handling
;; Based on "Essentials of Programming Languages" style and conventions

(provide (all-defined-out))

;; =============================================================================
;; ERROR HANDLING STRUCTURES
;; =============================================================================

(define-struct runtime-error (type line message) #:transparent)

(define (raise-runtime-error type line message)
  (raise (make-runtime-error type line message)))

(define (format-error error)
  (format "Line ~a: ~a - ~a" 
          (runtime-error-line error)
          (runtime-error-type error)
          (runtime-error-message error)))

;; =============================================================================
;; DATA TYPES AND STRUCTURES
;; =============================================================================

;; Environment representation
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (var symbol?)
   (val reference?)
   (saved-env environment?))
  (extend-env-recursively
   (proc-names (list-of symbol?))
   (b-vars (list-of(list-of symbol?)))
   (proc-bodies (list-of  statement?))
   (saved-env environment?)))

;; Reference for mutable storage
(define-datatype reference reference?
;   (a-ref (position integer?) (vec vector?)))
  (a-ref (position integer?)))

;; Expressed values
(define-datatype expval expval?
  (num-val (value number?))
  (bool-val (value boolean?))
  (string-val (value string?))
  (array-val (value vector?))
  (proc-val (procedure proc?))
  (thunk-val (thunk thunk?)))

;; Procedures
(define-datatype proc proc?
  (procedure 
   (bvars (list-of symbol?))
  ;  (body expression?)
   (body statement?)
   (env environment?))
  
  (rec-procedure
   (proc-name symbol?)
   (bvars (list-of symbol?))
   (body statement?)
   (env environment?)
   ))

;; Thunks for lazy evaluation
(define-datatype thunk thunk?
  (a-thunk (exp expression?) (env environment?)))
  ; (evaluated-thunk (val expval?)))
; (define-struct thunk (exp env) #:transparent)

;; Function declarations for mutual recursion
(define-datatype func-decl func-decl?
  (a-func-decl
   (name symbol?)
   (params (list-of symbol?))
   (body expression?)
   (return-type symbol?)))

;; Non-local exit for `return`
(define-struct return-signal (value) #:transparent)

;; =============================================================================
;; STORE OPERATIONS
;; =============================================================================

(define the-store 'uninitialized)

(define empty-store
  (lambda () '()))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      (a-ref next-ref))))

(define deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos) 
             (if (>= pos (length the-store))
                 (raise-runtime-error "ReferenceError" 0 "Invalid reference")
                 (list-ref the-store pos))))))

(define setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos)
             (if (>= pos (length the-store))
                 (raise-runtime-error "ReferenceError" 0 "Invalid reference")
                 (set! the-store
                       (letrec
                         ((setref-inner
                           (lambda (store1 ref1)
                             (cond
                               ((null? store1) (raise-runtime-error "ReferenceError" 0 "Invalid reference"))
                               ((zero? ref1) (cons val (cdr store1)))
                               (else (cons (car store1) (setref-inner (cdr store1) (- ref1 1))))))))
                         (setref-inner the-store pos))))))))

;; =============================================================================
;; ENVIRONMENT OPERATIONS  
;; =============================================================================

(define (display-environment env)
  (printf "---- Start of Environment ----\n")
  (let loop ((env env))
    (cases environment env
      (empty-env () 
        (printf "---- End of Environment ----\n"))
      
      (extend-env (var ref saved-env)
        (printf "~a -> " var)
        (displayln (deref ref))
        (loop saved-env))
      (else (displayln "88888888888888888888888888888888888888888")))))

(define init-env empty-env)

(define apply-env
  (lambda (env search-sym line)
    (cases environment env
      (empty-env ()
                 (raise-runtime-error "NameError" line (format "Undefined variable: ~a" search-sym)))
      (extend-env (var val saved-env)
                  (if (eqv? search-sym var)
                      val
                      (apply-env saved-env search-sym line)))
      (extend-env-recursively (p-names b-vars p-bodies saved-env)
                              (let ((n (location search-sym p-names)))
                                (if n
                                    (newref
                                     (proc-val
                                      (rec-procedure
                                       (list-ref p-names n)
                                       (list-ref b-vars n)
                                       (list-ref p-bodies n)
                                       env)))
                                    (apply-env saved-env search-sym line)))
                              ))))

(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms)) => (lambda (n) (+ n 1)))
      (else #f))))

;; =============================================================================
;; THUNK OPERATIONS
;; =============================================================================

; (define value-of-thunk
;   (lambda (th)
;     (cases thunk th
;       (a-thunk (exp env)
;                (let ((val (value-of exp env)))
;                  (set-car! (cddr th) (evaluated-thunk val))
;                  val))
;       (evaluated-thunk (val) val))))

(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp env)
        ;; evaluate in the captured environment
        (value-of exp env (get-line-from exp))))))

; (define thunk->exp
;   (lambda (th)
;     (cases thunk th
;       (a-thunk (exp env) exp)
;       (evaluated-thunk (val) (raise-runtime-error "EvaluationError" 0 "Thunk already evaluated")))))

(define (deref-and-install! env var line)
  (let* ((ref (apply-env env var line))
                     (w (deref ref)))
                 (cond
                   ((expval? w) w) ; already a value
                   ((thunk? w) ; force, memoize, return
                    (let ((val (value-of-thunk w)))
                      (setref! ref val)
                      val))
                   (else
                   (raise-runtime-error "EvaluationError" line
                     (format "Location for ~a holds neither value nor thunk" var))))))

;; Force an arbitrary value (used for array slots)
(define (array-cell-deref v)
  (cond
    ((expval? v) v)
    ((thunk? v)
     (cases thunk v
       (a-thunk (exp th-env)
         (value-of exp th-env (get-line-from exp)))))
    (else (raise-runtime-error "EvaluationError" 0
                               "Unknown value (neither expval nor thunk)"))))
;; =============================================================================
;; EXPRESSED VALUE OPERATIONS
;; =============================================================================

(define expval->num
  (lambda (v line)
    (cases expval v
      (num-val (num) num)
      (else (raise-runtime-error "TypeError" line "Expected a number")))))

(define expval->bool
  (lambda (v line)
    (cases expval v
      (bool-val (bool) bool)
      (else (raise-runtime-error "TypeError" line "Expected a boolean")))))

(define expval->string
  (lambda (v line)
    (cases expval v
      (string-val (str) str)
      (else (raise-runtime-error "TypeError" line "Expected a string")))))

(define expval->array
  (lambda (v line)
    (cases expval v
      (array-val (arr) arr)
      (else (raise-runtime-error "TypeError" line "Expected an array")))))

(define expval->proc
  (lambda (v line)
    (cases expval v
      (proc-val (proc) proc)
      (else (raise-runtime-error "TypeError" line "Expected a function")))))

;; =============================================================================
;; TYPE OPERATIONS
;; =============================================================================

(define get-default-value
  (lambda (type-spec)
    (case type-spec
      ((int) (num-val 0))
      ((float) (num-val 0.0))
      ((string) (string-val ""))
      (else (raise-runtime-error "TypeError" 0 (format "Unknown type ~s" type-spec))))))

;; =============================================================================
;; MAIN INTERPRETER FUNCTIONS
;; =============================================================================

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (with-handlers ([runtime-error? (lambda (err) (format-error err))])
      (cases program pgm
        (a-program (decl-list)
                   (value-of-declaration-list decl-list (init-env)))))))

(define value-of-declaration-list
  (lambda (decl-list env)
    (if (null? decl-list)
        (num-val 0)  ; Empty program returns 0
        (let ((new-env (process-declarations decl-list env)))
          (find-and-call-main new-env)))))

(define process-declarations
  (lambda (decl-list env)
    (if (null? decl-list)
        env
        (let ((new-env (value-of-declaration (car decl-list) env)))
          (process-declarations (cdr decl-list) new-env)))))

(define value-of-declaration
  (lambda (decl env)
    (cases declaration decl
      (vardec-decl (var-decl) (value-of-var-declaration var-decl env))
      (fundec-decl (fun-decl) (value-of-fun-declaration fun-decl env)))))

(define value-of-var-declaration
  (lambda (var-decl env)
    (cases var-declaration var-decl
      (var-spec (type-spec id)
                (let ((default-val (get-default-value type-spec)))
                  (extend-env id (newref default-val) env)))
      (array-spec (type-spec id size)
                  (let* ((size-val (expval->num (value-of size env (get-line-from size)) (get-line-from size)))
                         (default-val (get-default-value type-spec))
                         (array (make-vector size-val default-val)))
                    (extend-env id (newref (array-val array)) env))))))

(define value-of-fun-declaration
  (lambda (fun-decl env)
    (cases fun-declaration fun-decl
      (fun-dcl (type-spec id params body)
               ; check if it is a recursive function
              ;  (display (statement? body))
               (if (is-fun-recursive body id) 
                   (let ((param-names (extract-param-names params)))
                     (extend-env-recursively
                      (list id)
                      (list param-names)
                      (list body)
                      env))
                   (let ((param-names (extract-param-names params)))
                     (extend-env id
                                 (newref (proc-val (procedure param-names body env)))
                                 env)))))))

; TODO: add recursive function definition
(define is-fun-recursive
  (lambda (stmt fun-id)
    ; (display fun-id)
    ; #f
    (cases statement stmt
      (compound-stmt (stmnt-lst)
                      (check-stmnts-for-recursive-call stmnt-lst fun-id)
                    ;  #f
                      )
      (else #f))
    )
  )

(define check-stmnts-for-recursive-call
  (lambda (stmt-lst fun-id)
    (if (equal? stmt-lst (list)) #f
        (if (does-stmnt-call-fun-id (list-ref stmt-lst 0) fun-id) #t
            (check-stmnts-for-recursive-call (cdr stmt-lst) fun-id))
        ; #f
        )))

(define does-stmnt-call-fun-id
  (lambda (stmnt fun-id)
    ; (display "this is the stmnt:\n")
    ; (display stmnt)
    ; (display "\n")
    ; (display "hello ")
    (cases statement stmnt
      (compound-stmt (stmt-lst) (check-stmnts-for-recursive-call stmt-lst fun-id))
      (if-stmt (test conseq alt) (or (does-stmnt-call-fun-id conseq fun-id)
                                     (or (does-exp-call-fun-id test fun-id) (does-stmnt-call-fun-id alt fun-id))))
      (while-stmt (test body) 
                  (or (does-exp-call-fun-id test fun-id) (does-stmnt-call-fun-id body fun-id)))
      (return-stmt (exp) (does-exp-call-fun-id exp fun-id))
      (expression-stmt (exp) (does-exp-call-fun-id exp fun-id))
      (else #f))))

(define does-exp-call-fun-id
  (lambda (exp fun-id)
    ; (display "hello ")
    (cases expression exp
      (var-exp (var) (equal? var fun-id))
      (array-ref-exp (var index) (does-exp-call-fun-id index fun-id))
      (assign-exp (var exp1) (does-exp-call-fun-id exp1 fun-id))
      (binary-op-exp (op exp1 exp2) (or (does-exp-call-fun-id exp1 fun-id) (does-exp-call-fun-id exp2 fun-id)))
      (unary-op-exp (op exp1) (does-exp-call-fun-id exp1 fun-id))
      (call-exp (rator rands) (or (does-exp-call-fun-id rator fun-id)
                                  (does-expLst-call-fun-id rands fun-id)))
      (print-exp (format args) (or (does-exp-call-fun-id format fun-id)
                                   (does-expLst-call-fun-id args fun-id)))
      (else #f))
    ))

(define does-expLst-call-fun-id
  (lambda (exp-lst fun-id)
    (if (equal? exp-lst (list)) #f 
        (if (does-exp-call-fun-id (list-ref exp-lst 0) fun-id) #t
           (does-expLst-call-fun-id (cdr exp-lst) fun-id)
           )
      ;  #f
       )))

(define extract-param-names
  (lambda (params)
    (cases params-list params
      (param-list (param-list)
                  (map extract-single-param-name param-list))
      (empty-params () '()))))

(define extract-single-param-name
  (lambda (par)
    (cases param par
      (argvar (type-spec id) id)
      (argarray (type-spec id) id))))

(define find-and-call-main
  (lambda (env)
    (let ((main-proc-ref (apply-env env 'main 0)))
      (let ((main-proc (expval->proc (deref main-proc-ref) 0)))
        (apply-procedure main-proc '() env 0)))))

;; =============================================================================
;; STATEMENT EVALUATION
;; =============================================================================

(define value-of-statement
  (lambda (stmt env)
    (cases statement stmt
      (compound-stmt (stmt-list)
                     (value-of-statement-list stmt-list env))
      (if-stmt (test conseq alt)
               (if (expval->bool (value-of test env (get-line-from test)) (get-line-from test))
                   (value-of-statement conseq env)
                   (value-of-statement alt env)))
      (while-stmt (test body)
                  ; (letrec ((loop (lambda ()
                                  ;  (if (expval->bool (value-of test env))
                                  ;      (begin
                                  ;        (value-of-statement body env)
                                  ;        (loop))
                                  ;      (num-val 0)))))
                  (letrec ((loop (lambda (curr-env)
                                  (if (expval->bool (value-of test curr-env (get-line-from test)) (get-line-from test))
                                      (let ((result (value-of-statement body curr-env)))
                                        (if (pair? result)
                                             (loop (cdr result))  ; Use updated environment
                                            (loop curr-env)))
                                      (num-val 0)))))
                    ; (loop)))
                    (loop env)))
      (return-stmt (exp)
                  ;  (value-of exp env (get-line-from exp)))
                  (raise (make-return-signal (value-of exp env (get-line-from exp)))))
      (empty-return ()
                    ; (num-val 0))
                    (raise (make-return-signal (num-val 0))))
      (expression-stmt (exp)
                       (value-of exp env (get-line-from exp)))
      (empty-exp ()
                 (num-val 0))
      (var-dec-stmt (var-decl)
                    (let ((new-env (value-of-var-declaration var-decl env)))
                       ;; Return both value and updated environment
                       (cons (num-val 0) new-env))))))

(define value-of-statement-list
  (lambda (stmt-list env)
    (if (null? stmt-list)
        (num-val 0)
        (if (null? (cdr stmt-list))
            ;; Last statement
            (let ((result (value-of-statement (car stmt-list) env)))
              (if (pair? result)
                  (car result)  ; Return value, discard environment
                  result))
            ;; Not last statement - thread environment through
            (let ((result (value-of-statement (car stmt-list) env)))
              (if (pair? result)
                  ;; Statement returned updated environment
                  (value-of-statement-list (cdr stmt-list) (cdr result))
                  ;; Statement didn't change environment
                  (value-of-statement-list (cdr stmt-list) env)))))))

;; =============================================================================
;; EXPRESSION EVALUATION
;; =============================================================================

(define value-of
  (lambda (exp env line)
    (cases expression exp
      (const-exp (num) (num-val num))
      (const-float-exp (num) (num-val num))
      (const-string-exp (str) (string-val str))
      (const-bool-exp (bool) (bool-val bool))
      ; (var-exp (var) (deref (apply-env env var line)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; using thunks instead
      ; (var-exp (var)
      ;          (let* ((ref (apply-env env var line))
      ;                (w (deref ref)))
      ;            (cond
      ;              ((expval? w) w) ; already a value
      ;              ((thunk? w) ; force, memoize, return
      ;               (let ((val (value-of-thunk w)))
      ;                 (setref! ref val)
      ;                 val))
      ;              (else
      ;              (raise-runtime-error "EvaluationError" line
      ;                (format "Location for ~a holds neither value nor thunk" var))))))
      (var-exp (var) (deref-and-install! env var line))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (array-ref-exp (var index)
                    ;  (let ((array-ref (apply-env env var line))
                     (let ((arr-val (deref-and-install! env var line))
                           (idx-val (value-of index env line)))
                      ;  (let ((array (expval->array (deref array-ref) line))
                       (let ((array (expval->array arr-val line))
                             (idx (expval->num idx-val line)))
                         (if (or (< idx 0) (>= idx (vector-length array)))
                             (raise-runtime-error "IndexError" line 
                                                  (format "Array index ~a out of bounds for array of size ~a" 
                                                          idx (vector-length array)))
                            ;  (vector-ref array idx)))))
                            (let ((cell (vector-ref array idx)))
                              (if (thunk? cell)
                                  (let ((res (array-cell-deref cell)))
                                    (vector-set! array idx res)  ; memoize in the slot
                                    res)
                                  cell))))))
      (assign-exp (var exp)
                  (let ((val (value-of exp env line)))
                    (cases variable var
                      (simple-var (id)
                                  (setref! (apply-env env id line) val)
                                  val)
                      (array-var (id index)
                                 (let ((array-ref (apply-env env id line))
                                       (idx-val (value-of index env line)))
                                   (let ((array (expval->array (deref array-ref) line))
                                         (idx (expval->num idx-val line)))
                                     (if (or (< idx 0) (>= idx (vector-length array)))
                                         (raise-runtime-error "IndexError" line 
                                                              (format "Array index ~a out of bounds for array of size ~a" 
                                                                      idx (vector-length array)))
                                         (begin
                                           (vector-set! array idx val)
                                           val))))))))
      (binary-op-exp (op exp1 exp2)
                     ; Short Circuit Implementation
                     (let ((val1 (value-of exp1 env line)))
                       (cases expval val1
                         (num-val (n1)
                           (if (and (equal? op '*) (equal? n1 0))
                               (num-val 0)
                               (let ((val2 (value-of exp2 env line)))
                                 (apply-binary-op op val1 val2 line))))
                         (bool-val (b1)
                           (if (and (equal? op '||) (equal? b1 #t))
                               (bool-val #t)
                               (if (and (equal? op '&&) (equal? b1 #f))
                                   (bool-val #f)
                                   (let ((val2 (value-of exp2 env line)))
                                     (apply-binary-op op val1 val2 line)))))
                         [else (let ((val2 (value-of exp2 env line)))
                                 (apply-binary-op op val1 val2 line))])))
      (unary-op-exp (op exp)
                    (let ((val (value-of exp env line)))
                      (apply-unary-op op val line)))
      (call-exp (rator rands)
                (let ((proc-val-exp (value-of rator env line))
                      ; (args (map (lambda (rand) (value-of rand env line)) rands)))
                      (thunk-args (map (lambda (rand) (a-thunk rand env)) rands)))
                  (let ((proc (expval->proc proc-val-exp line)))
                    (apply-procedure proc thunk-args env line))))
      (print-exp (format-str args)
                 (let ((str (expval->string (value-of format-str env line) line))
                       (arg-vals (map (lambda (arg) (value-of arg env line)) args)))
                   (apply-print str arg-vals line)
                   (string-val ""))))))

;; =============================================================================
;; OPERATOR APPLICATION
;; =============================================================================

(define (apply-binary-op op v1 v2 line)
  (cases expval v1
    (num-val (n1)
      (cases expval v2
        (num-val (n2)
          (case op
            [(+) (num-val (+ n1 n2))]
            [(-) (num-val (- n1 n2))]
            [(*) (num-val (* n1 n2))]
            [(/) (if (= n2 0)
                     (raise-runtime-error "DivideByZero" line "Division by zero")
                     (num-val (/ n1 n2)))]
            [(<)  (bool-val (< n1 n2))]
            [(<=) (bool-val (<= n1 n2))]
            [(>)  (bool-val (> n1 n2))]
            [(>=) (bool-val (>= n1 n2))]
            [(==) (bool-val (= n1 n2))]
            [(!=) (bool-val (not (= n1 n2)))]
            [else (raise-runtime-error "TypeError" line
                      (format "Unsupported op ~a on numbers" op))]))
        [else (raise-runtime-error "TypeError" line
                 (format "Mismatched types: number ~a and ~a" n1 v2))]))

    (string-val (s1)
      (cases expval v2
        (string-val (s2)
          (case op
            [(+)  (string-val (string-append s1 s2))]
            [(==) (bool-val (string=? s1 s2))]
            [(!=) (bool-val (not (string=? s1 s2)))]
            [(<)  (bool-val (string<? s1 s2))]
            [(<=) (bool-val (string<=? s1 s2))]
            [(>)  (bool-val (string>? s1 s2))]
            [(>=) (bool-val (string>=? s1 s2))]
            [else (raise-runtime-error "TypeError" line
                      (format "Unsupported op ~a on strings" op))]))
        [else (raise-runtime-error "TypeError" line
                 (format "Mismatched types: string ~a and ~a" s1 v2))]))

    (bool-val (b1)
      (cases expval v2
        (bool-val (b2)
          (case op
            [(==) (bool-val (equal? b1 b2))]
            [(!=) (bool-val (not (equal? b1 b2)))]
            [(||) (bool-val (or b1 b2))]
            [(&&) (bool-val (and b1 b2))]
            [else (raise-runtime-error "TypeError" line
                      (format "Unsupported op ~a on booleans" op))]))
        [else (raise-runtime-error "TypeError" line
                 (format "Mismatched types: boolean ~a and ~a" b1 v2))]))

    [else (raise-runtime-error "TypeError" line
             (format "Unsupported operand type for ~a: ~a" op v1))]))

(define apply-unary-op
  (lambda (op val line)
    (case op
      ((-) (num-val (- (expval->num val line))))
      ((not) (bool-val (not (expval->bool val line))))
      (else (raise-runtime-error "OperatorError" line (format "Unknown unary operator: ~s" op))))))

;; =============================================================================
;; PROCEDURE APPLICATION
;; =============================================================================

(define apply-procedure
  (lambda (proc1 args env line)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (if (not (= (length vars) (length args)))
                     (raise-runtime-error "ArityError" line 
                                          (format "Function expects ~a arguments but got ~a" 
                                                  (length vars) (length args)))
                     (let ((new-env (extend-env* vars (map newref args) saved-env)))
                       (with-handlers ([return-signal?
                               (lambda (rs) (return-signal-value rs))])
                         (value-of-statement body new-env)))))
      (rec-procedure (name vars body saved-env)
                     (if (not (= (length vars) (length args)))
                         (raise-runtime-error "ArityError" line 
                                              (format "Function expects ~a arguments but got ~a" 
                                                      (length vars) (length args)))
                         (let ((new-env (extend-env* vars (map newref args) saved-env)))
                           (with-handlers ([return-signal?
                               (lambda (rs) (return-signal-value rs))])
                             (value-of-statement body new-env))))))))    

(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env (car vars) (car vals)
                    (extend-env* (cdr vars) (cdr vals) env)))))

;; =============================================================================
;; PRINT FUNCTION
;; =============================================================================

(define apply-print
  (lambda (format-str args line)
    (letrec ((print-helper
              (lambda (str arg-list)
                (cond
                  ((null? arg-list) (displayln str))
                  ((string-contains str "~")
                   (let ((pos (string-index str #\~)))
                     (display (substring str 0 pos))
                     (display (format-arg (car arg-list) line))
                     (print-helper (substring str (+ pos 2)) (cdr arg-list))))
                  (else (displayln str))))))
      (print-helper format-str args))))

(define format-arg
  (lambda (val line)
    (cases expval val
      (num-val (n) (number->string n))
      (bool-val (b) (if b "true" "false"))
      (string-val (s) s)
      (else (raise-runtime-error "TypeError" line "Cannot format value for printing")))))

(define string-contains
  (lambda (str substr)
    (string-index str (string-ref substr 0))))

(define string-index
  (lambda (str ch)
    (letrec ((helper
              (lambda (i)
                (cond
                  ((>= i (string-length str)) #f)
                  ((char=? (string-ref str i) ch) i)
                  (else (helper (+ i 1)))))))
      (helper 0))))

;; =============================================================================
;; GRAMMAR DEFINITIONS (matching parser output)
;; =============================================================================

(define-datatype program program?
  (a-program (decl-list (list-of declaration?))))

(define-datatype declaration declaration?
  (vardec-decl (var-decl var-declaration?))
  (fundec-decl (fun-decl fun-declaration?)))

(define-datatype var-declaration var-declaration?
  (var-spec (type-spec symbol?) (id symbol?))
  (array-spec (type-spec symbol?) (id symbol?) (size expression?)))

(define-datatype fun-declaration fun-declaration?
  (fun-dcl (type-spec symbol?) (id symbol?) (params params-list?) (body statement?)))

(define-datatype params-list params-list?
  (param-list (params (list-of param?)))
  (empty-params))

(define-datatype param param?
  (argvar (type-spec symbol?) (id symbol?))
  (argarray (type-spec symbol?) (id symbol?)))

(define-datatype statement statement?
  (compound-stmt (stmt-list (list-of statement?)))
  (if-stmt (test expression?) (conseq statement?) (alt statement?))
  (while-stmt (test expression?) (body statement?))
  (return-stmt (exp expression?))
  (empty-return)
  (expression-stmt (exp expression?))
  (empty-exp)
  (var-dec-stmt (var-decl var-declaration?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (const-float-exp (num number?))
  (const-string-exp (str string?))
  (const-bool-exp (bool boolean?))
  (var-exp (var symbol?))
  (array-ref-exp (var symbol?) (index expression?))
  (assign-exp (var variable?) (exp expression?))
  (binary-op-exp (op symbol?) (exp1 expression?) (exp2 expression?))
  (unary-op-exp (op symbol?) (exp expression?))
  (call-exp (rator expression?) (rands (list-of expression?)))
  (print-exp (format-str expression?) (args (list-of expression?))))

(define-datatype variable variable?
  (simple-var (id symbol?))
  (array-var (id symbol?) (index expression?)))

;; =============================================================================
;; LINE NUMBER EXTRACTION (PLACEHOLDER)
;; =============================================================================

;; This is a placeholder function - you'll need to modify your parser
;; to include line number information in the AST
(define (get-line-from ast-node)
  ;; For now, return 0 as a placeholder
  ;; You should modify your parser to include line numbers in the AST
  0)

;; =============================================================================
;; PARSER OUTPUT CONVERTER
;; =============================================================================

(define (convert-parser-output parser-output)
  (convert-program parser-output))

;; Helper
; (define (collect-decls dl)
;   ; (display (list-ref dl 3))
;   ; (display dl)
;   ; (display "\n")
;   (if (null? dl)
;       '()
      ; (if (null? (cddr dl))
      ;     (cons (list (car dl) (cadr dl))
      ;           '())
      ;     (cons (list (car dl) (cadr dl))
      ;           (collect-decls (car (cddr dl)))))))
                ; (collect-decls (cddr dl))))))

;; Convert a program
(define (convert-program pgm)
  (match pgm
    [(list 'program decl-list)
    ;  (a-program (map convert-declaration (collect-decls decl-list)))]))
     (a-program (map convert-declaration decl-list))]))
    ;  (a-program (convert-declaration-list decl-list))]))

; (define (convert-declaration-list decl-list)
;   (match decl-list
;     [(list decl)
;      ()]
;    [(list decl decl-list)]))

;; Convert a declaration (var or fun)
(define (convert-declaration decl)
  (match decl
    [(list 'vardec var-decl)
     (vardec-decl (convert-var-declaration var-decl))]
    [(list 'fundec fun-decl)
     (fundec-decl (convert-fun-declaration fun-decl))]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid declaration")]))

;; Convert a variable declaration
(define (convert-var-declaration vardecl)
  (match vardecl
    [(list 'var-spec type id)
     (var-spec type id)]
    [(list 'array-spec type id size)
     (array-spec type id (convert-expression size))]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid variable declaration")]))

;; Convert a function declaration
(define (convert-fun-declaration fundecl)
  (match fundecl
    [(list 'fun-dcl type id params body)
     (fun-dcl type id
              (convert-params params)
              (convert-statement body))]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid function declaration")]))

;; Convert params
(define (convert-params params)
  (match params
    [(list 'param-list plist)
     (param-list (map convert-param plist))]
    ['() (empty-params)]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid parameter list")]))

(define (convert-param p)
  (match p
    [(list 'argvar type id) (argvar type id)]
    [(list 'argarray type id) (argarray type id)]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid parameter")]))

;; Convert statements
(define (convert-statement st)
  (match st
    [(list 'compound-stmnt stmts)
     (compound-stmt (map convert-statement stmts))]
    [(list 'if test conseq 'else alt)
     (if-stmt (convert-expression test)
              (convert-statement conseq)
              (convert-statement alt))]
    [(list 'while test body)
     (while-stmt (convert-expression test)
                 (convert-statement body))]
    [(list 'return exp)
     (return-stmt (convert-expression exp))]
    [(list 'empty-return) (empty-return)]
    [(list 'empty-exp) (empty-exp)]
    [(list 'var-spec type id) 
     (var-dec-stmt (var-spec type id))]
    [(list 'array-spec type id size)
     (var-dec-stmt (array-spec type id (convert-expression size)))]
    [exp
     (expression-stmt (convert-expression exp))]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid statement")]))

;; Convert expressions
(define (convert-expression exp)
  (match exp
    [(? number? n) (const-exp n)]
    [(? string? s) (const-string-exp s)]
    [(list 'True) (const-bool-exp #t)]
    [(list 'False) (const-bool-exp #f)]
    [(list 'call (list 'print (list 'string fmt args)))
     (print-exp (const-string-exp fmt)
                (map convert-expression args))]
    [(list 'call (list id args))
     (call-exp (var-exp id) (map convert-expression args))]
    [(list var '= rhs)
     (assign-exp (convert-variable var)
                 (convert-expression rhs))]
    [(list id 'array-access index)
     (array-ref-exp id (convert-expression index))]
    [(list op lhs rhs) #:when (symbol? op)
     (binary-op-exp op
                    (convert-expression lhs)
                    (convert-expression rhs))]
    [(list lhs op rhs) #:when (symbol? op)
     (binary-op-exp op
                    (convert-expression lhs)
                    (convert-expression rhs))]
    [(list inner-exp) (convert-expression inner-exp)]
    [(list op e) #:when (symbol? op)
     (unary-op-exp op (convert-expression e))]
    [id #:when (symbol? id)
     (var-exp id)]
    [(list id) #:when (symbol? id)
     (var-exp id)]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid expression")]))

;; Convert variables
(define (convert-variable v)
  (match v
    [(? symbol? id) (simple-var id)]
    [(list id 'array-access index)
     (array-var id (convert-expression index))]
    [_ (raise-runtime-error "SyntaxError" 0 "Invalid variable")]))

;; =============================================================================
;; MAIN INTERFACE
;; =============================================================================

(define run
  (lambda (parser-output)
    (with-handlers ([runtime-error? (lambda (err) (format-error err))])
      (value-of-program (convert-parser-output parser-output)))))