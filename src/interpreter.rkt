#lang racket
(require eopl)

;; C-Minus Interpreter
;; Based on "Essentials of Programming Languages" style and conventions

(provide (all-defined-out))

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
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
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
   (env environment?)))

;; Thunks for lazy evaluation
(define-datatype thunk thunk?
  (a-thunk (exp expression?) (env environment?))
  (evaluated-thunk (val expval?)))

;; Function declarations for mutual recursion
(define-datatype func-decl func-decl?
  (a-func-decl
   (name symbol?)
   (params (list-of symbol?))
   (body expression?)
   (return-type symbol?)))

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
      (a-ref (pos) (list-ref the-store pos)))))

(define setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos)
             (set! the-store
                   (letrec
                     ((setref-inner
                       (lambda (store1 ref1)
                         (cond
                           ((null? store1) (eopl:error 'setref "Invalid reference ~s" ref))
                           ((zero? ref1) (cons val (cdr store1)))
                           (else (cons (car store1) (setref-inner (cdr store1) (- ref1 1))))))))
                     (setref-inner the-store pos)))))))
;; =============================================================================
;; ENVIRONMENT OPERATIONS  
;; =============================================================================

; (define init-env
;   (lambda ()
;     (extend-env 'i (newref (num-val 1))
;                 (extend-env 'v (newref (num-val 5))
;                             (extend-env 'x (newref (num-val 10))
;                                         (empty-env))))))

(define init-env empty-env)

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (var val saved-env)
                  (if (eqv? search-sym var)
                      val
                      (apply-env saved-env search-sym)))
      (extend-env-recursively (p-names b-vars p-bodies saved-env)
                              (let ((n (location search-sym p-names)))
                                (if n
                                    (newref
                                     (proc-val
                                      (procedure
                                       (list-ref b-vars n)
                                       (list-ref p-bodies n)
                                       env)))
                                    (apply-env saved-env search-sym)))))))

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

(define thunk->exp
  (lambda (th)
    (cases thunk th
      (a-thunk (exp env) exp)
      (evaluated-thunk (val) (eopl:error 'thunk->exp "Thunk already evaluated")))))

;; =============================================================================
;; EXPRESSED VALUE OPERATIONS
;; =============================================================================

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      ; (thunk-val (th) (expval->num (value-of-thunk th)))
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      ; (thunk-val (th) (expval->bool (value-of-thunk th)))
      (else (expval-extractor-error 'bool v)))))

(define expval->string
  (lambda (v)
    (cases expval v
      (string-val (str) str)
      ; (thunk-val (th) (expval->string (value-of-thunk th)))
      (else (expval-extractor-error 'string v)))))

(define expval->array
  (lambda (v)
    (cases expval v
      (array-val (arr) arr)
      ; (thunk-val (th) (expval->array (value-of-thunk th)))
      (else (expval-extractor-error 'array v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      ; (thunk-val (th) (expval->proc (value-of-thunk th)))
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; =============================================================================
;; TYPE OPERATIONS
;; =============================================================================

(define get-default-value
  (lambda (type-spec)
    (case type-spec
      ((int) (num-val 0))
      ((float) (num-val 0.0))
      ((string) (string-val ""))
      (else (eopl:error 'get-default-value "Unknown type ~s" type-spec)))))

;; =============================================================================
;; MAIN INTERPRETER FUNCTIONS
;; =============================================================================

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (decl-list)
                 (value-of-declaration-list decl-list (init-env))))))

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
                  (let* ((size-val (expval->num (value-of size env)))
                         (default-val (get-default-value type-spec))
                         (array (make-vector size-val default-val)))
                    (extend-env id (newref (array-val array)) env))))))

(define value-of-fun-declaration
  (lambda (fun-decl env)
    (cases fun-declaration fun-decl
      (fun-dcl (type-spec id params body)
               (let ((param-names (extract-param-names params)))
                 (extend-env id
                             (newref (proc-val (procedure param-names body env)))
                             env))))))

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
    (let ((main-proc-ref (apply-env env 'main)))
      (let ((main-proc (expval->proc (deref main-proc-ref))))
        (apply-procedure main-proc '() env)))))

;; =============================================================================
;; STATEMENT EVALUATION
;; =============================================================================

(define value-of-statement
  (lambda (stmt env)
    (cases statement stmt
      (compound-stmt (stmt-list)
                     (value-of-statement-list stmt-list env))
      (if-stmt (test conseq alt)
               (if (expval->bool (value-of test env))
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
                                  (if (expval->bool (value-of test curr-env))
                                       (let ((result (value-of-statement body curr-env)))
                                         (if (pair? result)
                                             (loop (cdr result))  ; Use updated environment
                                             (loop curr-env)))
                                       (num-val 0)))))
                    ; (loop)))
                    (loop env)))
      (return-stmt (exp)
                   (value-of exp env))
      (empty-return ()
                    (num-val 0))
      (expression-stmt (exp)
                       (value-of exp env))
      (empty-exp ()
                 (num-val 0))
      (var-dec-stmt (var-decl)
                    (let ((new-env (value-of-var-declaration var-decl env)))
                       ;; Return both value and updated environment
                       (cons (num-val 0) new-env)))))) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define value-of-statement-list
;   (lambda (stmt-list env)
;     (if (null? stmt-list)
;         (num-val 0)
;         (if (null? (cdr stmt-list))
;             (value-of-statement (car stmt-list) env)
;             (begin
;               (value-of-statement (car stmt-list) env)
;               (value-of-statement-list (cdr stmt-list) env))))))

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
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (const-float-exp (num) (num-val num))
      (const-string-exp (str) (string-val str))
      (const-bool-exp (bool) (bool-val bool))
      (var-exp (var) (deref (apply-env env var)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; using thunks instead
      ; (var-exp (var) (let ((w (deref (apply-env env var)))))
      ;                   (if (expval? w)
      ;                       w
      ;                       (let ((val1 (value-of-thunk w)))
      ;                         (begin
      ;                           (setref! ref1 val1)
      ;                           val1))))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (array-ref-exp (var index)
                     (let ((array (expval->array (deref (apply-env env var))))
                           (idx (expval->num (value-of index env))))
                       (vector-ref array idx)))
      (assign-exp (var exp)
                  (let ((val (value-of exp env)))
                    (cases variable var
                      (simple-var (id)
                                  (setref! (apply-env env id) val)
                                  val)
                      (array-var (id index)
                                 (let ((array (expval->array (deref (apply-env env id))))
                                       (idx (expval->num (value-of index env))))
                                   (vector-set! array idx val)
                                   val)))))
      (binary-op-exp (op exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (apply-binary-op op val1 val2)))
      (unary-op-exp (op exp)
                    (let ((val (value-of exp env)))
                      (apply-unary-op op val)))
      (call-exp (rator rands)
                (let ((proc (expval->proc (value-of rator env)))
                      (args (map (lambda (rand) (value-of rand env)) rands)))
                  (apply-procedure proc args env)))
      (print-exp (format-str args)
                 (let ((str (expval->string (value-of format-str env)))
                       (arg-vals (map (lambda (arg) (value-of arg env)) args)))
                   (apply-print str arg-vals)
                   (string-val ""))))))

;; =============================================================================
;; OPERATOR APPLICATION
;; =============================================================================

(define apply-binary-op
  (lambda (op val1 val2)
      (case op
        ((+) (num-val (+ (expval->num val1) (expval->num val2))))
        ((-) (num-val (- (expval->num val1) (expval->num val2))))
        ((*) (num-val (* (expval->num val1) (expval->num val2))))
        ((/) (num-val (/ (expval->num val1) (expval->num val2))))
        ((>) (bool-val (> (expval->num val1) (expval->num val2))))
        ((<) (bool-val (< (expval->num val1) (expval->num val2))))
        ((>=) (bool-val (>= (expval->num val1) (expval->num val2))))
        ((<=) (bool-val (<= (expval->num val1) (expval->num val2))))
        ((==) (bool-val (= (expval->num val1) (expval->num val2))))
        ((!=) (bool-val (not (= (expval->num val1) (expval->num val2)))))
        ((&&) (bool-val (and (expval->bool val1) (expval->bool val2))))
        ((||) (bool-val (or (expval->bool val1) (expval->bool val2))))
        (else (eopl:error 'apply-binary-op "Unknown operator: ~s" op)))))

(define apply-unary-op
  (lambda (op val)
    (case op
      ((-) (num-val (- (expval->num val))))
      ((not) (bool-val (not (expval->bool val))))
      (else (eopl:error 'apply-unary-op "Unknown unary operator: ~s" op)))))

;; =============================================================================
;; PROCEDURE APPLICATION
;; =============================================================================

(define apply-procedure
  (lambda (proc1 args env)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (let ((new-env (extend-env* vars (map newref args) saved-env)))
                  ;  (value-of body new-env))))))
                  (value-of-statement body new-env))))))

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
  (lambda (format-str args)
    (letrec ((print-helper
              (lambda (str arg-list)
                (cond
                  ((null? arg-list) (displayln str))
                  ((string-contains str "~")
                   (let ((pos (string-index str #\~)))
                     (display (substring str 0 pos))
                     (display (format-arg (car arg-list)))
                     (print-helper (substring str (+ pos 2)) (cdr arg-list))))
                  (else (displayln str))))))
      (print-helper format-str args))))

(define format-arg
  (lambda (val)
    (cases expval val
      (num-val (n) (number->string n))
      (bool-val (b) (if b "true" "false"))
      (string-val (s) s)
      (else "<?>"))))

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
;; GRAMMAR DEFINITIONS (matching your parser output)
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
  (var-dec-stmt (var-decl var-declaration?)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; ;; =============================================================================
; ;; PARSER OUTPUT CONVERTER
; ;; =============================================================================

; ;; Convert parser output to internal AST representation
; (define convert-parser-output
;   (lambda (parser-output)
;     (convert-program parser-output)))

; (define convert-program
;   (lambda (pgm)
;     (cases list pgm
;       ((program decl-list)
;        (a-program (map convert-declaration decl-list))))))

; (define convert-declaration
;   (lambda (decl)
;     (cases list decl
;       ((vardec var-decl)
;        (vardec-decl (convert-var-declaration var-decl)))
;       ((fundec fun-decl)
;        (fundec-decl (convert-fun-declaration fun-decl))))))

; ;; Additional converter functions would be implemented here...


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
    [_ (eopl:error 'convert-declaration "Invalid declaration: ~s" decl)]))

;; Convert a variable declaration
(define (convert-var-declaration vardecl)
  (match vardecl
    [(list 'var-spec type id)
     (var-spec type id)]
    [(list 'array-spec type id size)
     (array-spec type id (convert-expression size))]))

;; Convert a function declaration
(define (convert-fun-declaration fundecl)
  (match fundecl
    [(list 'fun-dcl type id params body)
     (fun-dcl type id
              (convert-params params)
              (convert-statement body))]))

;; Convert params
(define (convert-params params)
  (match params
    [(list 'param-list plist)
     (param-list (map convert-param plist))]
    ['() (empty-params)]))

(define (convert-param p)
  (match p
    [(list 'argvar type id) (argvar type id)]
    [(list 'argarray type id) (argarray type id)]))

;; Convert statements
(define (convert-statement st)
  (match st
    [(list 'compound-stmnt stmts)
     (compound-stmt (map convert-statement stmts))
     ]
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
    ;  (var-dec-stmt (var-spec type id))]
     (var-dec-stmt (var-spec type id))]
    [(list 'array-spec type id size)
     (var-dec-stmt (array-spec type id (convert-expression size)))]
    [exp
     (expression-stmt (convert-expression exp))]))

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
    ; [(list 'print (list 'string fmt args))
    ;  (print-exp (const-string-exp fmt)
    ;             (map convert-expression args))]
    [(list var '= rhs)
     (assign-exp (convert-variable var)
                 (convert-expression rhs))]
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
     (var-exp id)]  ; Handle (a), (b) from parser's (var) rule
    ; [(list inner-exp) (convert-expression inner-exp)]  ; Handle other parenthesized expressions
    [_ (eopl:error 'convert-expression "Invalid expression: ~s" exp)]))

;; Convert variables
(define (convert-variable v)
  (match v
    [(? symbol? id) (simple-var id)]
    [(list id "[" index "]")
     (array-var id (convert-expression index))]))



;; =============================================================================
;; MAIN INTERFACE
;; =============================================================================

(define run
  (lambda (parser-output)
    (value-of-program (convert-parser-output parser-output))))

;; Example usage:
; (convert-parser-output '(program ((fundec (fun-dcl int main () (compound-stmt ()))))))
; (convert-parser-output '(program (fundec (fun-dcl int main () (compound-stmnt ())))))

; (run '(program (fundec (fun-dcl int main () (compound-stmnt ()))))) ;wasn't commented

; (convert-parser-output '(program (fundec (fun-dcl int main () (compound-stmnt ((call (print (string "hello woooorld" ())))))))))

; (run '(program (fundec (fun-dcl int main () (compound-stmnt ((call (print (string "hello woooorld" ())))))))));wasn't commented
; (convert-parser-output '(program (fundec (fun-dcl int salam () (compound-stmnt ())) (fundec (fun-dcl int main () (compound-stmnt ((call (salam ())))))))));wasn't commented
; (convert-parser-output '(program (fundec (fun-dcl int salam () (compound-stmnt ())) (fundec (fun-dcl int main () (compound-stmnt ((call (print (string "sd" ()))))))))));wasn't commented

; (convert-parser-output '(program (fundec (fun-dcl int main () (compound-stmnt ((print (string "hello woooorld" ()))))))))
; (run '(program (fundec (fun-dcl int main () (compound-stmnt ((print (string "hello woooorld" ()))))))))
; (run '(program (fundec (fun-dcl int main () (compound-stmnt ((call (print (string "hello woooorld" ())))))))))
; (run '(program (fundec (fun-dcl int main (param-list ((argvar int a) (argarray int b))) (compound-stmnt ())))))
; (collect-decls '(fundec (fun-dcl int salam () (compound-stmnt ())) (fundec (fun-dcl int main (param-list ((argvar int a) (argvar string b))) (compound-stmnt ((var-spec int x) (call (print (string "what the hell" ())))))))))
; (convert-parser-output '(program (fundec (fun-dcl int main () (compound-stmnt ((var-spec int a) (a = 2) (var-spec int b) (b = (a + 1)) (return b)))))))

; (run '(program (fundec (fun-dcl int main () (compound-stmnt ((var-spec int a) (a = 7) (var-spec int b) (b = ((a) + 9)) (return (a))))))));wasn't commented
; (convert-parser-output '(program (fundec (fun-dcl int main () (compound-stmnt ((var-spec int a)))))));wasn't commented
; (convert-parser-output '(program (fundec (fun-dcl int main () (compound-stmnt ((var-spec int a) (a = 3)))))));wasn't commented
; (run '(program (fundec (fun-dcl int main () (compound-stmnt ((var-spec int a) (a = 3)))))));wasn't commented