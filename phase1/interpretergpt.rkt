#lang racket
(require eopl)

;; C-Minus Interpreter (with store-based thunk memoization)
;; Fixes:
;; 1) Thunks are memoized by storing thunk records in the mutable store and
;;    updating via setref!, avoiding illegal set-car! on datatype values.
;; 2) Environment bindings store integer references (matching newref/deref),
;;    so the environment datatype expects integer? for values.

(provide (all-defined-out))

;; =============================================================================
;; DATA TYPES AND STRUCTURES
;; =============================================================================

;; Environment representation (values are integer refs into the store)
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (var symbol?)
   (val integer?)
   (saved-env environment?))
  (extend-env-recursively
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
   (saved-env environment?)))

;; NOTE: We keep a reference datatype around if you want it later, but
;; the store API below uses integer indices as references.
;; (Unused by the rest of this file.)
(define-datatype reference reference?
  (a-ref (position integer?) (vec vector?)))

;; Expressed values
(define-datatype expval expval?
  (num-val (value number?))
  (bool-val (value boolean?))
  (string-val (value string?))
  (array-val (value vector?))
  (proc-val (procedure proc?))
  ;; Store-based thunks: hold an integer ref pointing to a thunk record in store
  ; (thunk-val (ref integer?)))
  )

;; Procedures
(define-datatype proc proc?
  (procedure 
   (bvars (list-of symbol?))
   (body expression?)
   (env environment?)))

;; Thunks for lazy evaluation (records that live *inside* the store)
(define-datatype thunk thunk?
  (a-thunk (exp expression?) (env environment?))
  (evaluated-thunk (val expval?)))

(define-datatype thunk-val thunk-val?
  (thunk-val (ref integer?)))

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
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec
            ((setref-inner
              (lambda (store1 ref1)
                (cond
                  ((null? store1) (eopl:error 'setref "Invalid reference ~s" ref))
                  ((zero? ref1) (cons val (cdr store1)))
                  (else (cons (car store1) (setref-inner (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

;; =============================================================================
;; ENVIRONMENT OPERATIONS  
;; =============================================================================

(define init-env
  (lambda ()
    (extend-env 'i (newref (num-val 1))
                (extend-env 'v (newref (num-val 5))
                            (extend-env 'x (newref (num-val 10))
                                        (empty-env))))))

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
;; THUNK OPERATIONS (store-based memoization)
;; =============================================================================

;; Create a thunk in the store and return an expval wrapping its ref
;; helper: allocate a thunk in the store
(define (make-thunk exp env)
  (thunk-val (newref (a-thunk exp env))))

(define (value-of-thunk th)
  (cases thunk-val th
    (thunk-val (ref)
      (let ((cell (deref ref)))
        (cases thunk cell
          (a-thunk (exp env)
            (let ((val (value-of exp env)))
              (setref! ref (evaluated-thunk val))
              val))
          (evaluated-thunk (val) val))))))

(define thunk->exp
  (lambda (th-ref)
    (let ((cell (deref th-ref)))
      (cases thunk cell
        (a-thunk (exp env) exp)
        (evaluated-thunk (val)
                         (eopl:error 'thunk->exp "Thunk already evaluated"))))))

;; =============================================================================
;; EXPRESSED VALUE OPERATIONS
;; =============================================================================

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (thunk-val (ref) (expval->num (value-of-thunk ref)))
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (thunk-val (ref) (expval->bool (value-of-thunk ref)))
      (else (expval-extractor-error 'bool v)))))

(define expval->string
  (lambda (v)
    (cases expval v
      (string-val (str) str)
      (thunk-val (ref) (expval->string (value-of-thunk ref)))
      (else (expval-extractor-error 'string v)))))

(define expval->array
  (lambda (v)
    (cases expval v
      (array-val (arr) arr)
      (thunk-val (ref) (expval->array (value-of-thunk ref)))
      (else (expval-extractor-error 'array v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (thunk-val (ref) (expval->proc (value-of-thunk ref)))
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
  (lambda (param)
    (cases param param
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
                  (letrec ((loop (lambda ()
                                   (if (expval->bool (value-of test env))
                                       (begin
                                         (value-of-statement body env)
                                         (loop))
                                       (num-val 0)))))
                    (loop)))
      (return-stmt (exp)
                   (value-of exp env))
      (empty-return ()
                    (num-val 0))
      (expression-stmt (exp)
                       (value-of exp env))
      (empty-exp ()
                 (num-val 0)))))

(define value-of-statement-list
  (lambda (stmt-list env)
    (if (null? stmt-list)
        (num-val 0)
        (if (null? (cdr stmt-list))
            (value-of-statement (car stmt-list) env)
            (begin
              (value-of-statement (car stmt-list) env)
              (value-of-statement-list (cdr stmt-list) env))))))

;; =============================================================================
;; EXPRESSION EVALUATION
;; =============================================================================

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (const-float-exp (num) (num-val num))
      (const-string-exp (str) (string-val str))
      (var-exp (var) (deref (apply-env env var)))
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
    (let ((num1 (expval->num val1))
          (num2 (expval->num val2)))
      (case op
        ((+) (num-val (+ num1 num2)))
        ((-) (num-val (- num1 num2)))
        ((*) (num-val (* num1 num2)))
        ((/) (num-val (/ num1 num2)))
        ((>) (bool-val (> num1 num2)))
        ((<) (bool-val (< num1 num2)))
        ((>=) (bool-val (>= num1 num2)))
        ((<=) (bool-val (<= num1 num2)))
        ((==) (bool-val (= num1 num2)))
        ((!=) (bool-val (not (= num1 num2))))
        ((&&) (bool-val (and (expval->bool val1) (expval->bool val2))))
        ((||) (bool-val (or (expval->bool val1) (expval->bool val2))))
        (else (eopl:error 'apply-binary-op "Unknown operator: ~s" op))))))

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
                   (value-of body new-env))))))

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
                  ((null? arg-list) (display str))
                  ((string-contains str "~")
                   (let ((pos (string-index str #\~)))
                     (display (substring str 0 pos))
                     (display (format-arg (car arg-list)))
                     (print-helper (substring str (+ pos 2)) (cdr arg-list))))
                  (else (display str))))))
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
  (empty-exp))

(define-datatype expression expression?
  (const-exp (num number?))
  (const-float-exp (num number?))
  (const-string-exp (str string?))
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
;; PARSER OUTPUT CONVERTER
;; =============================================================================

;; Convert parser output to internal AST representation
(define convert-parser-output
  (lambda (parser-output)
    (convert-program parser-output)))

(define convert-program
  (lambda (pgm)
    (cases list pgm
      ((program decl-list)
       (a-program (map convert-declaration decl-list))))))

(define convert-declaration
  (lambda (decl)
    (cases list decl
      ((vardec var-decl)
       (vardec-decl (convert-var-declaration var-decl)))
      ((fundec fun-decl)
       (fundec-decl (convert-fun-declaration fun-decl))))))

;; Additional converter functions would be implemented here...

;; =============================================================================
;; MAIN INTERFACE
;; =============================================================================

(define run
  (lambda (parser-output)
    (value-of-program (convert-parser-output parser-output))))

;; Example usage:
;; (run '(program ((fundec (fun-dcl int main () (compound-stmt ()))))))
