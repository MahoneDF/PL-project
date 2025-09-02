#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "Lexer.rkt")
(provide (all-defined-out))
(define our-parser (parser
    (start program)
    (end EOF)
    (error (lambda (tok-ok? tok-name tok-value)
         (printf "Parse error at token ~a (value: ~a)\n" tok-name tok-value)
         (error 'parser "Cannot continue after error")))
    (tokens value-tokens empty-tokens)
    (debug "debugging_log.log")
    (grammar
        (program
            [(declaration-list) (list 'program $1)])
        (declaration-list
            ; [(declaration-list declaration) (dclist $1 $2)]
            [(declaration-list declaration) (append $1 (list $2))]
            ; [(declaration) (decl $1)])
            [(declaration) (list $1)])
        (declaration 
            [(var-declaration) (list 'vardec $1)]
            [(fun-declaration) (list 'fundec $1)])
        (var-declaration
            [(type-spec ID LEFTBR expression RIGHTBR SEMICOLON) (list 'array-spec $1 $2 $4)]
            [(type-spec ID SEMICOLON) (list 'var-spec $1 $2)])
        (type-spec 
            [(INT) 'int]
            [(FLOAT) 'float]
            [(STRING) 'string]
            [(BOOL) 'bool])
        (fun-declaration
            [(type-spec ID LEFTPAR params RIGHTPAR compound-stmnt) (list 'fun-dcl $1 $2 $4 $6)])
        (params
            [(param-list) (list 'param-list $1)]
            [() '()])
        (param-list
            [(param-list COMMA param) (append $1 $3)]
            [(param) $1])
        (param
            [(type-spec ID LEFTBR RIGHTBR) (list (list 'argarray $1 $2))]
            [(type-spec ID) (list (list 'argvar $1 $2))])
        (compound-stmnt
            [(LEFTVILI stmnt-list RIGHTVILI) (list 'compound-stmnt $2)])
        ; (local-dec 
            ; [(local-dec var-declaration) (append $1 (list $2))]
            ; [(local-dec var-declaration) (append $1 (list $2))]
            ; [() '()])
        (stmnt-list
            ; [(stmnt-list stmnt) (append $1 (list $2))]
            [(stmnt-list stmnt) (append $1 (list $2))]
            [() '()])
        (stmnt
            [(compound-stmnt) $1]
            [(cond-stmnt) $1]
            [(iter-stmnt) $1]
            [(return-stmnt) $1]
            [(expression-stmnt) $1]
            [(var-declaration) $1])
        (cond-stmnt
            [(IF LEFTPAR expression RIGHTPAR compound-stmnt ELSE compound-stmnt) (list 'if $3 $5 'else $7)]
            ; [(IF LEFTPAR expression RIGHTPAR stmnt) (list 'if $3 $5)])
        )
        (iter-stmnt
            [(WHILE LEFTPAR expression RIGHTPAR compound-stmnt) (list 'while $3 $5)])
        (return-stmnt
            [(RETURN SEMICOLON) (list 'empty-return)]
            [(RETURN expression SEMICOLON) (list 'return $2)])
        (expression-stmnt
            [(expression SEMICOLON) $1]
            [(SEMICOLON) (list 'empty-exp)])
        (expression
            [(var ASSIGN expression) (list $1 '= $3)]
            [(simple-expression) $1])
        (var
            [(ID) $1]
            [(ID LEFTBR expression RIGHTBR) (list $1 'array-access $3)])
        (simple-expression
            [(simple-expression log-op simple-expression) (list $1 $2 $3)]
            [(logical-expression) $1]
            [(temp-expression) $1])
        (temp-expression
         [(TRUE) (list 'True)]
         [(FALSE) (list 'False)])
        (rel-op 
            [(GT) '>]
            [(LT) '<]
            [(GEQ) '>=]
            [(LEQ) '<=]
            [(EQ) '==]
            [(NEQ) '!=])
        (logical-expression
            [(additive-expression rel-op additive-expression) (list $2 $1 $3)]
            [(additive-expression) $1])
        (log-op
            [(AND) '&&]
            [(OR) '||])
        (additive-expression
            [(additive-expression add-op additive-expression) (list $1 $2 $3)]
            [(mul-expression) $1])
        (add-op
            [(SUM) '+]
            [(MINUS) '-])
        (mul-expression
            [(mul-expression mul-op mul-expression) (list $2 $1 $3)]
            [(term) $1])
        (mul-op
            [(MUL) '*]
            [(DIV) '/])
        (term
            [(LEFTPAR expression RIGHTPAR) (list $2)]
            [(var) (list $1)]
            [(call) (list 'call $1)]
            [(NUM) $1]
            [(FNUM) $1]
            [(STR) $1])
        (call
            [(ID LEFTPAR args RIGHTPAR) (list $1 $3)]
            [(PRINT LEFTPAR print-ful-args RIGHTPAR) (list 'print $3)]
            [(LEN LEFTPAR expression RIGHTPAR) (list 'len $3)])
        (print-ful-args
            [(STR args) (list 'string $1 $2)])
        (args
            [(arg-list) $1]
            [() '()])
        (arg-list
            [(arg-list COMMA expression) (append $1 (list $3))]
            [(expression) (list $1)])
    )
))