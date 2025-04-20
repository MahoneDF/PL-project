#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "Lexer.rkt")
(require "datatypes.rkt")

(define our-parser (parser
    (start program)
    (end EOF)
    (error void)
    (tokens value-tokens empty-tokens)
    (grammer
        (program
            [(declaration-list) (program $1)])
        (declaration-list
            ; [(declaration-list declaration) (dclist $1 $2)]
            [(declaration-list declaration) (append $1 (list $2))]
            ; [(declaration) (decl $1)])
            [(declaration) $1])
        (declaration 
            [(var-declaration) (vardec $1)]
            [(fun-declaration) (fundec $2)])
        (var-declaration
            [(type-spec ID SEMICOLON) (var-spec $1 $2)]
            [(type-spec ID LEFTBR NUM RIGHTBR SEMICOLON) (array-spec $1 $2 $4)])
        (type-spec 
            [(int) ("int")]
            [(float) ("float")]
            [(string) ("string")])
        (fun-declaration
            [(type-spec ID LEFTPAR params RIGHTPAR compound-stmnt) (fun-dcl $1 $2 $4 $6)])
        (params
            [(param-list) (a-param-list $1)]
            [() ()])
        (param-list
            [(param-list COMMA param) (append $1 (list $3))]
            [(param) $1])
        (param
            [(type-spec ID) (argvar $1 $2)]
            [(type-spec ID LEFTBR RIGHTBR) (argarray $1 $2)])
        (compound-stmnt
            [(LEFTVILI local-dec stmnt-list RIGHTVILI) (compound-stmnt $2 $3)])
        (local-dec 
            [(local-dec var-declaration) (append $1 (list $2))]
            [() ()])
        (stmnt-list
            [(stmnt-list stmnt) (append $1 (list $2))]
            [() ()])
        (stmnt
            [(compound-stmnt) $1]
            [(cond-stmnt) $1]
            [(iter-stmnt) $1]
            [(return-stmnt) $1]
            [(expression-stmnt) $1])
        (compound-stmnt
            [(IF LEFTPAR expression RIGHTPAR stmnt) (comp)]
            [(IF LEFTPAR expression RIGHTPAR stmnt ELSE stmnt) ()])
    )
))