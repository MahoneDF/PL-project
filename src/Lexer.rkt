#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out))

(define-tokens value-tokens (STR NUM FNUM ID))
(define-empty-tokens empty-tokens (COMMENT EOF INT FLOAT STRING PRINT IF ELSE COMMA SEMICOLON PASS BREAK CONTINUE ASSIGN LEFTPAR RIGHTPAR LEFTBR RIGHTBR WHILE OR AND EQ NEQ GT LT GEQ LEQ SUM MINUS MUL DIV TRUE FALSE RETURN LEFTVILI RIGHTVILI))

(define our-lexer (lexer
    [whitespace (our-lexer input-port)]
    [(eof) (token-EOF)]
    ["int" (token-INT)]
    ["float" (token-FLOAT)]
    ["string" (token-STRING)]
    ["print" (token-PRINT)]
    ["if" (token-IF)]
    ["else" (token-ELSE)]
    ["," (token-COMMA)]
    [";" (token-SEMICOLON)]
    ["pass" (token-PASS)]
    ["break" (token-BREAK)]
    ["continue" (token-CONTINUE)]
    ["=" (token-ASSIGN)]
    ["(" (token-LEFTPAR)]
    [")" (token-RIGHTPAR)]
    ["[" (token-LEFTBR)]
    ["]" (token-RIGHTBR)]
    ["{" (token-LEFTVILI)]
    ["}" (token-RIGHTVILI)]
    ["while" (token-WHILE)]
    ["||" (token-OR)]
    ["&&" (token-AND)]
    ["==" (token-EQ)]
    ["!=" (token-NEQ)]
    [">" (token-GT)]
    ["<" (token-LT)]
    [">=" (token-GEQ)]
    ["<=" (token-LEQ)]
    ["+" (token-SUM)]
    ["-" (token-MINUS)]
    ["*" (token-MUL)]
    ["/" (token-DIV)]
    ["True" (token-TRUE)]
    ["False" (token-FALSE)]
    ["return" (token-RETURN)]
    [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (token-COMMENT)] ;comment
    [(:: "\"" (complement (:: any-string "\"" any-string)) "\"") (token-STR lexeme)] ;string
    [(:: (:+ (char-range #\0 #\9))) (token-NUM lexeme)] ;int
    [(:: (:+ (char-range #\0 #\9) #\. (:+ (char-range #\0 #\9)))) (token-FNUM lexeme)] ;float
    [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) #\_)) (token-ID lexeme)]
))
