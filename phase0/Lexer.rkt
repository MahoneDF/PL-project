#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens value-tokens (STR NUM FNUM ID))
(define-empty-tokens empty-tokens (EOF INT FLOAT STRING IF ELSE COMMA SEMICOLON PASS BREAK CONTINUE ASSIGN LEFTPAR RIGHTPAR LEFTBR RIGHTBR WHILE OR AND EQ NEQ GT LT GEQ LEQ SUM MINUS MUL DIV TRUE FALSE RETURN LEFTVILI RIGHTVILI))

(define our-lexer (lexer
    [whitespace (our-lexer input-port)]
    [(eof) (token-EOF)]
    [(:: "\"" (complement (:: any-string "\"" any-string)) "\"") (token-STR lexeme)] ;string
    [(:: (:+ (char-range #\0 #\9))) (token-NUM lexeme)] ;int
    [(:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))) (token-FNUM lexeme)] ;float
    [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) #\_)) (token-ID lexeme)]
    ["int" (token-INT)]
    ["float" (token-FLOAT)]
    ["string" (token-STRING)]
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
))

(define (test-lexer input-string)
  (printf "Testing input: ~a\n" input-string)
  (let ([ip (open-input-string input-string)])
    (port-count-lines! ip) ; Enable line/column tracking for better error messages
    (let loop ()
      (let ([token (our-lexer ip)])
        (printf "Token: ~a\n" token)
        (unless (eq? (token-name token) 'EOF)
          (loop)))))
  (printf "Ooooooooh Yeahhhhhhhhhh\n"))

;; Run test cases
(define (run-tests)
  (test-lexer "int x; int f () {return 3;}") ; Keywords, operators, int, float
;   (test-lexer "\"hello world\"") ; String
;   (test-lexer "while (True) { 123; }") ; Mixed tokens
;   (test-lexer "1.23 45 \"test\" if else") ; Multiple tokens
;   (test-lexer "") ; Empty input
;   (test-lexer "@#$") ; Invalid input to trigger error
;   (test-lexer "3.14.15")) ; Malformed float to test error handling
)

;; Execute the tests
(run-tests)