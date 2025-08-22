#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "../src/Lexer.rkt")
(require "../src/Parser.rkt")
(require "../src/interpreter.rkt")

(define (lex-all input-code)
  (define port (open-input-string input-code))
  (let loop ()
    (define token (our-lexer port))
    (if (eq? (token-name token) 'EOF)
        (list (token-EOF))
        (cons (token-name token) (loop)))))

(define (do-parse input-string)
  (let ([ip (open-input-string input-string)])
    (with-handlers ([exn:fail? (lambda (exn) (printf "Error: ~a\n" (exn-message exn)))])
      (let ([result (our-parser (lambda () (our-lexer ip)))])
        ; (format "~a" result)
        ; (quote result)
        ; (printf "ast: ~a\n" result)
        result
        ))))


(define test1 "
int main() {
    int x;
    float y;
    x = 42;
    y = 3.14;
    if (x > 0) {
        y = y * 2;
    } else {
        return 1;
    }
    return x;
}
")


(define test2 "
int main() {
    int x;
    x = True;
    return (True) && (((True))) || (False);
}
")



; (define temp (do-parse test2))
; (list-ref (list-ref (list-ref (list-ref (list-ref (list-ref (do-parse test2) 1) 1) 4) 1) 1) 2)
; temp
; (do-parse test2)
(run (do-parse test2))
; (define temp (do-parse test1))
; temp

; (define myTest '(program (fundec (fun-dcl int main () (compound-stmnt ((var-spec int a) (a = 7) (var-spec int b) (b = ((a) + 9)) (return (a))))))))
; (define myTest '(1 (2 3 4 (2))))
; (list-ref (list-ref myTest 1) 3)