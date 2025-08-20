#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "Lexer.rkt")
(require "Parser.rkt")

(define (test-parse input-string)
  (let ([ip (open-input-string input-string)])
    (with-handlers ([exn:fail? (lambda (exn) (printf "Error: ~a\n" (exn-message exn)))])
      (let ([result (our-parser (lambda () (our-lexer ip)))])
        (printf "AST: ~a\n" result)
        ))))

(test-parse "int salam (){} int main(int a, string b) {int x; print(\"what the hell\");}")
(test-parse "int a;")
(test-parse "int main () {}")
(test-parse "int main () {int a;}")
(test-parse "int main () {int a; a = 3;}")
(test-parse "int main () {int a; a = 2; int b; b = a + 1; return b;}")
(test-parse "int main () {print(\"hello woooorld\");}")
(test-parse "int salam (){} int main () {salam();}")
(test-parse "int main (int a, int b []) {}")
(test-parse "int main (int a, int b []) {
    int y;
    int z;
    int h;
    x = y + 3;
    z = ii - 1;
    if (3 == 9) {d = y + 0;}
    else {;}
    return 0;
    }")
(test-parse "
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
    while (y < 10) {
        y = y + 1;
    }
    return x;
}
"
  )

(test-parse "int multiDeclaration() {int a; print(\"salam be hame\"); int another_declaration;}")