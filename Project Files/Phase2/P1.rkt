#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "../src/Lexer.rkt")
(require "../src/Parser.rkt")
(require "../src/interpreter.rkt")

(define (do-parse input-string)
  (let ([ip (open-input-string input-string)])
    (with-handlers ([exn:fail? (lambda (exn) (printf "Error: ~a\n" (exn-message exn)))])
      (let ([result (our-parser (lambda () (our-lexer ip)))])
        result))))


(define p1 "
int quotient(int a, int b){
    int q;
    q = 0;
    while (a >= b){
        a = a - b;
        q = q + 1;
    }
    return q;
}

int reminder(int a, int b){
    while (a >= b){
        a = a - b;
    }
    return a;
}

int length_number(int num){
    int t;
    t = 0;
    while (num >= 1){
        num = quotient(num, 10);
        t = t + 1;
    }
    return t;
}

int power(int a, int n){
    int solve;
    solve = 1;
    while (n != 0) {
        solve = solve * a;
        n = n - 1;
    }
    return solve;
}

int main(){
    int n;
    n = 12200;
    if (n < 0){
        print(\"you should provide a non-negative number!\n\");
        return 0;
    }
    else{
        int a;
        int tmp;
        tmp = 0;
        int length;
        length = length_number(n);
        int i;
        i = 1;
        while(length != 0){
            a = quotient(reminder(n, power(10, i)), power(10, i - 1));
            tmp = tmp + a * power(10, length - 1);
            length = length - 1;
            i = i + 1;
        }
        print(\"~x\" tmp);
    }
    return 0;
}
")

(run (do-parse p1))
