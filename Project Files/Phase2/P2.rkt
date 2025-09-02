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

(define p2 "
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

int power(int a, int n){
    int solve;
    solve = 1;
    while (n != 0) {
        solve = solve * a;
        n = n - 1;
    }
    return solve;
}

int convert_to_tenth_base(int n){
    int num;
    num = 0;
    int i;
    i = 0;
    while (n != 0) {
        num = num + power(2, i) * reminder(n, 10);
        n = quotient(n, 10);
        i = i + 1;
    }

    return num;
}

int main(){
    int len_a;
    len_a = 6;
    int a[6];
    a[0] = 7;
    a[1] = 11;
    a[2] = 20;
    a[3] = 16;
    a[4] = 14;
    a[5] = 1;

    int len_b;
    len_b = 6;
    int b[6];
    b[0] = 111;
    b[1] = 1011;
    b[2] = 1100;
    b[3] = 10000;
    b[4] = 0;
    b[5] = 10001001;

    int i;
    i = 0;
    int odd;
    odd = 1;
    int odd_check;
    odd_check = 0;
    int even;
    even = 1;
    int even_check;
    even_check = 0;
    int c;
    int j;
    while(i < len_b){
        c = convert_to_tenth_base(b[i]);
        j = 0;
        while (j < len_a) {
            if(c == a[j]){
                if(reminder(c, 2) == 0){
                    even_check = 1;
                    even = even * c;
                }else {
                    odd_check = 1;
                    odd = odd * c;
                }
            } else {
            }
            j = j + 1;
        }
        i = i + 1;
    }

    if (even_check == 1 && odd_check == 1) {
        print(\"~x\" odd + even);
    }else{
    }
    if (even_check == 1 && odd_check == 0) {
        print(\"~x\" even);
    }else{
    }
    if (even_check == 0 && odd_check == 1) {
        print(\"~x\" odd);
    }else{
    }
    if (even_check == 0 && odd_check == 0){
        print(\"0\");
    }else{}

    return 0;
}
")

(run (do-parse p2))