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
        result))))


(define test1 "
int fac (int n) {
if (n <= 1) {
return 1;
} else {
return n * fac(n - 1);
}
}
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
    return fac(4);
}
")


(define test2 "
int glob;
int isGlobFine() {
glob = glob + 1;
return glob;
}
int main() {
int glob;
glob = 18;
isGlobFine();
return isGlobFine();
}
")



; (define temp (do-parse test2))
; (list-ref (list-ref (list-ref (list-ref (list-ref (list-ref (do-parse test2) 1) 1) 4) 1) 1) 2)
; temp
; (do-parse test1)
(run (do-parse test1))
; (define temp (do-parse test1))
; temp

; (define myTest '(program (fundec (fun-dcl int main () (compound-stmnt ((var-spec int a) (a = 7) (var-spec int b) (b = ((a) + 9)) (return (a))))))))
; (define myTest '(1 (2 3 4 (2))))
; (list-ref (list-ref myTest 1) 3)

;; Test cases for error handling

(define type-error-test "
int main() {
    int x;
    string s;
    s = \"hello\";
    x = 5 + s;
    return x;
}
")

(define undefined-var-test "
int main() {
    int x;
    x = x / 4;
    return x;
}
")

(define division-by-zero-test "
int main() {
    int x;
    x = 10 / 0;
    return x;
}
")

(define index-out-of-range-test "
int main() {
    int arr[5];
    arr[10] = 42;
    return arr[10];
}
")

(define arity-mismatch-test "
int add(int a, int b) {
    return a + b;
}

int main() {
    int x;
    x = add(5);
    return x;
}
")

(define array-index-type-error-test "
int main() {
    int arr[5];
    string s;
    s = \"hello\";
    arr[s] = 42;
    return arr[0];
}
")

(define print-format-error-test "
int main() {
    int x;
    x = 5;
    print(\"world\");
    return 0;
}
")

(define function-call-type-error-test "
int test(int x) {
    return 0;
}

int main() {
    string s;
    s = \"hello\";
    test(s);
    return 0;
}
")

(define nested-error-test "
int main() {
    int arr[3];
    int x;
    x = 10 / (arr[5] - 5);
    return x;
}
")

(define array-type-error "
int main() {
    int arr[3];
    string x;
    x = \"salam\";
    arr[0] = x;
    return x;
}
")

(define assignment-type-error "
int main() {
    int x;
    x = \"salam\";
    return x;
}
")

;; Run the tests
(printf "Testing type error:\n")
(run (do-parse type-error-test))
(newline)

(printf "Testing undefined variable:\n")
(run (do-parse undefined-var-test))
(newline)

(printf "Testing division by zero:\n")
(run (do-parse division-by-zero-test))
(newline)

(printf "Testing index out of range:\n")
(run (do-parse index-out-of-range-test))
(newline)

(printf "Testing arity mismatch:\n")
(run (do-parse arity-mismatch-test))
(newline)

(printf "Testing array index type error:\n")
(run (do-parse array-index-type-error-test))
(newline)

(printf "Testing print format error:\n")
(run (do-parse print-format-error-test))
(newline)

(printf "Testing function call type error:\n")
(run (do-parse function-call-type-error-test))
(newline)

(printf "Testing nested errors:\n")
(run (do-parse nested-error-test))
(newline)

(printf "Testing array type errors:\n")
(run (do-parse array-type-error))
(newline)

(printf "Testing assignment type errors:\n")
(run (do-parse assignment-type-error))
(newline)


(define scoping-test "
int main() {
    int a;
    a = 0;
    int b;
    b = 0;
    while(b == 0){
        a = a + 1;
        b = 1;
    }
    return a;
}
")

(run (do-parse scoping-test))
(newline)
; =====================================================================================

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
        num = num / 10;
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
    n = 123310;
    if (n < 0){
        print(\"you should provide a non-negative nuⅿber!\n\");
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
        return tmp;
    }
}
")

(printf ">>>>>>>>>>> P1:\n")
(run (do-parse p1))
(newline)

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
    int a[len_a];
    a[0] = 7;
    a[1] = 11;
    a[2] = 20;
    a[3] = 16;
    a[4] = 14;
    a[5] = 1;

    int len_b;
    len_b = 6;
    int b[len_b];
    b[0] = 111;
    b[1] = 1011;
    b[2] = 1100;
    b[3] = 10000;
    b[4] = 0;
    b[5] = 10001001;

    convert_to_tenth_base(b[5]);
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
            }
            j = j + 1;
        }
        i = i + 1;
    }

    if (even_check == 1 && odd_check == 1) {
        return odd + even;
    }
    if (even_check == 1 && odd_check == 0) {
        return even;
    }
    if (even_check == 0 && odd_check == 1) {
        return odd;
    }

    return 0;
}
")

; (printf ">>>>>>>>>>> P2:\n")
; (run (do-parse p2))
; (newline)