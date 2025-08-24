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
    x = 5;
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

(define chechkk "
int f (int n) {
        int y;
        y = 1;
        while (n > 1) {
            y = y * n;
            n = n - 1;
        }
        return y;
    }
int main () {
    int x;
    x = 7;
    int y;
    if (x < 6) {
        y = 9;
    }
    else {
        y = 11;
    }
    {
        int x;
        x = 10;
        print(\"dsfsdsfdf\");
    }
    int z;
    z = f (5);
    float g;
    g = x / 1;
    return (x + 2*(y + z) + g);
}
")

(define scopeTesting "
int f(int n) {
    return n + 3;
}

int g(int n, int m) {
    return 2 * n;
}

int main(){
    int n;
    n = 7;
    print(\"ghjghgjh  ~x sdjksjk ~x uiuiuiu ~x  ioioio ~x aytytyt ~x awawa\" n, n+2, 3.14, n==8, \"qqq\");
    {
        n = 99;
        print(\"~x\" n);
    }
    print(\"~x\" n);
    return f (g (n, 89));
}
")

(define SimpleScopeTesting "
int main(){
    int n;
    n = 7;
    {
        n = 99;
        print(\"~x\" n);
    }
    print(\"~x\" n);
}
")

(define AnotherSimpleScopeTesting "
int f(int m) {
    m = m + 9;
    return m;
}
int main(){
    int n;
    n = 7;
    {
        int n;
        n = 99;
        print(\"~x\" n);
    }
    int k;
    print(\"~x\" f(n));
    print(\"~x\" n);
}
")

(define FinalScopingCheck "
int main() {
    int arr[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    {
        int arr[2];
        arr[0] = 100;
        arr[1] = 200;
        print(\"Inner array: ~d, ~d\" arr[0], arr[1]);
    }
    print(\"Outer array: ~d, ~d, ~d\" arr[0], arr[1], arr[2]);
    return 0;
}
")

(define array "
int increment_all(int arr[], int size) {
    int i;
    i = 0;
    while (i < size) {
        arr[i] = arr[i] + 1;
        i = i + 1;
    }
    return 0;
}

int process_array(int arr[], int size) {
    increment_all(arr, size);
    increment_all(arr, size);
    return 0;
}

int main() {
    int values[3];
    values[0] = 10;
    values[1] = 20;
    values[2] = 30;
    print(\"Initial: ~d ~d ~d\" values[0], values[1], values[2]);
    process_array(values, 3);
    print(\"Final: ~d ~d ~d\" values[0], values[1], values[2]);
    return 0;
}
")

;; Run the tests
(run (do-parse chechkk))
(newline)

(run (do-parse scopeTesting))
(newline)

(run (do-parse SimpleScopeTesting))
(newline)

(run (do-parse AnotherSimpleScopeTesting))
(newline)

(run (do-parse FinalScopingCheck))
(newline)

(run (do-parse array))
(newline)

; (printf "Testing type error:\n")
; (run (do-parse type-error-test))
; (newline)

; (printf "Testing undefined variable:\n")
; (run (do-parse undefined-var-test))
; (newline)

; (printf "Testing division by zero:\n")
; (run (do-parse division-by-zero-test))
; (newline)

; (printf "Testing index out of range:\n")
; (run (do-parse index-out-of-range-test))
; (newline)

; (printf "Testing arity mismatch:\n")
; (run (do-parse arity-mismatch-test))
; (newline)

; (printf "Testing array index type error:\n")
; (run (do-parse array-index-type-error-test))
; (newline)

; (printf "Testing print format error:\n")
; (run (do-parse print-format-error-test))
; (newline)

; (printf "Testing function call type error:\n")
; (run (do-parse function-call-type-error-test))
; (newline)

; (printf "Testing nested errors:\n")
; (run (do-parse nested-error-test))
; (newline)

; (printf "Testing array type errors:\n")
; (run (do-parse array-type-error))
; (newline)

; (printf "Testing assignment type errors:\n")
; (run (do-parse assignment-type-error))
; (newline)