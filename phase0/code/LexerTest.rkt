#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "Lexer.rkt")

(require rackunit)
(define (lex-all port)
  (let loop ()
    (define token (our-lexer port))
    (if (eq? (token-name token) 'EOF)
        (list (token-EOF))
        (cons (token-name token) (loop)))))

(define (test-lexer input-code expected-tokens)
  (check-equal? (lex-all (open-input-string input-code)) expected-tokens))

; First test with simple code!
(define simple-code "int main(){}")
(define expected-tokens-for-simple-code
  (list
   'INT
   'ID
   'LEFTPAR
   'RIGHTPAR
   'LEFTVILI
   'RIGHTVILI
   'EOF))
(test-lexer simple-code expected-tokens-for-simple-code)

; Second test with all tokens!
(define code-with-all-tokens "
  int main()
    {
        int a, b;
        int c = 0;
        int d = a;
        int k = 1;
        while(d >= 8){
            int r = d - 8*(d / 8);
            c = c + (10 , k-1) * r;
            d = (d/8);
            k++;
        }
        c = c + d * (10,k - 1);

        int h,y,z,s;
        h = (10,b) * (c/(10,b));
        y = ((c - h)/(10, b-1));
        z = (y - 2)*(y > 1) + (y <= 1)*(y + 6);

        s = c + ((z - y) * (10, b-1));
        int w = 0;
        int i = 1;
        while(s >= (10, i - 1)){
            int l = (10,i) * (s/(10,i));
            int p = ((s - l)/(10, i-1));
            w += p * (8, i-1);
            i++;
        }

        string a;
        a = \"njvdskkksdj\";

        /*sjdkvnskdjvnsksdkjvnsdjvnskdvnskdvsdv*/
        

        return 0;
    }
  ")
(define expected-tokens-for-code-with-all-tokens
  (list
   'INT
   'ID
   'LEFTPAR
   'RIGHTPAR
   'LEFTVILI
   'INT
   'ID
   'COMMA
   'ID
   'SEMICOLON
   'INT
   'ID
   'ASSIGN
   'NUM
   'SEMICOLON
   'INT
   'ID
   'ASSIGN
   'ID
   'SEMICOLON
   'INT
   'ID
   'ASSIGN
   'NUM
   'SEMICOLON
   'WHILE
   'LEFTPAR
   'ID
   'GEQ
   'NUM
   'RIGHTPAR
   'LEFTVILI
   'INT
   'ID
   'ASSIGN
   'ID
   'MINUS
   'NUM
   'MUL
   'LEFTPAR
   'ID
   'DIV
   'NUM
   'RIGHTPAR
   'SEMICOLON
   'ID
   'ASSIGN
   'ID
   'SUM
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'MUL
   'ID
   'SEMICOLON
   'ID
   'ASSIGN
   'LEFTPAR
   'ID
   'DIV
   'NUM
   'RIGHTPAR
   'SEMICOLON
   'ID
   'SUM
   'SUM
   'SEMICOLON
   'RIGHTVILI
   'ID
   'ASSIGN
   'ID
   'SUM
   'ID
   'MUL
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'SEMICOLON
   'INT
   'ID
   'COMMA
   'ID
   'COMMA
   'ID
   'COMMA
   'ID
   'SEMICOLON
   'ID
   'ASSIGN
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'RIGHTPAR
   'MUL
   'LEFTPAR
   'ID
   'DIV
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'RIGHTPAR
   'RIGHTPAR
   'SEMICOLON
   'ID
   'ASSIGN
   'LEFTPAR
   'LEFTPAR
   'ID
   'MINUS
   'ID
   'RIGHTPAR
   'DIV
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'RIGHTPAR
   'SEMICOLON
   'ID
   'ASSIGN
   'LEFTPAR
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'MUL
   'LEFTPAR
   'ID
   'GT
   'NUM
   'RIGHTPAR
   'SUM
   'LEFTPAR
   'ID
   'LEQ
   'NUM
   'RIGHTPAR
   'MUL
   'LEFTPAR
   'ID
   'SUM
   'NUM
   'RIGHTPAR
   'SEMICOLON
   'ID
   'ASSIGN
   'ID
   'SUM
   'LEFTPAR
   'LEFTPAR
   'ID
   'MINUS
   'ID
   'RIGHTPAR
   'MUL
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'RIGHTPAR
   'SEMICOLON
   'INT
   'ID
   'ASSIGN
   'NUM
   'SEMICOLON
   'INT
   'ID
   'ASSIGN
   'NUM
   'SEMICOLON
   'WHILE
   'LEFTPAR
   'ID
   'GEQ
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'RIGHTPAR
   'LEFTVILI
   'INT
   'ID
   'ASSIGN
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'RIGHTPAR
   'MUL
   'LEFTPAR
   'ID
   'DIV
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'RIGHTPAR
   'RIGHTPAR
   'SEMICOLON
   'INT
   'ID
   'ASSIGN
   'LEFTPAR
   'LEFTPAR
   'ID
   'MINUS
   'ID
   'RIGHTPAR
   'DIV
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'RIGHTPAR
   'SEMICOLON
   'ID
   'SUM
   'ASSIGN
   'ID
   'MUL
   'LEFTPAR
   'NUM
   'COMMA
   'ID
   'MINUS
   'NUM
   'RIGHTPAR
   'SEMICOLON
   'ID
   'SUM
   'SUM
   'SEMICOLON
   'RIGHTVILI
   'STRING
   'ID
   'SEMICOLON
   'ID
   'ASSIGN
   'STR
   'SEMICOLON
   'COMMENT
   'RETURN
   'NUM
   'SEMICOLON
   'RIGHTVILI
   'EOF))
(test-lexer code-with-all-tokens expected-tokens-for-code-with-all-tokens)