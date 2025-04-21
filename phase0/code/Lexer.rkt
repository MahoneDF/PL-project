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
    [(:: (:or (:+ (char-range #\0 #\9)) (:: "-"(:+ (char-range #\0 #\9))))) (token-NUM lexeme)] ;int
    [(:: (:or (:+ (char-range #\0 #\9) #\. (:+ (char-range #\0 #\9)))
              (:: "-"(:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))) (token-FNUM lexeme)] ;float
    [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) #\_)) (token-ID lexeme)]
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
(define input-code "
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

(define input-code-with-negetive-number "
        int negNum = -1;
        float negFloat = -12.5;
  ")
(define (run-tests)
  (displayln "---------------------First Test---------------------")
  (test-lexer input-code)
  (displayln "---------------------Second Test---------------------")
  (test-lexer input-code-with-negetive-number)
)

; Execute the tests
; (run-tests)