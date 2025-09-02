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

(define p3 "
int main(){
    string board[36];
    board[0] = \"O\";
    board[1] = \"-\";
    board[2] = \"X\";
    board[3] = \"X\";
    board[4] = \"X\";
    board[5] = \"X\";
    board[6] = \"-\";
    board[7] = \"O\";
    board[8] = \"-\";
    board[9] = \"-\";
    board[10] = \"-\";
    board[11] = \"-\";
    board[12] = \"O\";
    board[13] = \"-\";
    board[14] = \"O\";
    board[15] = \"-\";
    board[16] = \"-\";
    board[17] = \"-\";
    board[18] = \"O\";
    board[19] = \"-\";
    board[20] = \"-\";
    board[21] = \"O\";
    board[22] = \"-\";
    board[23] = \"-\";
    board[24] = \"O\";
    board[25] = \"-\";
    board[26] = \"-\";
    board[27] = \"-\";
    board[28] = \"-\";
    board[29] = \"-\";
    board[30] = \"-\";
    board[31] = \"-\";
    board[32] = \"-\";
    board[33] = \"-\";
    board[34] = \"-\";
    board[35] = \"-\";

    int i;
    i = 0;
    int j;
    int x_win;
    x_win = 0;
    int O_win;
    O_win = 0;
    while(i < 3){
        j = 0;
        while (j < 6) {
            if (board[6*j + i] == \"X\" && board[6*j + i + 1] == \"X\" && board[6*j + i + 2] == \"X\" && board[6*j + i + 3] == \"X\"){
                x_win = 1;
            }else{
            }

            if (board[6*j + i] == \"O\" && board[6*j + i + 1] == \"O\" && board[6*j + i + 2] == \"O\" && board[6*j + i + 3] == \"O\"){
                O_win = 1;
            }else{
            }
            j = j + 1;
        }
        i = i + 1;
    }

    j = 0;
    while(j < 3){
        i = 0;
        while (i < 6) {
            if (board[6*j + i] == \"X\" && board[6*j + i + 6] == \"X\" && board[6*j + i + 12] == \"X\" && board[6*j + i + 18] == \"X\"){
                x_win = 1;
            }else{
            }

            if (board[6*j + i] == \"O\" && board[6*j + i + 6] == \"O\" && board[6*j + i + 12] == \"O\" && board[6*j + i + 18] == \"O\"){
                O_win = 1;
            }else{
            }
            i = i + 1;
        }
        j = j + 1;
    }

    j = 0;
    while(j < 3){
        i = 0;
        while (i < 3) {
            if (board[6*j + i] == \"X\" && board[6*j + i + 6 + 1] == \"X\" && board[6*j + i + 12 + 1] == \"X\" && board[6*j + i + 18 + 1] == \"X\"){
                x_win = 1;
            }else{
            }

            if (board[6*j + i] == \"O\" && board[6*j + i + 6 + 1] == \"O\" && board[6*j + i + 12 + 2] == \"O\" && board[6*j + i + 18 + 3] == \"O\"){
                O_win = 1;
            }else{
            }
            i = i + 1;
        }
        j = j + 1;
    }

    if (x_win == 1 && O_win == 1){
        print(\"Somebody Cheated!\");
    }else{
    }
    if (x_win == 1 && O_win == 0){
        print(\"X wins!\");
    }else{
    }
    if (x_win == 0 && O_win == 1){
        print(\"O wins!\");
    }else{
    }
    if (x_win == 0 && O_win == 0){
        print(\"Draw!\");
    }else{
    }
    return 0;
}
")

(run (do-parse p3))