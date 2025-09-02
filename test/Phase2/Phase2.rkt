#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "../../src/Lexer.rkt")
(require "../../src/Parser.rkt")
(require "../../src/interpreter.rkt")

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

(define p4 "
bool isValid(string date) {
    bool valid;
    valid = True;
    if (len(date) != 8) { return False;}
    else {
        string M1;
        string M2;
        M1 = date[4];
        M2 = date[5];
        if (M1 == \"0\") {
            valid = valid && ((M2 == \"9\") || (M2 == \"1\") || (M2 == \"2\") || (M2 == \"3\") || (M2 == \"4\") || (M2 == \"5\") || (M2 == \"6\") || (M2 == \"7\") || (M2 == \"8\"));
        } else {
            if (M1 == \"1\") {
                valid = valid && ((M2 == \"0\") || (M2 == \"1\") || (M2 == \"2\"));
            } else { return False; }
        }
        string D1;
        string D2;
        D1 = date[6];
        D2 = date[7];
        if (D1 == \"0\") {
            valid = valid && ((D2 == \"9\") || (D2 == \"1\") || (D2 == \"2\") || (D2 == \"3\") || (D2 == \"4\") || (D2 == \"5\") || (D2 == \"6\") || (D2 == \"7\") || (D2 == \"8\"));            
        } else {
            if (D1 == \"1\" || D1 == \"2\") {
                valid = valid && ((D2 == \"0\") || (D2 == \"9\") || (D2 == \"1\") || (D2 == \"2\") || (D2 == \"3\") || (D2 == \"4\") || (D2 == \"5\") || (D2 == \"6\") || (D2 == \"7\") || (D2 == \"8\"));                            
            }
            else {
                if (D1 == \"3\") {
                    valid = valid && ((D2 == \"0\") || (D2 == \"1\"));
                } else {
                    return False;
                }
            }
        }
        return valid;
    }
}

string convertFormat(string date){
    string M1;
    string M2;
    M1 = date[4];
    M2 = date[5];
    string month;
    if (M1 == \"0\") {
        if (M2 == \"1\") {month = \"Jan\";} else {
        if (M2 == \"2\") {month = \"Feb\";} else {
        if (M2 == \"3\") {month = \"Mar\";} else {
        if (M2 == \"4\") {month = \"Apr\";} else {
        if (M2 == \"5\") {month = \"May\";} else {
        if (M2 == \"6\") {month = \"Jun\";} else {
        if (M2 == \"7\") {month = \"Jul\";} else {
        if (M2 == \"8\") {month = \"Aug\";} else {
        if (M2 == \"9\") {month = \"Sep\";} else { return \"bad\";}}}}}}}}}
    } else {
        if (M2 == \"0\") {month = \"Oct\";} else {
        if (M2 == \"1\") {month = \"Nov\";} else {
        if (M2 == \"2\") {month = \"Dec\";} else { return \"bod\";}}}
    }
    string day;
    day = \"11\";
    day[0] = date[6];
    day[1] = date[7];
    string year;
    year = \"1111\";
    year[0] = date[0];
    year[1] = date[1];
    year[2] = date[2];
    year[3] = date[3];
    return (month + \" \" + day + \" of \" + year);
}

bool compareDigits(string s1, string s2){
    if(s1 == \"0\"){
        if(s2 == \"0\"){ return False; }
        else{ return True; }
    }
    else{
        if(s1 == \"1\"){
            if(s2 == \"0\"){ return False; }
            else{
                if(s2 == \"1\"){ return False; }
                else{ return True; }}}
        else{
            if(s1 == \"2\"){
                if(s2 == \"0\"){ return False; }
                else{
                    if(s2 == \"1\"){ return False; }
                    else{
                        if(s2 == \"2\"){ return False; }
                        else{ return True; }}}}
            else{
                if(s1 == \"3\"){
                    if(s2 == \"0\"){ return False; }
                    else{
                        if(s2 == \"1\"){ return False; }
                        else{
                            if(s2 == \"2\"){ return False; }
                            else{
                                if(s2 == \"3\"){ return False; }
                                else{ return True; }}}}}
                else{
                    if(s1 == \"4\"){
                        if(s2 == \"0\"){ return False; }
                        else{
                            if(s2 == \"1\"){ return False; }
                            else{
                                if(s2 == \"2\"){ return False; }
                                else{
                                    if(s2 == \"3\"){ return False; }
                                    else{
                                        if(s2 == \"4\"){ return False; }
                                        else{ return True; }}}}}}
                    else{
                        if(s1 == \"5\"){
                            if(s2 == \"0\"){ return False; }
                            else{
                                if(s2 == \"1\"){ return False; }
                                else{
                                    if(s2 == \"2\"){ return False; }
                                    else{
                                        if(s2 == \"3\"){ return False; }
                                        else{
                                            if(s2 == \"4\"){ return False; }
                                            else{
                                                if(s2 == \"5\"){ return False; }
                                                else{ return True; }}}}}}}
                        else{
                            if(s1 == \"6\"){
                                if(s2 == \"0\"){ return False; }
                                else{
                                    if(s2 == \"1\"){ return False; }
                                    else{
                                        if(s2 == \"2\"){ return False; }
                                        else{
                                            if(s2 == \"3\"){ return False; }
                                            else{
                                                if(s2 == \"4\"){ return False; }
                                                else{
                                                    if(s2 == \"5\"){ return False; }
                                                    else{
                                                        if(s2 == \"6\"){ return False; }
                                                        else{ return True; }}}}}}}}
                            else{
                                if(s1 == \"7\"){
                                    if(s2 == \"0\"){ return False; }
                                    else{
                                        if(s2 == \"1\"){ return False; }
                                        else{
                                            if(s2 == \"2\"){ return False; }
                                            else{
                                                if(s2 == \"3\"){ return False; }
                                                else{
                                                    if(s2 == \"4\"){ return False; }
                                                    else{
                                                        if(s2 == \"5\"){ return False; }
                                                        else{
                                                            if(s2 == \"6\"){ return False; }
                                                            else{
                                                                if(s2 == \"7\"){ return False; }
                                                                else{ return True; }}}}}}}}}
                                else{
                                    if(s1 == \"8\"){
                                        if(s2 == \"0\"){ return False; }
                                        else{
                                            if(s2 == \"1\"){ return False; }
                                            else{
                                                if(s2 == \"2\"){ return False; }
                                                else{
                                                    if(s2 == \"3\"){ return False; }
                                                    else{
                                                        if(s2 == \"4\"){ return False; }
                                                        else{
                                                            if(s2 == \"5\"){ return False; }
                                                            else{
                                                                if(s2 == \"6\"){ return False; }
                                                                else{
                                                                    if(s2 == \"7\"){ return False; }
                                                                    else{
                                                                        if(s2 == \"8\"){ return False; }
                                                                        else{ return True; }}}}}}}}}}
                                    else{
                                        if(s1 == \"9\"){
                                            if(s2 == \"0\"){ return False; }
                                            else{
                                                if(s2 == \"1\"){ return False; }
                                                else{
                                                    if(s2 == \"2\"){ return False; }
                                                    else{
                                                        if(s2 == \"3\"){ return False; }
                                                        else{
                                                            if(s2 == \"4\"){ return False; }
                                                            else{
                                                                if(s2 == \"5\"){ return False; }
                                                                else{
                                                                    if(s2 == \"6\"){ return False; }
                                                                    else{
                                                                        if(s2 == \"7\"){ return False; }
                                                                        else{
                                                                            if(s2 == \"8\"){ return False; }
                                                                            else{
                                                                                if(s2 == \"9\"){ return False; }
                                                                                else{ return True; }}}}}}}}}}}
                                        else{ return False; }}}}}}}}}}
}

bool compareDate(string d1, string d2){
    int i;
    i = 0;
    while (i < len(d1)){
        string tmp1;
        string tmp2;
        tmp1 = d1[i];
        tmp2 = d2[i];
        if(compareDigits(tmp1, tmp2)){return True;}
        else {if(compareDigits(tmp2, tmp1)) {return False;}
        else{
            i = i + 1;
        }}
    }
    return False;
}

int main () {
    string input[7];
    input[0] = \"020250112\";
    input[1] = \"20190418\";
    input[2] = \"20230928\";
    input[3] = \"20241515\";
    input[4] = \"20210101\";
    input[5] = \"20001231\";
    input[6] = \"20250618\";

    int n;
    n = len(input);
    int i;
    i = 0;
    int m;
    m = 0;
    while (i < n){
        if (isValid(input[i])){
            m = m + 1;
        } else {}
        i = i + 1;
    }
    string output[m];
    i = 0;
    int c;
    c = 0;
    while (i < n){
        if (isValid(input[i])){
            output[c] = input[i];
            c = c + 1;
        } else {}
        i = i + 1;
    }
    i = 0;
    while (i < m - 1){
        int min;
        min = i;
        int j;
        j = i + 1;
        while (j < m) {
            if(compareDate(output[j], output[min])){
                min = j;
            }
            else{}
            j = j + 1;
        }
        if (min != i) {
            string temp;
            temp = output[min];
            output[min] = output[i];
            output[i] = temp;
        } else {}
        i = i + 1;
    }
    i = 0;
    while (i < m) {
        output[i] = convertFormat(output[i]);
        print(\"~x\" output[i]);
        i = i + 1;
    }
    return 0;
}
")

(define p5 "
int arrSize;
string stdId[100];
string names[100];
string sirNames[100];
float stdAvg[100];
string stdMentored[100];

int getStdInd(string id) {
    bool stayInWhile;
    int ind;
    while (stayInWhile) {
        if (ind >= arrSize || stdId[ind] == \"\") {
            ind = 0 - 1;
            stayInWhile = False;
        } else {
            if (stdId[ind] == id) {
                stayInWhile = False;
            } else {
                ind = ind + 1;
            }
        }
    }

    return ind;
}

float calcGoodnessVal(float curSum, int curCount, string stdId) {
    if (stdId == \"-1\") {
        if (curCount != 0) {
            return curSum / curCount;
        } else {
            return 1.5;
        }
    } else {
        int Ind;
        Ind = getStdInd(stdId);
        curSum = curSum + stdAvg[Ind];
        curCount = curCount + 1;
        return calcGoodnessVal(curSum, curCount, stdMentored[Ind]);
    }
}

float calcGoodness(string stdId) {
    return calcGoodnessVal(0, 0, stdMentored[getStdInd(stdId)]);
}

bool isFromYear(string stdId, string year) {
    int yearSize;
    yearSize = len(year);
    int ind;
    bool wasFrom;
    wasFrom = True;
    bool keepChecking;
    while (ind < yearSize && keepChecking) {
        string stdIdChar;
        string yearChar;
        stdIdChar = stdId[ind];
        yearChar = year[ind];

        if (yearChar != stdIdChar) {
            wasFrom = False;
            keepChecking = False;
        } else {
            ind = ind + 1;
        }
    }
    
    return wasFrom;
 }

int indexOfBestMentor(string year) {
    int mentorInd;
    mentorInd = 0 - 1;
    float bestAvg;

    int whileInd;

    while (whileInd < arrSize && stdId[whileInd] != \"\") {
        string tempId;
        tempId = stdId[whileInd];
        if (isFromYear(tempId, year)) {
            float res; res = calcGoodness(tempId);
            if (res > bestAvg) {
                mentorInd = getStdInd(tempId);
                bestAvg = res;
            } else {
            
            }
        } else {
        
        }
        
        whileInd = whileInd + 1;
    
    }

    return mentorInd;
}

string findAnswer(string year) {
    int ansInd;
    ansInd = indexOfBestMentor(year);
    string name;
    string sirName;
    name = names[ansInd];
    sirName = sirNames[ansInd];

    return name + \" \" + sirName;
}

int main() {
    arrSize = 100;
    string inputYear;

    inputYear = \"400\";

    stdId[0] = \"99111111\"; stdId[1] = \"400111111\"; stdId[2] = \"400222222\";
    stdId[3] = \"401111111\"; stdId[4] = \"402111111\"; stdId[5] = \"402222222\";
    stdId[6] = \"403111111\"; 

    names[0] = \"person0\"; names[1] = \"person1\"; names[2] = \"person2\";
    names[3] = \"person3\"; names[4] = \"person4\"; names[5] = \"person5\";
    names[6] = \"person6\"; 

    sirNames[0] = \"test0\"; sirNames[1] = \"test1\"; sirNames[2] = \"test2\";
    sirNames[3] = \"test3\"; sirNames[4] = \"test4\"; sirNames[5] = \"test5\";
    sirNames[6] = \"test6\"; 

    stdAvg[0] = 18.98; stdAvg[1] = 18.12; stdAvg[2] = 19; stdAvg[3] = 16.55;
    stdAvg[4] = 19.72; stdAvg[5] = 17.29; stdAvg[6] = 14.84;

    stdMentored[0] = \"403111111\"; stdMentored[1] = \"401111111\"; stdMentored[2] = \"402222222\";
    stdMentored[3] = \"402111111\"; stdMentored[4] = \"-1\"; stdMentored[5] = \"-1\";
    stdMentored[6] = \"-1\"; 

    print(\"Best mentor's name: ~a\" findAnswer(\"400\"));

    return 0;
}
")

(run (do-parse p5))