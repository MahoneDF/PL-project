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

(run (do-parse p4))