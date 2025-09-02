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