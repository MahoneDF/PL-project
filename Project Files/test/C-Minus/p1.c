
#include <stdio.h>
int n = 123450;

int quotient(int a, int b){
    if (a < b){
        return 0;
    }
    else {
        return quotient(a - b, b) + 1;
    }
}

int reminder(int a, int b){
    if (a < b){
        return a;
    }
    else {
        return reminder(a - b, b);
    }
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
    if (n < 0){
        printf("you should provide a non-negative nuⅿber!\n");
    }
    else{
        int a;
        int tmp;
        int length;
        length = length_number(n);
        int i;
        i = 1;
        tmp = 0;
        while(length != 0){
            a = quotient(reminder(n, power(10, i)), power(10, i - 1));
            printf("a = %d\n", a);
            tmp = tmp + a * power(10, length - 1);
            length = length - 1;
            i = i + 1;
        }
        printf("%d\n", tmp);
    }
    return 0;
}