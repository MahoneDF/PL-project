#include <cstdio>
#include <string>
using namespace std;

int main(){
    string board[36];
    board[0] = "-";
    board[1] = "-";
    board[2] = "X";
    board[3] = "X";
    board[4] = "X";
    board[5] = "X";
    board[6] = "O";
    board[7] = "O";
    board[8] = "-";
    board[9] = "-";
    board[10] = "-";
    board[11] = "-";
    board[12] = "O";
    board[13] = "-";
    board[14] = "-";
    board[15] = "-";
    board[16] = "-";
    board[17] = "-";
    board[18] = "O";
    board[19] = "-";
    board[20] = "-";
    board[21] = "O";
    board[22] = "-";
    board[23] = "-";
    board[24] = "O";
    board[25] = "-";
    board[26] = "-";
    board[27] = "-";
    board[28] = "-";
    board[29] = "-";
    board[30] = "-";
    board[31] = "-";
    board[32] = "-";
    board[33] = "-";
    board[34] = "-";
    board[35] = "-";

    int i;
    i = 0;
    int j;
    int x_win = 0;
    int y_win = 0;
    while(i < 3){
        j = 0;
        while (j < 6) {
            if (board[6*j + i] == "X" && board[6*j + i + 1] == "X" && board[6*j + i + 2] == "X" && board[6*j + i + 3] == "X"){
                x_win = 1;
            }

            if (board[6*j + i] == "O" && board[6*j + i + 1] == "O" && board[6*j + i + 2] == "O" && board[6*j + i + 3] == "O"){
                y_win = 1;
            }
            j = j + 1;
        }
        i = i + 1;
    }

    j = 0;
    while(j < 3){
        i = 0;
        while (i < 6) {
            if (board[6*j + i] == "X" && board[6*j + i + 6] == "X" && board[6*j + i + 12] == "X" && board[6*j + i + 18] == "X"){
                x_win = 1;
            }

            if (board[6*j + i] == "O" && board[6*j + i + 6] == "O" && board[6*j + i + 12] == "O" && board[6*j + i + 18] == "O"){
                y_win = 1;
            }
            i = i + 1;
        }
        j = j + 1;
    }

    j = 0;
    while(j < 3){
        i = 0;
        while (i < 3) {
            if (board[6*j + i] == "X" && board[6*j + i + 6 + 1] == "X" && board[6*j + i + 12 + 1] == "X" && board[6*j + i + 18 + 1] == "X"){
                x_win = 1;
            }

            if (board[6*j + i] == "O" && board[6*j + i + 6 + 1] == "O" && board[6*j + i + 12 + 2] == "O" && board[6*j + i + 18 + 3] == "O"){
                y_win = 1;
            }
            i = i + 1;
        }
        j = j + 1;
    }

    if (x_win == 1 && y_win == 1){
        printf("Somebody Cheated!\n");
    }
    if (x_win == 1 && y_win == 0){
        printf("X wins!\n");
    }
    if (x_win == 0 && y_win == 1){
        printf("Y wins!\n");
    }
    if (x_win == 0 && y_win == 0){
        printf("Draw!\n");
    }
    return 0;
}