#include <cstdio>
#include <string>
using namespace std;

bool isValid(string date) {
    if (len(date) != 8) { return False;}
    else {
        string M1;
        string M2;
        M1 = date[4];
        M2 = date[5];
        if (M1 == "0") {
            return ((M2 == "9") || (M2 == "1") || (M2 == "2") || (M2 == "3") || (M2 == "4") || (M2 == "5") || (M2 == "6") || (M2 == "7") || (M2 == "8"));
        } else {
            if (M1 == "1") {
                return ((M2 == "0") || (M2 == "1") || (M2 == "2"));
            } else { return False; }
        }
    }
}

int main () {
    string input[7];
    input[0] = "020250112";
    input[1] = "20190418";
    input[2] = "20230928";
    input[3] = "20241515";
    input[4] = "20210101";
    input[5] = "20001231";
    input[6] = "20250618";


}