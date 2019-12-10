// Mark Judy
// My C solutions


#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Day 1 ///////////////////////////////////////////////////////////////////////////////////////////

int modules[] = {
    74099,  50130,  81867,  55356,  73088,  73706,  55902,  113399, 129578, 78051,
    117663, 137454, 66285,  115389, 50547,  51588,  115792, 91085,  118882, 109486,
    135616, 107771, 90105,  101182, 54766,  86615,  91261,  104321, 121607, 82197,
    68626,  111255, 136080, 87509,  70125,  91180,  75925,  53492,  96853,  115081,
    121621, 87461,  116030, 67335,  61282,  112424, 106785, 142243, 110564, 56983,
    131420, 116534, 117376, 147088, 117628, 53964,  73163,  106736, 76217,  128590,
    116138, 66841,  109265, 106285, 64013,  78357,  125640, 145761, 139426, 127558,
    135076, 130989, 68054,  134669, 144482, 125870, 112818, 60193,  107162, 112557,
    115972, 50890,  148652, 89547,  120228, 85967,  103941, 130915, 129496, 66401,
    87018,  149539, 105847, 60981,  82610,  134396, 121711, 142655, 104400, 103752
};

int day1p1(void) {
    int sum = 0, nummodules = 100;

    for (int i = 0; i < nummodules; i++)
        sum += modules[i] / 3 - 2;

    return sum;
}

int day1p1n(int n) {
    int64_t sum = 0;

    for (int i = 1; i <= n; i++)
        sum += i / 3 - 2;
    
    return sum;
}

int day1p2(void) {
    int sum = 0, mfsum = 0, nummodules = 100;

    for (int i = 0; i < nummodules; i++) {
        mfsum = 0;
        for (int currentf = modules[i] / 3 - 2; currentf > 0; currentf = currentf / 3 - 2)
            mfsum += currentf;
        sum += mfsum;
    }

    return sum;
}

int day1p2n(int n) {
    int64_t sum = 0, mfsum = 0;

    for (int i = 1; i <= n; i++) {
        mfsum = 0;
        for (int currentf = i / 3 - 2; currentf > 0; currentf = currentf / 3 - 2)
            mfsum += currentf;
        sum += mfsum;
    }

    return sum;
}

// Day 2 ///////////////////////////////////////////////////////////////////////////////////////////

int instructions[] = {
    1,   0,    0,   3,
    1,   1,    2,   3,
    1,   3,    4,   3,
    1,   5,    0,   3,
    2,  10,    1,  19,
    1,  19,    5,  23,
    1,  23,    9,  27,
    2,  27,    6,  31,
    1,  31,    6,  35,
    2,  35,    9,  39,
    1,   6,   39,  43,
    2,  10,   43,  47,
    1,  47,    9,  51,
    1,  51,    6,  55,
    1,  55,    6,  59,
    2,  59,   10,  63,
    1,   6,   63,  67,
    2,   6,   67,  71,
    1,  71,    5,  75,
    2,  13,   75,  79,
    1,  10,   79,  83,
    1,   5,   83,  87,
    2,  87,   10,  91,
    1,   5,   91,  95,
    2,  95,    6,  99,
    1,  99,    6, 103,
    2, 103,    6, 107,
    2, 107,    9, 111,
    1, 111,    5, 115,
    1, 115,    6, 119,
    2,   6,  119, 123,
    1,   5,  123, 127,
    1, 127,   13, 131,
    1,   2,  131, 135,
    1, 135,   10,   0,
    99,
    2,  14,    0,   0
};

int day2p1(void) {
    int *pcpy;
    pcpy = malloc(sizeof(instructions));
    memcpy(pcpy, instructions, sizeof(instructions));

    pcpy[1] = 12;
    pcpy[2] = 2;

    for (int pos = 0; pcpy[pos] != 99; pos += 4) {
        switch (pcpy[pos]) {
            case 1:
                pcpy[pcpy[pos+3]] = pcpy[pcpy[pos+1]] + pcpy[pcpy[pos+2]];
                break;
            case 2:
                pcpy[pcpy[pos+3]] = pcpy[pcpy[pos+1]] * pcpy[pcpy[pos+2]];
                break;
            default:
                printf("Invalid instruction %d at %d\n", pcpy[pos], pos);
                free(pcpy);
                return -1;
        }
    }

    free(pcpy);
    return pcpy[0];
}

int day2p2(void) {
    int *pcpy;
    pcpy = malloc(sizeof(instructions));
    
    for (int noun = 0; noun <= 99; noun++) {
        for (int verb = 0; verb <= 99; verb++) {
            memcpy(pcpy, instructions, sizeof(instructions));

            pcpy[1] = noun;
            pcpy[2] = verb;
            
            for (int pos = 0; pcpy[pos] != 99; pos += 4) {
                switch (pcpy[pos]) {
                    case 1:
                        pcpy[pcpy[pos+3]] = pcpy[pcpy[pos+1]] + pcpy[pcpy[pos+2]];
                        break;
                    case 2:
                        pcpy[pcpy[pos+3]] = pcpy[pcpy[pos+1]] * pcpy[pcpy[pos+2]];
                        break;
                    default:
                        printf("Invalid instruction %d at %d\n", pcpy[pos], pos);
                        free(pcpy);
                        return -1;
                }
            }

            if (pcpy[0] == 19690720) {
                free(pcpy);
                return noun * 100 + verb;
            }
        }
    }

    printf("Noun and verb exceeded 99\n");
    free(pcpy);
    return -1;
}

// Main ////////////////////////////////////////////////////////////////////////////////////////////

int main(void) {

    printf("%d\n", day2p2());

    return 0;
}