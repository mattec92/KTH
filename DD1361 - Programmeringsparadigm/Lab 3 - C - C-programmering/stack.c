#include <stdlib.h>
#include <stdio.h>

int main(int argc, char * argv[]) {
long long l1 = 1;
long long l2 = 2;

unsigned int i1 = 3;
unsigned int i2 = 4;

char c1[5] = "heheh";
char c2[5] = "hoho\0";

printf("%-10s %-10s %-10s %-10s\n", "Name", "Value", "Size", "Address");
printf("%-10s %-10llu %-10lu %-10p\n", "l1", l1, sizeof(l1), &l1);
printf("%-10s %-10llu %-10lu %-10p\n", "l2", l2, sizeof(l2), &l2);
printf("%-10s %-10u %-10lu %-10p\n", "i1", i1, sizeof(i1), &i1);
printf("%-10s %-10u %-10lu %-10p\n", "i2", i2, sizeof(i2), &i2);
printf("%-10s %-10s %-10lu %-10p\n", "c1", c1, sizeof(c1), &c1);
printf("%-10s %-10s %-10lu %-10p\n", "c2", c2, sizeof(c2), &c2);
}

