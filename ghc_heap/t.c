#include <stdio.h>
#include <stdlib.h>

int* new_memory() { return (int*)malloc(10); }

void delete_memory(int* x) { free(x); }
