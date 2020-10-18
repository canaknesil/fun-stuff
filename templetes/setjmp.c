#include <stdio.h>
#include <setjmp.h>

jmp_buf jmp1;

void foo()
{
    longjmp (jmp1, 3); // raise exception 
}

int main (void)
{
    if (setjmp(jmp1)==0) { // try
        printf ("This code is inside the try statement.\n");
        tryjump();
    } else { // catch
        printf ("Error happened.\n");
    }
    printf("After.\n");
    return 0;
}