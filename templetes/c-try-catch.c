#include <stdio.h>
#include <setjmp.h>


#define TEST_EXCEPTION  3
#define JMP_BUFF_NAME   jmpBuff
#define KISS_EXCEPTION  getExceptionStr(__KISS_EXC_NUM__)

jmp_buf JMP_BUFF_NAME;

#define raise(N)   longjmp(JMP_BUFF_NAME, N)
#define try int __KISS_EXC_NUM__ = setjmp(JMP_BUFF_NAME); if (__KISS_EXC_NUM__ == 0) 
#define catch else

static char *getExceptionStr(int n) {
    switch (n) {
        case TEST_EXCEPTION: return "Test exception";
        default: return "Unknown exception";
    }
}



void foo()
{
    raise(TEST_EXCEPTION); 
}

int main (void)
{
    try {
        printf ("This code is inside the try statement.\n");
        foo();
    } catch { // catch
        printf ("Error happened: %s\n", KISS_EXCEPTION);
    }
    printf("After.\n");
    return 0;
}