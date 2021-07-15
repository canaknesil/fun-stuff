#include <stdio.h>

extern char _binary_hello_world_txt_start;
extern char _binary_hello_world_txt_end;

void main()
{
  char*  p = &_binary_hello_world_txt_start;
  
  while ( p != &_binary_hello_world_txt_end ) putchar(*p++);
}

