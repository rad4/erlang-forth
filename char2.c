#include <stdio.h>
#define MAXMESG 255

typedef unsigned char byte;

int main(int argc, char *argv[])
{
  char *a;
  byte buffer[MAXMESG+2];
  int n;
  while((n=read(fileno(stdin), buffer, 255))>0)
    {
        printf("This is the buffer output: %s \n",buffer);
    }

  return 0;

}
  
  
