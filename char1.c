#include <stdio.h>
#define MAXMESG 255

typedef unsigned char byte;

int main(int argc, char *argv[])
{
  char *a;
  byte buffer[MAXMESG+2];
  buffer[0] = 128;
  buffer[1] = 232;
  byte *bb = &buffer[2]; 
  a = fgets(bb,MAXMESG,stdin);
  printf("This is the buffer output: %s \n",buffer);
  printf("Second print: %s \n", bb);
  printf("First byte: %d \n", buffer[0]);
  printf("Return value: %s \n", a);

  return 0;

}
  
  
