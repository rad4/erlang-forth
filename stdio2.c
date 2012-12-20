/* Basic setup to read erlang message and output to stdout
Read 2 bytes from stdin
Convert to length
Read length bytes into buffer
Write length bytes to stdout

Test at command line with pipe
$ printf "\x0\x4Powder" | ./stdio1
Powd
$ printf "\x0\x4\x41lexander" | ./stdio1 use \x41 instead of A or \x4 becomex \x4A
Alex
\x0 is byte zero in hexadecimal
length is therefore output is 


*/

#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <string.h> 

#define MAXMESG 1024

typedef unsigned char byte;

byte stdinbuff[MAXMESG+2];  /* read from stdin into this buffer */
int mesglen;


int read_stdincmd(byte *buff);
int read_stdin(byte *buff, int length);
int get_16bit(byte *buff);

int stdinfd, stdoutfd; /* fd = file desciptor */
int ttyfd; /* serial port file descriptor */

int main(int argc, char *argv[])
{
   mesglen = 0;
   stdinfd = fileno(stdin);
   stdoutfd = fileno(stdout);
   struct termios ttyconfig;
   
   memset(&ttyconfig,0,sizeof(ttyconfig)); // clear out all settings
   ttyconfig.c_iflag=0;
   ttyconfig.c_oflag=0;
   ttyconfig.c_lflag=0;
   ttyconfig.c_cflag=CS8|CREAD|CLOCAL;

   while((mesglen=read_stdincmd(stdinbuff)) > 0)
     {
       /* at this point stdin has been succesfully read
	  length bytes have been over written */
       write(stdoutfd,stdinbuff,mesglen);
     }
}

int read_stdin(byte *buff, int length)
/* read until we get length requested,
   allows for fragmented reads */

{
  int x = 0;
  int recvd = 0; /* accumulator for received bytes */
  do
    {
      x = read(stdinfd, buff+recvd, length);
      recvd += x;
    } while (recvd < length);
	   if (x <= 0)
	     return x;
	   return length;
}
   

int read_stdincmd(byte *buff)
{
  /* 1. read first two btyes from erlang which contain lenths bytes using
     read_stdin;  MSB first
     2. extract length 
     3. read this number of bytes from stdin using read_stdin

  */
  int len; /* hold result of converting length bytes */

  if (read_stdin(buff,2) != 2)
    {
    printf("Here 1\n");
    return -1;
    }
  len = get_16bit(buff);
  
  if (read_stdin(buff,len) != len)
    {
    printf("Here 21\n");
    return -1;
    }
  // mesglen = len;
  return len;
}

int get_16bit(byte *buff)  
{
  return (buff[0] << 8) | buff[1];
}
