#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h> // needed for memset
 
int main(int argc,char** argv)
{
        struct termios tio;
        struct termios stdio;
        int tty_fd;
        fd_set rdset;
	typdef unsigned char byte
 
        unsigned char c='D';
 
        printf("Please start with %s /dev/ttyS1 (for example)\n",argv[0]);
        memset(&stdio,0,sizeof(stdio));
        stdio.c_iflag=0;
        stdio.c_oflag=0;
        stdio.c_cflag=0;
        stdio.c_lflag=0;
        stdio.c_cc[VMIN]=1;
        stdio.c_cc[VTIME]=0;
        tcsetattr(STDOUT_FILENO,TCSANOW,&stdio);
        tcsetattr(STDOUT_FILENO,TCSAFLUSH,&stdio);
        fcntl(STDIN_FILENO, F_SETFL, O_NONBLOCK);       // make the reads non-blocking
 
 
 
 
      
        while (c!='q')
        {
	        
	    
//if new data is available on the serial port, print it out
	  if (read(STDIN_FILENO,&c,1)>0)
	    write(STDOUT_FILENO,&c,1);
 // if new data is available on the console, send it to the serial port
        
	  write(STDOUT_FILENO,"Done\n",4);
	}
 
       
}
