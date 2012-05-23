CC	= gcc
CFLAGS	= -Wall -pedantic $(INCLUDE) -fPIC -shared -g
INCLUDE	= -I/usr/lib/erlang/erts-5.9.1/include -I/usr/local/include 
LDFLAGS	= -L/usr/local/lib
LIBS    = -lwiringPi

SRC	=	pwm_nif.c
OBJ	=	pwm_nif.so

all: pwm_nif

pwm_nif:
	$(CC) $(CFLAGS) $(LDFLAGS) $(LIBS) -o pwm_nif.so pwm_nif.c 
	
clean:
	rm -f $(OBJ) *~ *.beam

