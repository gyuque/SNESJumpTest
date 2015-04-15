CL65	= ../cc65/bin/cl65.exe
LD65	= ../cc65/bin/ld65.exe

#-------------------------------------------------------------------------------
CSOURCES =

ASMSOURCES = GameTest.asm \
             initreg.asm \
             GraphicsUtils.asm

OBJECTS	=	$(CSOURCES:.c=.o) $(ASMSOURCES:.asm=.o)

LIBRARIES =
#-------------------------------------------------------------------------------
all :	$(OBJECTS) $(LIBRARIES)
	$(LD65) -o GameTest.smc --config GameTest.cfg --obj $(OBJECTS)

.SUFFIXES : .asm .o

.c.o :
	$(CL65) -t none -o $*.o -c -O $*.c

.asm.o :
	$(CL65) -t none -o $*.o -c $*.asm

clean :
	rm *.smc
	rm *.o
