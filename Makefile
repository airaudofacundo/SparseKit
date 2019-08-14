# Makefile

COMPILER	:=  gfortran
VPATH		:=  Source
BINDIR		:=  Bin
OBJECTDIR	:=  Objects
FFLAGS		:=  -g -O3 -J$(OBJECTDIR)
FFLAGSDebug 	:=  -g -Wall -fcheck=all -J$(OBJECTDIR)

OBJECTS := $(BINDIR)/utilities.o $(BINDIR)/quicksort.o $(BINDIR)/SparseKit.o

main: $(OBJECTS)
	$(COMPILER) $(FFLAGS) $(OBJECTS) main.f90 -o main

mainDebug: $(OBJECTS)
	$(COMPILER) $(FFLAGSDebug) $(OBJECTS) main.f90 -o main

$(BINDIR)/%.o : $(VPATH)/%.f90
	$(COMPILER) $(FFLAGS) -c $^ -o $@ 
clean:
	rm -f $(BINDIR)/*.o $(OBJECTDIR)/*.mod main
