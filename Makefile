# Makefile

COMPILER	:=  gfortran
VPATH		:=  Source
BINDIR		:=  Bin
OBJECTDIR	:=  Objects
FFLAGS		:=  -g -O3 -static -J$(OBJECTDIR)
FFLAGSDebug 	:=  -g -Wall -static -fcheck=all -J$(OBJECTDIR)

OBJECTS := $(BINDIR)/utilities.o $(BINDIR)/quicksort.o $(BINDIR)/SparseKit.o $(BINDIR)/main.o

main: $(OBJECTS)
	$(COMPILER) $(FFLAGS) $(OBJECTS) -o main

debug: $(OBJECTS)
	$(COMPILER) $(FFLAGSDebug) $(OBJECTS) -o main

$(BINDIR)/main.o : main.f90
	$(COMPILER) $(FFLAGS) -c $^ -o $@ 

$(BINDIR)/%.o : $(VPATH)/%.f90
	$(COMPILER) $(FFLAGS) -c $^ -o $@ 
clean:
	rm -f $(BINDIR)/*.o $(OBJECTDIR)/*.mod main
