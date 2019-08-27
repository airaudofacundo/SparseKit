# Makefile

COMPILER	:=  gfortran
VPATH		:=  $(PWD)/Source
BINDIR		:=  $(PWD)/Bin
LIBDIR		:=  $(PWD)/SparseLibrary-v0.9
OBJECTDIR	:=  $(LIBDIR)/Objects
FFLAGS		:=  -g -O3 -static -J$(OBJECTDIR)
FFLAGSDebug 	:=  -g -Wall -static -fcheck=all -J$(OBJECTDIR)

OBJECTS := $(BINDIR)/utilities.o $(BINDIR)/quicksort.o $(BINDIR)/SparseKit.o $(BINDIR)/main.o

main: $(OBJECTS)
	$(COMPILER) $(FFLAGS) $(OBJECTS) -o main

run: $(OBJECTS)
	$(COMPILER) $(FFLAGS) $(OBJECTS) -o main; ./main

library: $(OBJECTS)
	ar rcv $(LIBDIR)/sparseLib.a $(OBJECTS)

debug: $(OBJECTS)
	$(COMPILER) $(FFLAGSDebug) $(OBJECTS) -o main

$(BINDIR)/main.o : main.f90
	$(COMPILER) $(FFLAGS) -c $^ -o $@ 

$(BINDIR)/%.o : $(VPATH)/%.f90
	$(COMPILER) $(FFLAGS) -c $^ -o $@ 

$(LIBDIR)/%.a : $(BINDIR)/%.o
	ar rcv $^ 

clean:
	rm -f $(BINDIR)/*.o main

cleanAll:
	rm -f $(BINDIR)/*.o $(OBJECTDIR)/*.mod main $(LIBDIR)/*.a
