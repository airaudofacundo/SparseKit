# Makefile

COMPILER	:=  ifort
VPATH		:=  $(PWD)/Source
BINDIR		:=  $(PWD)/Bin
LIBDIR		:=  $(PWD)/SparseLibrary-v1.0
OBJECTDIR	:=  $(LIBDIR)/Objects
FFLAGS		:=  -Ofast -qopenmp -free -check bounds -mkl -liomp5 -lpthread -ldl -traceback -module $(OBJECTDIR)
FFLAGSDebug 	:=  -g -Wall -static -fcheck=all -J$(OBJECTDIR)

OBJECTS := $(BINDIR)/Debugger.o $(BINDIR)/Utilities.o $(BINDIR)/Quicksort.o $(BINDIR)/SparseKit.o $(BINDIR)/main.o

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
