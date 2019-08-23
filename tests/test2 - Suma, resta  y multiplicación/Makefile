# Makefile

COMPILER	:=  gfortran
LIB		:=  $(HOME)/Documentos/SparseKit/SparseKit/Libraries
LIBOBJECTS	:=  $(HOME)/Documentos/SparseKit/SparseKit/Libraries/Objects
FFLAGS		:=  -g -O3
FFLAGSDebug 	:=  -g -Wall -fcheck=all

OBJECTS   := main.o
LIBRARIES := $(LIB)/sparseLib.a

main: $(OBJECTS)
	$(COMPILER) $(FFLAGS) $(OBJECTS) $(LIBRARIES) -I$(LIBOBJECTS) -L$(LIB) -o main

run: $(OBJECTS)
	$(COMPILER) $(FFLAGS) $(OBJECTS) -I$(LIBOBJECTS) -L$(LIB) $(LIBRARIES) -o main;./main

debug: $(OBJECTS)
	$(COMPILER) $(FFLAGSDebug) $(OBJECTS) -I$(LIBOBJECTS) -L$(LIB) $(LIBRARIES) -o main

main.o : main.f90
	$(COMPILER) $(FFLAGS) -c $^ -I$(LIBOBJECTS) -o $@ 

clean:
	rm -f main main.o
#gfortran main.f90 Libraries/sparseLib.a -ILibraries/Objects -o main
