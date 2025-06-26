.SUFFIXES: .o .for

FC   = gfortran
LIBS = 
INC  = 
FFLAGS = -O2 -ffree-line-length-none -Wunused -fallow-argument-mismatch -std=legacy -mtune=native -fbacktrace -fcheck=bounds #-x f95-cpp-input -DWGET #-g
OBJS = read62.o   #calutils.o
EXE  = READ62.EXE

.for.o:
		${FC} ${FFLAGS} -c ${INC} $<

$(EXE): ${OBJS}
		${FC} -o ${EXE} ${OBJS} ${LIBS} 

clean:
		rm -f *.o *.mod $(EXE)

