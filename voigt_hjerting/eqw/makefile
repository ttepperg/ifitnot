TARGET = eqw
FC = g77
FFLAGS = -O -Wall -Wsurprising -W -ffast-math -funroll-all-loops -ffortran-bounds-check -pedantic -ffixed-line-length-none -finit-local-zero
LIBS = -lm

OBJECTS = eqw.o exp_h_ax.o id.o

eqw:	$(OBJECTS)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS) $(LIBS)

clean:	rm *.o

eqw.o: eqw.f variables_eqw.f subroutines_eqw.f
