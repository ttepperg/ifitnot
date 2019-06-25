TARGET = lyman
FC = g77
FFLAGS = -O -Wall -Wsurprising -W -ffast-math -funroll-all-loops -ffortran-bounds-check -pedantic -ffixed-line-length-none -finit-local-zero
LIBS = -lm

OBJECTS = lyman.o

lyman:	$(OBJECTS)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS) $(LIBS)

clean:	rm *.o

lyman.o: lyman.f parameters.f
