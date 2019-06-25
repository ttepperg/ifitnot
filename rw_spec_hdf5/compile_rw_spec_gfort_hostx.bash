#!/bin/bash

gmake --file=Makefile.READ_SPEC_HDF5_GFORT_${1} clean
gmake --file=Makefile.WRITE_FIT_HDF5_GFORT_${1} clean

gmake --file=Makefile.READ_SPEC_HDF5_GFORT_${1}
gmake --file=Makefile.WRITE_FIT_HDF5_GFORT_${1}

