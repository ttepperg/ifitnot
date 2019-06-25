#!/bin/bash

gmake --file=Makefile.IFIT_GFORT_${1} clean

gmake --file=Makefile.IFIT_GFORT_${1}

