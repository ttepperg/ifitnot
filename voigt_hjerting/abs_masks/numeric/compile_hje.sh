#!/bin/bash

g77 -Wall -Wsurprising -W -ffast-math -funroll-all-loops -ffortran-bounds-check -pedantic -ffixed-line-length-none -finit-local-zero -o hjerting hjerting.f
