#!/bin/bash

# Reads in spectra in HDF5 format and writes out in ASCII format

# Arguments:

# simulation spectrum-type spectra-file spec_id

sim=$1
spectype=$2
specfile=$3
id=$4

./plot_spec_hdf5 ${sim} ${spectype} ${specfile} ${id} none
