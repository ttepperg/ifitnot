#!/bin/bash
# Calls profiles_hdf5_loop several times
#
# Arguments are Simulation Ion S/N (T/F) S/N-value

profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 100
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 100
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 50
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 50
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 30
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 30
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 10
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 10
