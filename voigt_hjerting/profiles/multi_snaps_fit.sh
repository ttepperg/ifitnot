#!/bin/bash
# Calls profiles_hdf5_loop several times
#
# Arguments are Simulation Ion S/N (or 'inf' for no noise)

#profiles_hdf5_loop ${1} short 0.000 1 1000 ${2} ${3}
#profiles_hdf5_loop ${1} short 0.125 1 1000 ${2} ${3}
#profiles_hdf5_loop ${1} short 0.250 1 1000 ${2} ${3}
#profiles_hdf5_loop ${1} short 0.375 1 1000 ${2} ${3}
#profiles_hdf5_loop ${1} short 0.500 1 1000 ${2} ${3}
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 50
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 50
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 50
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.250 1 5000 o6 50
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 50
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.250 1 5000 ne8 50
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 30
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.250 1 5000 o6 30
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 30
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.250 1 5000 ne8 30
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 o6 100
profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.500 1 5000 ne8 100
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.250 1 5000 o6 10
#profiles_hdf5_loop REF_L050N512_snaps_5000_LOS short 0.250 1 5000 ne8 10
