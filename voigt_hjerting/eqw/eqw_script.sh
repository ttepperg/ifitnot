#!/bin/bash

# Argument: Ion
# See ions.dat for a list of available ions
#-------------------------------------------------------------------------------
  for b in {10..150..2}
   do
   echo '#'
   for i in {13..20}
   do
    for Nion in $(seq -f "${i}.%0.2g" 0 2 8) ;
    do
     echo "eqw ${1} ${Nion} ${b}." | sh | awk '{print $2,$1,$3}'
    done # over column density
   done # over column density
   echo 
   echo 
   echo '# For labelling using gnuplot'
   echo "eqw ${1} ${Nion} ${b}." | sh | awk '{print $2, "20.5", $3}'
   echo 
   echo 
  done # over b-value
#-------------------------------------------------------------------------------

exit

#-------------------------------------------------------------------------------
  for i in {13..18}
  do
   for Nion in $(seq -f "${i}.%0.2g" 0 2 8) ;
   do
    echo '#'
    for b in {10..150..2}
    do
     echo "eqw ${1} ${Nion} ${b}." | sh
    done # over b-value
    echo 
    echo 
    echo '# For labelling using gnuplot'
    echo "eqw ${1} ${Nion} ${b}." | sh | awk '{print $1, "155.", $3}'
    echo 
    echo 
   done # over column density
  done # over column density
#-------------------------------------------------------------------------------

# To merge two files with equivalent widths for different ions:
# paste Ion1.tbl Ion2.tbl | awk '{print $1, $2, $3, $6}' > test.tbl
