#!/bin/bash
# check for host; since default shell is tcsh on volans;
# need to expand aliases on bash
if [ $HOSTNAME="volans.astro.physik.uni-potsdam.de" ]; then
 shopt -s expand_aliases
fi

# clear screen

clear

#-------------------------------------------------------------------------------
# tput allows use of coloured output in bash

  echo -e "------------------------------------------------------------------\n"

  echo -e "$(tput bold)$(tput setaf 3) MAKE SURE TO SET \
$(tput setaf 2)test_run=TRUE $(tput setaf 3)IN input parameter \
file $(tput setaf 5)ifit_parameters.ion\n"

  echo -e "$(tput bold)$(tput setaf 3) MAKE SURE TO SET $(tput setaf 2)profile \
$(tput setaf 3)AND $(tput setaf 2)fwhm_kms $(tput setaf 3)WITH *IDENTICAL* \
VALUES IN"
  echo -e "$(tput setaf 5) test_ifit.bash $(tput setaf 3)AND \
$(tput setaf 5)ifit_parameters.ion$(tput setaf 3), RESPECTIVELY\n"

  echo -e "$(tput bold)$(tput setaf 3) ONLY THEN COMMENT OUT THE EXIT LINE \
IN THIS SCRIPT!\n"

  echo -e \
  "$(tput sgr0)------------------------------------------------------------------\n"

# reset output colors
tput sgr0

exit

#-------------------------------------------------------------------------------
# define profile function
#profile='voigt'
profile='gauss'

# set instrumental broadening
fwhm_kms=20.0

# set file appropriate fit file suffix

#suffix='_lines'
#if [ "$(echo "$fwhm_kms > 0.0" | bc -l)" -eq 1 ]; then
# suffix='_conv'${suffix}
#fi

# set column density limits and step
logN_min=13.0
logN_max=15.0
logN_step=0.5

# set b-value limits and step
b_min=10.0
b_max=100.0
b_step=10.0

# counter to enumerate visualisation files
visual_counter=0

# remove previous files
rm -f ${HOME}/ifitnot/voigt_hjerting/abs_line_profile/output_data/*
rm -f ${HOME}/ifitnot/spec_logN_*_b_*_SN*_*.*

#-------------------------------------------------------------------------------
# for a list of valid ions see ions.dat

for ion in h1
 do

 for sn in 1000 #5 10 20 50 100 1000
 do
 
# remove previous files
rm -f ${HOME}/ifitnot/output_test_ifit/\
test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat

# write block header
 touch \
 ${HOME}/ifitnot/output_test_ifit/\
test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat
 echo \
'# in logN | in b | out logN | out b | err logN | err b | out (N/b) | in (N/b)'\
  >> \
 ${HOME}/ifitnot/output_test_ifit/\
test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat
 echo '# SN='${sn} >> \
 ${HOME}/ifitnot/output_test_ifit/\
test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat

  for logN in $(gseq ${logN_min} ${logN_step} ${logN_max})
  do

   for b_kms in $(gseq ${b_min} ${b_step} ${b_max})
   do

# generate synthetic profile
# abs_line_profile <ion> <log N> <b km/s> <S/N> <line-prof.> <LSF FWHM> <plot>

    cd ${HOME}/ifitnot/voigt_hjerting/abs_line_profile
    
    echo -e \
"./synspec ${ion} ${logN} ${b_kms} ${sn} ${profile} ${fwhm_kms}"
    ./synspec ${ion} ${logN} ${b_kms} ${sn} ${profile} ${fwhm_kms} || exit 0
pwd
    mv \
    ./output_data/\
synspec_${logN}_${b_kms}_${sn}_${profile}_${ion}.ifit \
   ${HOME}/ifitnot/spec_logN_${logN}_b_${b_kms}_SN${sn}_${profile}.dat || exit 0

# fit
    cd ${HOME}/ifitnot/

    ./ifitnot spec_logN_${logN}_b_${b_kms}_SN${sn}_${profile}.dat || exit


#-------------------------------------------------------------------------------
# set file appropriate fit file suffix

 suffix='_lines'
 if test -e \
 spec_logN_${logN}_b_${b_kms}_SN${sn}_${profile}_${ion}_conv${suffix}.fit ; then
 suffix='_'${ion}'_conv'${suffix}
 else
 suffix='_'${ion}'_noconv'${suffix}
 fi

#-------------------------------------------------------------------------------
# store input/output values

# If no lines detected, print out zeros
    lines=`cat \
spec_logN_${logN}_b_${b_kms}_SN${sn}_${profile}${suffix}.fit | wc -l`

    if [ ${lines} -eq 0 ]; then
     echo '0 0 0 0' ${logN} ${b_kms} | \
     awk '{print $1,$2,$3,$4, log(10**($5)/$6)/log(10)}' \
     > ./output_test_ifit/output_SN${sn}_${ion}_${profile}.dat
     echo ${logN} ${b_kms} >> \
     ./output_test_ifit/input_SN${sn}_${ion}_${profile}.dat
    else
     for i in $(gseq 1 1 ${lines})
     do
      echo ${logN} ${b_kms} >> \
      ./output_test_ifit/input_SN${sn}_${ion}_${profile}.dat
     done
     cat spec_logN_${logN}_b_${b_kms}_SN${sn}_${profile}${suffix}.fit | \
     awk '{print $5, $6, log($9)/log(10), $10, $5 - log($6)/log(10)}'\
     > ./output_test_ifit/output_SN${sn}_${ion}_${profile}.dat
    fi

    paste \
    ./output_test_ifit/input_SN${sn}_${ion}_${profile}.dat output_test_ifit/\
output_SN${sn}_${ion}_${profile}.dat \
    | awk '{print $0, log(10**($1)/$2)/log(10)}' \
    >> ./output_test_ifit/test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat

# clean up
    rm -f spec_logN_${logN}_b_${b_kms}_SN${sn}_${profile}*.*
    rm -f ./output_test_ifit/input_SN${sn}_${ion}_${profile}.dat
    rm -f ./output_test_ifit/output_SN${sn}_${ion}_${profile}.dat
    rm -f chi2_interval_*_${ion}.dat
    rm visual_*_${ion}*.dat
    rm visual_*_${ion}*.plot

# save visualisation files (to be done)

   done #loop over b-value

# write block separation
  echo '' >> \
  output_test_ifit/test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat

  done #loop over column density

# write block separation
  echo '' >> \
  output_test_ifit/test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat
  echo '' >> \
  output_test_ifit/test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat

# visualise results for each S/N value for each ion
# create settings file

echo \
'plotfile="'output_test_ifit/test_io_${ion}_${profile}_SN${sn}_PSF${fwhm_kms}.dat'"' \
> output_test_ifit/settings.plot

#replace underscore (_) by hyphen (-)
echo 'ion="'${ion/_/-}'"' >> output_test_ifit/settings.plot

echo 'profile="'${profile}'"' >> output_test_ifit/settings.plot

echo 'fwhm='${fwhm_kms} >> output_test_ifit/settings.plot

echo 'logNlim='${logN_min}-${logN_step} >> output_test_ifit/settings.plot
echo 'logNmin='${logN_min}-${logN_step} >> output_test_ifit/settings.plot
echo 'logNmax='${logN_max}+${logN_step} >> output_test_ifit/settings.plot

echo 'bmin='${b_min}-${b_step} >> output_test_ifit/settings.plot
echo 'bmax='${b_max}+${b_step} >> output_test_ifit/settings.plot

echo 'sn='${sn} >> output_test_ifit/settings.plot

gnuplot ./output_test_ifit/plot_test_generic.plot -persist

 done #loop over signal-to-noise

done #loop over ion

# END OF SCRIPT

# remove files

rm -f ${HOME}/ifitnot/voigt_hjerting/abs_line_profile/output_data/*

#-------------------------------------------------------------------------------
# tput allows use of coloured output in bash

  echo -e "------------------------------------------------------------------\n"

  echo -e "$(tput bold)$(tput setaf 3) IF SOMETHING GOES WRONG:\n"

  echo -e "$(tput bold)$(tput setaf 3) MAKE SURE TO SET \
$(tput setaf 2)test_run=TRUE $(tput setaf 3)IN input parameter \
file $(tput setaf 5)ifit_parameters.ion\n"

  echo -e "$(tput bold)$(tput setaf 3) MAKE SURE TO SET $(tput setaf 2)profile \
$(tput setaf 3)AND $(tput setaf 2)fwhm_kms $(tput setaf 3)WITH *IDENTICAL* \
VALUES IN"
  echo -e "$(tput setaf 5) test_ifit.bash $(tput setaf 3)AND \
$(tput setaf 5)ifit_parameters.ion}$(tput setaf 3), RESPECTIVELY\n"

  echo -e \
  "$(tput sgr0)------------------------------------------------------------------\n"

# reset output colors
tput sgr0
#-------------------------------------------------------------------------------

exit
