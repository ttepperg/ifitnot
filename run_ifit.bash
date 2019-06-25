#!/bin/bash
# Runs ifit

# USAGE (in conjunction with OWLS/HDF5):

# run_ifit.bash <full specfile path> <specfile name> <output (ps/term)>

# 'specfile' refers to the ascii file with spectrum to be fitted; this file
# should contain three columns:
#
# wavelength | flux | noise

# script messages
 statmsg='STATUS (run_ifit.bash):'
 warnmsg='WARNING (run_ifit.bash):'
 errmsg='ERROR (run_ifit.bash):'

#-------------------------------------------------------------------------------
# Check number of parameters
if test $# -ne 3 ; then

echo
echo -e "$(tput bold)$(tput setaf 2) USAGE:"
echo -e "$(tput setaf 7) \
./run_ifit.bash <full specfile path> <specfile name> <output (ps/term)>\n"

# reset output colors
tput sgr0

exit

fi

#-------------------------------------------------------------------------------
# directories
 ifitdir=${HOME}'/ifitnot/'
 plotspecdir=${ifitdir}'/plotspec_ascii/'

#-------------------------------------------------------------------------------
# Define input parameters
 path=${1}
 specfile=${2}
 specfile_prefix=${specfile%.*} # extract substring previous to dot (.)
 out=${3}

#-------------------------------------------------------------------------------
  echo -e "Change into directory ${ifitdir}.\n"

  cd ${ifitdir}. || exit

#-------------------------------------------------------------------------------
  echo ''
  echo -e "$(tput bold)$(tput setaf 2)"${warnmsg}
  echo -e "$(tput sgr0)delete previous ifit output files [if existent]:"
  echo -e "*.isp *.abs *_[lines/model].fit \n"

  rm -f *.abs || exit
  rm -f *.isp || exit
  rm -f *_lines.fit || exit
  rm -f *_model.fit || exit

  echo ''
  echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
  echo -e "$(tput sgr0)copy ${path}/${specfile} to ${ifitdir}.\n"

  cp ${path}/${specfile} ${ifitdir}. || exit

  echo ''
  echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
  echo -e "$(tput sgr0)invoke ifit with following command:\n"
  echo -e "./ifitnot ${specfile}\n"

  ./ifitnot ${specfile} || exit

# set prefix to call plotspec_ascii

  ifit_out=`ls ${specfile_prefix}*.isp`
  ifit_out_prefix=${ifit_out%.*}

# check if instrumental broadening has been included by checking the existence
# of corresponding ifitnot output file

  convolution='false'
  if test -e ${ifit_out%_noconv.*}_conv_lines.fit ; then
   convolution='true'
  fi

  echo ${convolution} ${ifit_out%_noconv.*}_conv_lines.fit

  echo ''
  echo -e "$(tput bold)$(tput setaf 2)"${warnmsg}
  echo -e "$(tput sgr0)delete previous ifit output files [if existent]:"
  echo -e "${path}/${ifit_out_prefix}*.[fit/abs] / \
  ${path}/${ifit_out_prefix}.isp\n"
 
  rm -f ${path}/${ifit_out_prefix}.isp || exit
  rm -f ${path}/${ifit_out_prefix}_model.fit || exit
  rm -f ${path}/${ifit_out_prefix}_lines.fit || exit
  rm -f ${path}/${ifit_out_prefix}.abs || exit

  echo ''
  echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
  echo -e "$(tput sgr0)move ifit output files:"
  echo -e "${ifit_out_prefix}*.[fit/abs] / \
  ${ifit_out_prefix}.isp"
   echo -e "to ${path}/.\n"
 
  mv ${ifit_out_prefix}.isp ${path}/. || exit
  mv ${ifit_out_prefix}_lines.fit ${path}/. || exit
  mv ${ifit_out_prefix}_model.fit ${path}/. || exit
  mv ${ifit_out_prefix}.abs ${path}/. || exit

  echo ''
  echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
  echo -e "$(tput sgr0)delete auxiliary ifit input file ${specfile}\n"
  
  rm -f ${specfile} || exit

#-------------------------------------------------------------------------------
# instrumental broadening 

if [ "${convolution}" == "true" ]; then

	echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
	echo -e \
	"$(tput sgr0)Process file with CONVOLVED fit \
	${ifit_out%_noconv.*}__conv_lines.fit\n"

	echo ''
	echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
	echo -e "$(tput sgr0)move ifit output files:"
	echo -e "${ifit_out%_noconv.*}_conv.[fit/abs] to ${path}/.\n"

	mv ${ifit_out%_noconv.*}_conv_lines.fit ${path}/. || exit
	mv ${ifit_out%_noconv.*}_conv_model.fit ${path}/. || exit
	mv ${ifit_out%_noconv.*}_conv.abs ${path}/. || exit

	echo ''
	echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
	echo -e "$(tput sgr0)copy spectrum file:"
	echo -e "${path}/${ifit_out_prefix}.isp"
	echo -e "to auxiliary file ${path}/${ifit_out_prefix}_conv.isp\n"

	cp ${path}/${ifit_out_prefix}.isp ${path}/${ifit_out%_noconv.*}_conv.isp || exit

fi

echo ''
echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
echo -e "$(tput sgr0)move ifit output files:"
echo -e "${ifit_out_prefix}*.[gpf/plot/ps] to ${path}/.\n"

mv ${ifit_out_prefix}*.gpf ${path}/. || exit
mv ${ifit_out_prefix}*.plot ${path}/. || exit
mv ${ifit_out_prefix}*.ps ${path}/. || exit

#-------------------------------------------------------------------------------
# Visualise:
if [ "${out}" != "none" ]; then

	echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
	echo -e "$(tput sgr0)change into ${plotspecdir}.\n"

	cd ${plotspecdir} || exit

	echo -e "$(tput bold)$(tput setaf 3)"${statmsg}
	echo -e "$(tput sgr0)delete previous plot files in \
	${plotspecdir}output_data/.:"
	echo -e "$(tput sgr0)${ifit_out_prefix}.psh plotspec.plot \
	plotspec.ps\n"

	rm -f ${plotspecdir}/output_data/${ifit_out_prefix}.psh || exit
	rm -f ${plotspecdir}/output_data/plotspec.plot || exit
	rm -f ${plotspecdir}/output_data/plotspec.ps || exit

	cd ${plotspecdir} || exit

	echo -e "$(tput sgr0)invoke plotspec_ascii with following command:\n"
	echo -e "./plot_spec_ascii ${path} ${ifit_out_prefix} ${out}_wl\n"

	./plot_spec_ascii ${path} ${ifit_out_prefix} ${out}_wl || exit

#-------------------------------------------------------------------------------
	echo -e "Change into directory ${ifitdir}.\n"

	cd ${ifitdir}. || exit

#-------------------------------------------------------------------------------
# instrumental broadening 

	 if [ "${convolution}" == "true" ]; then

		  echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
		  echo -e \
		  "$(tput sgr0)Process file with CONVOLVED fit \
		${ifit_out%_noconv.*}_conv_lines.fit\n"

		  echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
		  echo -e "$(tput sgr0)change into ${plotspecdir}.\n"

		  cd ${plotspecdir} || exit

		  echo -e "$(tput bold)$(tput setaf 3)"${warnmsg}
		  echo -e "$(tput sgr0)delete previous plot files in \
		${plotspecdir}output_data/:"
		  echo -e "$(tput sgr0)${ifit_out%_noconv.*}_conv.psh plotspec_conv.plot \
		plotspec_conv.ps\n"

		  rm -f ${plotspecdir}/output_data/${ifit_out%_noconv.*}_conv.psh || exit
		  rm -f ${plotspecdir}/output_data/plotspec_conv.plot || exit
		  rm -f ${plotspecdir}/output_data/plotspec_conv.ps || exit

		  echo -e "$(tput sgr0)invoke plotspec_ascii with following command:\n"
		  echo -e "./plot_spec_ascii' ${path} ${ifit_out%_noconv.*}_conv ${out}_wl\n"

		  ./plot_spec_ascii ${path} ${ifit_out%_noconv.*}_conv ${out}_wl || exit

		  echo ''
		  echo -e "$(tput bold)$(tput setaf 2)"${statmsg}
		  echo -e "$(tput sgr0)delete auxiliary spectrum file:"
		  echo -e "${ifit_out%_noconv.*}_conv.isp\n"
 
		  rm -f ${path}/${ifit_out%_noconv.*}_conv.isp || exit

	 fi # convolved fit?

#-------------------------------------------------------------------------------

fi # visualise

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
echo -e "Change into directory ${ifitdir}.\n"

cd ${ifitdir}. || exit

#-------------------------------------------------------------------------------

echo -e "$(tput bold)$(tput setaf 2)"${statmsg}"$(tput sgr0) Done.\n"

#-------------------------------------------------------------------------------


echo -e "------------------------------------------------------------------\n"


# tput allows use of coloured output in bash

echo -e "$(tput sgr0) IMPORTANT NOTE:\n"
echo -e "$(tput bold)$(tput setaf 3) IF ANYTHING GOES WRONG, YOU MAY WANT TO \
CHECK THE VALUES OF:"
echo -e "  $(tput setaf 2)spectrum_shifted_str$(tput setaf 3)"
echo -e "  $(tput setaf 2)test_run$(tput setaf 3)"
echo -e " IN input parameter file $(tput setaf 5)ifit_parameters.gen\n"
echo -e "$(tput bold)$(tput setaf 3) AND CHECK THAT THE *FULL* PATH TO THE \
SPECTRUM FILE IS CORRECTLY GIVEN WHEN INVOKING THIS SCRIPT"
echo -e \
"$(tput sgr0)----------------------------------------------------------------\
--\n"

# reset output colors
tput sgr0

exit

#-------------------------------------------------------------------------------
# END 
echo -e "$(tput bold)$(tput setaf 2)"${statmsg}"$(tput sgr0) Done.\n"

# reset output colors
tput sgr0

exit
