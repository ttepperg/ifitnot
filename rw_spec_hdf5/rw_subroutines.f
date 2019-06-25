!       write_spec_hdf5, write_fit_hdf5: Subroutines and Functions
        
!       ------------------------------------------------------------------------
        subroutine read_input_params_r

!       Reads input paramters and performs a small sanity check 

        use common_strings
        use common_nums
        
        implicit none
        
        integer :: argcount

!       Call intrinsic IArgC()
        
        argcount = IArgC()
         
        if (argcount .lt. 6) then
          write(6,*) 'USAGE: read_spec_hdf5 <simulation> <short|long> '//
     &          '<redshift (e.g. 0.250)> <spec ID> <ion|all> <noise(T|F)> '//
     &          '<S/N(if noise=T)>'
         stop 1
              end if

        call GetArg(6,noise_added)
        
        if (trim(noise_added) .eq. 'T') then
               
         noise_added = 'TRUE'
         
         if (argcount .ne. 7) then
          write(6,*) 'USAGE: read_spec_hdf5 <simulation> <short|long> '//
     &          '<redshift (e.g. 0.250)> <spec ID> <ion|all> <noise(T|F)> '//
     &          '<S/N(if noise=T)>'
          stop 1
               end if

         call GetArg(1,simulation)

         call GetArg(2,spectype)

         call GetArg(3,redshift_str)
         read(redshift_str, '(f5.3)') z_box
         specwizfile =
     &         'Spectrum_'//trim(spectype)//'_z'//trim(redshift_str)//'.hdf5'

         call GetArg(4,spec_id)
         read(spec_id,'(i5)') specid_int

         call GetArg(5,ion)

         noisy = .true.
         call GetArg(7,signaltonoise_str)
         read(signaltonoise_str,'(f5.0)') signaltonoise

        else if (trim(noise_added) .eq. 'F') then
               
         noise_added = 'FALSE'
         
         if (argcount .ne. 6) then
          write(6,*) 'USAGE: read_spec_hdf5 <simulation> <short|long> '//
     &          '<redshift (e.g. 0.250)> <spec ID> <ion|all> <noise(T|F)> '//
     &          '<S/N(if noise=T)>'
          stop 1
               end if

         call GetArg(1,simulation)

         call GetArg(2,spectype)

         call GetArg(3,redshift_str)
         read(redshift_str, '(f5.3)') z_box
         specwizfile =
     &         'Spectrum_'//trim(spectype)//'_z'//trim(redshift_str)//'.hdf5'

         call GetArg(4,spec_id)
         read(spec_id,'(i5)') specid_int

         call GetArg(5,ion)
         
         noisy = .false.
         signaltonoise = 1.0d3
         signaltonoise_str = 'inf'

        else
         
         write(6,*) trim(errmsg)//'Need to specify noise (T|F).'
         stop 1

        end if
        
        end subroutine read_input_params_r

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine read_input_params_w

!       Reads input paramters and performs a small sanity check 

        use common_strings
        use common_nums
        
        implicit none
        
        integer :: argcount

!       Call intrinsic IArgC()
        
        argcount = IArgC()

              if (argcount .ne. 8) then
         write(6,*) 'USAGE: write_fit_hdf5 <simulation> <short|long> '//
     1         '<specfile> <spec ID> <fitfile> <linesfile> <ewrfile> <ion|all>'
         stop 1
              end if

        call GetArg(1,simulation)

        call GetArg(2,spectype)

        call GetArg(3,fitfile_hdf5)

        call GetArg(4,spec_id)

        call GetArg(5,fitfile)

        call GetArg(6,linesfile)

        call GetArg(7,ewrfile)

        call GetArg(8,ion)

        
        end subroutine read_input_params_w

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine create_header_long(file_num)

!       Creates a header in file 'file_num'
        
        use hdf5_wrapper         
        use common_arrays_rw
        use common_strings
        use common_nums
        
        implicit none
         
        integer :: file_num

         call hdf5_create_group(file_num,'/Header')


!        General information
         attributename='/Header/Simulation'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(simulation))
         attributename='/Header/Specfile'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(specwizfile))
         attributename='/Header/Redshift_Abs_Min'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         z_min)
         attributename='/Header/Redshift_Abs_Max'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         z_max)
         attributename='/Header/Redshift_QSO'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         z_qso)

!        Time stamp
         attributename='/Header/TimeStamp'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         datetime) 

!        Spectra information and spectral properties
         attributename='/Header/Number_Of_Spectra'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         total_los)
         attributename='/Header/SpectrumID_List'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         spec_list)
         attributename='/Header/Noise_Added'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(noise_added))
         attributename='/Header/SignalToNoise'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         signaltonoise)
         attributename='/Header/PixSize_Angstrom'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         pxsize_ang)
         attributename='/Header/Spectrum_Convolved'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(spectrum_convolved_str))
         attributename='/Header/FWHM_Kms'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         fwhm_kms)
         attributename='/Header/PixSizeKms_Before_Convolution'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         pxsize_noconv)
         attributename='/Header/Number_Of_Pixels'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         nveloc)
         
!       Atomic data
         call hdf5_create_group(file_num,'/Header/Ion_AtomicData')
         attributename='/Header/Ion_AtomicData/Ion'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(ion))
         attributename='/Header/Ion_AtomicData/RestWavelength_Ang'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         wl_central)
         attributename='/Header/Ion_AtomicData/Oscillator_Strength'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         f_osc)
         
        end subroutine create_header_long
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine create_header_short(file_num)

!       Creates a header in file 'file_num'

        use hdf5_wrapper         
        use common_arrays_rw
        use common_strings
        use common_nums

        implicit none
         
        integer :: file_num

!        General information
         call hdf5_create_group(file_num,'/Header')
         attributename='/Header/Simulation'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(simulation))
         attributename='/Header/Specfile'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(specwizfile))
         attributename='/Header/Redshift'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         z_box)

!        Time stamp
         attributename='/Header/TimeStamp'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         datetime) 

!        Spectra information and spectral properties
         attributename='/Header/Number_Of_Spectra'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         total_los)
         attributename='/Header/SpectrumID_List'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         spec_list)
         attributename='/Header/Noise_Added'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(noise_added))
         attributename='/Header/SignalToNoise'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         signaltonoise)
         attributename='/Header/Spectrum_Convolved'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(spectrum_convolved_str))
         attributename='/Header/FWHM_Kms'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         fwhm_kms)
         attributename='/Header/PixSizeKms_Before_Convolution'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         pxsize_noconv)
         attributename='/Header/Number_Of_Pixels'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         nveloc)

!       Atomic data
         call hdf5_create_group(file_num,'/Header/Ion_AtomicData')
         attributename='/Header/Ion_AtomicData/Ion'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(ion))
         attributename='/Header/Ion_AtomicData/RestWavelength_Ang'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         wl_central)
         attributename='/Header/Ion_AtomicData/Oscillator_Strength'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         f_osc)
        
        end subroutine create_header_short
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine update_header(filename)

!       Updates the values of the attributes 'Number_Of_Spectra' and
!       'SpectrumID_List' in the header of file 'filename'

        use hdf5_wrapper         
        use common_arrays_rw
        use common_strings
        use common_nums

        implicit none
        
        integer :: file_num, err, i

        character(len=*), intent(in) :: filename

!       Open file and read current number of spectra          
          call hdf5_open_file(file_num,trim(filename))

          attributename='/Header/Number_Of_Spectra'
          call hdf5_read_attribute(file_num,trim(attributename),total_los)

!       Allocate memory for auxiliary spectra ID list
          allocate(spec_list_aux(total_los), stat=err)
*      err = 0 if succeeded, otherwise non-zero error code
          if (err .ne. 0) then
            write (6,*) trim(errmsg)//'Allocation failed: ', err
            stop 1
          end if

!       Read current spectra ID list into auxiliary list
          attributename='/Header/SpectrumID_List'
          call hdf5_read_attribute(file_num,trim(attributename),spec_list_aux)

!       Update number of spectra          
          total_los = total_los + 1

!       Allocate memory for actual spectra ID list

          allocate(spec_list(total_los), stat=err)
!       err = 0 if succeeded, otherwise non-zero error code
          if (err .ne. 0) then
            write (6,*) trim(errmsg)//'Allocation failed: ', err
            stop 1
          end if

!       Update spectra ID list, writing auxiliary list into actual list and 
!       appending current spec_id to the latter
          
          do i=1,total_los-1
           spec_list(i) = spec_list_aux(i)
          end do
          spec_list(total_los) = specid_int

!       Sort array elements in ascending order
          call sort(total_los,spec_list)

!       Close file

          call hdf5_close_file(file_num)

!       Delete existing attribute 'Number_Of_Spectra'
!       Note that for this subroutine, file MUST NOT be in use; also, not
!       the file_num but the filename itself is passed on

          call hdf5_delete_attribute(filename,'/Header','Number_Of_Spectra')
          
!       Delete existing attribute 'SpectrumID_List' (IDEM)

          call hdf5_delete_attribute(filename,'/Header','SpectrumID_List')

!       Open file, re-create attribute 'number of spectra' and
!       'spectra ID list', and close

          call hdf5_open_file(file_num,trim(filename))
           
           attributename='/Header/Number_Of_Spectra'
           call hdf5_write_attribute(file_num,trim(attributename),total_los)

           attributename='/Header/SpectrumID_List'
           call hdf5_write_attribute(file_num,trim(attributename),spec_list)
          
          call hdf5_close_file(file_num)


        end subroutine update_header
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine spectrum_status_check_r(file_num,spec_num)

!       Checks in file 'file_num' first, if the current spectrum already
!       exists, if it was succesfully written, and if it has bee already
!       fitted. If any of the previous happens to be.TRUE. stop cleanly
!       printing a warning message
!       Used exlusively by write_spec_hdf5

        use hdf5_wrapper         
        use common_arrays_rw
        use common_strings
        use common_nums

        implicit none
        
        integer, intent(in) :: file_num
        integer :: tot_los, err, specidint
        integer, allocatable :: specidlist(:)
        
        character(len=*), intent(in) :: spec_num
        character(len=256) :: attname, wstatus, fstatus
        
        logical :: check

!       Convert spectrum ID to integer
         read(spec_num,'(i5)') specidint

!       Read attribute 'number of spectra'           
         attname='/Header/Number_Of_Spectra'
         call hdf5_read_attribute(file_num,trim(attname),tot_los)

!       Allocate memory for specidlist array
         allocate(specidlist(tot_los),stat=err)
*      err = 0 if succeeded, otherwise non-zero error code
         if (err .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', err
           stop 1
         end if

!       Read 'spectra ID list'
           
          attname='/Header/SpectrumID_List'
          call hdf5_read_attribute(file_num,trim(attname),specidlist)

!       Check if current spectrum already exist (maybe it is possible to
!       check directly the existence of a group in an HDF5 file?).
!       If so, check status
!       stop cleanly printing a warning message
          
          check = any(specidlist .eq. specidint)
          
          if (check) then

           print*, trim(warnmsg)//'Spectrum '//trim(spec_num)//' already'
     &//' exists in file '//trim(outfile)
                
!       Read writting-status
           
           attname='/Spectrum'//trim(spec_num)//'/Spectrum_Written'
           call hdf5_read_attribute(file_num,trim(attname),wstatus)
           if (wstatus.eq.'TRUE') then
            spectrum_written = .true.
           else
            spectrum_written = .false.
           end if
           write(6,*) trim(statmsg)//'Spectrum '//trim(spec_num)//
     &' successfully written : '//trim(wstatus)

!       Read fitting status
           
           attname='/Spectrum'//trim(spec_num)//'/Spectrum_Fitted'
           call hdf5_read_attribute(file_num,trim(attname),fstatus)
           if (fstatus.eq.'TRUE') then
            spectrum_fitted = .true.
           else
            spectrum_fitted = .false.
           end if
           write(6,*) trim(statmsg)//'Spectrum '//trim(spec_num)//
     &' successfully fitted : '//trim(fstatus)
                
           if (.not.spectrum_fitted) write(6,*) trim(statmsg)//
     &'Processing again Spectrum '//trim(spec_num)

                if (spectrum_written.and.spectrum_fitted) stop 1

          end if !check
          
        end subroutine spectrum_status_check_r
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine spectrum_status_check_w(file_num,spec_num)

!       Checks in file 'file_num' first, if the current spectrum already
!       exists, if it was succesfully written, and if it has bee already
!       fitted. If any of the previous happens to be.TRUE. stop cleanly
!       printing a warning message
!       Used exlusively by write_fit_hdf5

        use hdf5_wrapper         
        use common_arrays_rw
        use common_strings
        use common_nums

        implicit none
        
        integer, intent(in) :: file_num
        integer :: tot_los, err, specidint
        integer, allocatable :: specidlist(:)
        
        character(len=*), intent(in) :: spec_num
        character(len=256) :: attname, wstatus, fstatus
        
        logical :: check

!       Convert spectrum ID to integer
         read(spec_num,'(i5)') specidint

!       Read attribute 'number of spectra'           
         attname='/Header/Number_Of_Spectra'
         call hdf5_read_attribute(file_num,trim(attname),tot_los)

!       Allocate memory for specidlist array
         allocate(specidlist(tot_los),stat=err)
*      err = 0 if succeeded, otherwise non-zero error code
         if (err .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', err
           stop 1
         end if

!       Read 'spectra ID list'
           
         attname='/Header/SpectrumID_List'
         call hdf5_read_attribute(file_num,trim(attname),specidlist)

!       Check if current spectrum already exist (maybe it is possible to
!       check directly the existence of a group in an HDF5 file?).
!       If so, check status
!       stop cleanly printing a warning message

         check = any(specidlist .eq. specidint)

         if (check) then

!       Read writting-status
           
          attname='/Spectrum'//trim(spec_num)//'/Spectrum_Written'
          call hdf5_read_attribute(file_num,trim(attname),wstatus)
          if (wstatus.eq.'TRUE') then
           spectrum_written = .true.
          else
           spectrum_written = .false.
          end if

!       Read fitting status
           
          attname='/Spectrum'//trim(spec_num)//'/Spectrum_Fitted'
          call hdf5_read_attribute(file_num,trim(attname),fstatus)
          if (fstatus.eq.'TRUE') then
           spectrum_fitted = .true.
          else
           spectrum_fitted = .false.
          end if

               if (spectrum_written.and.spectrum_fitted) then

           print*, trim(warnmsg)//'Spectrum '//trim(spec_num)//' already'
     &           //' exists in file '//trim(outfile)
           write(6,*) trim(statmsg)//'Spectrum '//trim(spec_num)//
     &           ' successfully written : '//trim(wstatus)
           write(6,*) trim(statmsg)//'Spectrum '//trim(spec_num)//
     &           ' successfully fitted : '//trim(fstatus)
           
            stop 1
            
          end if
          
         end if
          
        end subroutine spectrum_status_check_w
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine spectrum_status_set(filename,spec_num,attname,value)

!       Sets the attribute 'attname' in spectrum 'spec_num' of file 
!       'filename' to value 'value'

        use hdf5_wrapper         
        use common_arrays_rw
        use common_strings
        use common_nums

        implicit none
        
        integer :: file_num
        
        character(len=256) :: grname, path
        character(len=*), intent(in) :: filename, spec_num, attname,value

!       Delete attribute 'attname'
         grname='/Spectrum'//trim(spec_num)
         call hdf5_delete_attribute(filename,trim(grname),trim(attname))

!       Open file
         call hdf5_open_file(file_num,trim(filename))
          
!       Update attribute 'attname'
         path=trim(grname)//'/'//trim(attname)
         call hdf5_write_attribute(file_num,trim(path),trim(value))

!       Close file
         call hdf5_close_file(file_num)

        end subroutine spectrum_status_set
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine include_ifit_fitparams(file_num)

!       Includes in file 'file_num' the values of the fit parameters
!        read by inparam() from ~/ifitnot/ifit_parameters.<ion>, used to fit
!        the spectra contained in file 'file_num'

        use hdf5_wrapper         
        use common_strings
        use common_nums, only: fwhm_kms
        use ifit_par

        implicit none
         
        integer :: file_num

!       ------------------------------------------------------------------------
!       Write fit parameters to output file

         groupname='/Header/ifit_Fitparameters'
         
         call hdf5_create_group(file_num,trim(groupname))

         attributename=trim(groupname)//'/ParameterDescription'
         dummy = 'For the meaning of the fit parameters please consult '
     &         //'the file ~/ifitnot/ifit_parameters.'
         call hdf5_write_attribute(file_num,trim(attributename),trim(dummy))
         
         attributename=trim(groupname)//'/FSigma'
         call hdf5_write_attribute(file_num,trim(attributename),fsigma)

         attributename=trim(groupname)//'/Nion_Min_Absolut'
         call hdf5_write_attribute(file_num,trim(attributename),Nionmin)

         attributename=trim(groupname)//'/Nion_Max_NonSaturatedLine'
         call hdf5_write_attribute(file_num,trim(attributename),Nionmaxns)

         attributename=trim(groupname)//'/Nion_Max_SaturatedLine'
         call hdf5_write_attribute(file_num,trim(attributename),Nionmaxsat)

         attributename=trim(groupname)//'/b_value_Max'
         call hdf5_write_attribute(file_num,trim(attributename),bparmax)

         attributename=trim(groupname)//'/b_value_Min_Absolut'
         call hdf5_write_attribute(file_num,trim(attributename),bparmin)

         attributename=trim(groupname)//'/Profile'
         call hdf5_write_attribute(file_num,trim(attributename),profile_str)

         attributename=trim(groupname)//'/FWHM_KMpS'
         call hdf5_write_attribute(file_num,trim(attributename),fwhm_avp)

         attributename=trim(groupname)//'/N_Split_Min'
         call hdf5_write_attribute(file_num,trim(attributename),nsplitmin)

         attributename=trim(groupname)//'/N_Sigma_Detection_Threshold'
         call hdf5_write_attribute(file_num,trim(attributename),N_Sigma)

         attributename=trim(groupname)//'/Max_Iterations_Marquardt'
         call hdf5_write_attribute(file_num,trim(attributename),maxiter)

         attributename=trim(groupname)//'/Chi_Square_Bad'
         call hdf5_write_attribute(file_num,trim(attributename),chisqbad)

         attributename=trim(groupname)//'/Chi_Square_Good'
         call hdf5_write_attribute(file_num,trim(attributename),chisqgood)

         attributename=trim(groupname)//'/Chi_Square_Tol_Convergence'
         call hdf5_write_attribute(file_num,trim(attributename),chisqtol)

         attributename=trim(groupname)//'/Spectrum_Cycled'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         spectrum_cycled_str)
        
        end subroutine include_ifit_fitparams
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine compare_str(str1,str2,str3)

!       Compares two strings program if they are not equal

        use common_strings

        implicit none

        character(len=*) :: str1, str2, str3
        
        if (trim(str1) .ne. trim(str2)) then
         write(6,*) trim(errmsg)//'Incompatible '//trim(str3)//' values: '//
     1   trim(str1)//' vs. '//trim(str2)//' in HDF5 file '//trim(specwizfile)
         stop 1
        end if
        
        end subroutine compare_str
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine compare_num(val1,val2,str1)
        
!       Compares two double precision numerical values and stops program
!       if they are not equal

        use common_strings

        implicit none

        double precision :: val1, val2
        character(len=*) :: str1
        
        if (val1 .ne. val2) then
         write(6,*) trim(errmsg)//'Incompatible '//trim(str1)//' values: ',
     1   val1,' vs. ', val2, ' in HDF5 file '//trim(specwizfile)
         stop 1
        end if
        
        end subroutine compare_num
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine compare_num_int(val1,val2,str1)
        
!       Compares two integer numerical values program if they are not equal

        use common_strings

        implicit none

        integer :: val1, val2
        character(len=*) :: str1
        
        if (val1 .ne. val2) then
         write(6,*) trim(errmsg)//'Incompatible '//trim(str1)//' values: ',
     1   val1,' vs. ', val2, ' in HDF5 file '//trim(specwizfile)
         stop 1
        end if
        
        end subroutine compare_num_int
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine inparam(ionname)

!       Reads the values of the fit parameters specified in ifit_parameters
!       Included here to write ifit fit parameters into output file

        use ifit_par
        use common_strings

        implicit none

        character(len=*), intent(in) :: ionname
        character(len=256) :: parfile
        
        logical :: filexists

!       ifit input parameters
! 1.0d0   scale_noise -- factor to rescale noise level
! 1.0d10  Nion_min -- min column density (absolute)
! 1.0d20  Nion_maxns -- max column density for non-saturated line
! 1.0d20  Nion_maxsat -- max column density (absolute)
! 1.00d3  bparmax -- max b-value (absolute)
! 2.10d0  bparmin -- min b-value (absolut) should be larger than pixel size
! gauss   profile -- voigt | gauss
! 0.0d0   fwhm_kms -- width of Gaussian (instrumental) broadening in km/s
! 3.0d0   N_sigma -- detection threshold (detection_region)
! 500     maxiter -- maximum number of iterations for Levenberg-Marquardt minimization
! 0.683d0 conf_lo -- lower goodness-of-fit confidence level
! 0.954d0 conf_hi -- higher goodness-of-fit confidence level
! 1.00d1  z_qso -- emission redshift; used to constrain the wavelength range
! FALSE   spectrum_shifted_str -- cycles spectrum to avoid features at boundaries
! FALSE   visual_str -- to visualise fit on-the-fly
! FALSE   test_run -- set to TRUE only when running test_ifit.bash!
! FALSE   debug_str -- to output a lot of useful information during runtime
! FALSE   verbose_str -- to output useful information during runtime; overrides debug!
! FALSE   quite_str -- keep output during runtime to a minimum; overrides verbose and debug!


!       Define parameter file
        parfile=trim(homedir)//'/ifitnot/ifit_parameters.'//trim(ionname)
        
!       File exists?
        inquire(file=parfile,exist=filexists)

!       If file doesn't exist, stop cleanly
        if (.not.filexists) then
         write(6,*) trim(errmsg)//'File '//trim(parfile)//' does not exist'
         stop 1
        end if

        open(3,file=trim(parfile),status='old')
         read(3,*) !comment line
         read(3,*) fsigma
         read(3,*) Nionmin
         read(3,*) Nionmaxns
         read(3,*) Nionmaxsat
         read(3,*) bparmax
         read(3,*) bparmin
         read(3,*) profile_str
         read(3,*) fwhm_avp
         read(3,*) N_sigma
         read(3,*) maxiter
         read(3,*) chisqbad
         read(3,*) chisqgood
         read(3,*) z_qso_avp
         read(3,*) spectrum_shifted_str
!         read(3,*) visual_str
!         read(3,*) benchmark_str
!         read(3,*) debug_str
!         read(3,*) verbose_str
!         read(3,*) quite_str
        
        close(3)

        return

        end subroutine inparam
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine header_long_sanity_check(file_num)

!       Compare header values in file 'file_num' to check for consistency
!       with present values (to avoid mixing of fitfiles with different
!       values)

        use hdf5_wrapper         
        use common_strings
        use common_nums

        implicit none
         
        integer :: file_num

         call hdf5_read_attribute(file_num,'/Header/Simulation',dummy)
         comstr='Simulation'
         call compare_str(dummy,simulation,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/Specfile',dummy)
         comstr='Specfile'
         call compare_str(dummy,specwizfile,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/Redshift_Abs_Min',aux_value)
         comstr='Redshift_Abs_Min'
         call compare_num(aux_value,z_min,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/Redshift_Abs_Max',aux_value)
         comstr='Redshift_Abs_Max'
         call compare_num(aux_value,z_max,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/Redshift_QSO',aux_value)
         comstr='Redshift_QSO'
         call compare_num(aux_value,z_qso,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/Noise_Added',dummy)
         comstr='Noise_Added'
         call compare_str(dummy,noise_added,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/SignalToNoise',aux_value)
         comstr='SignalToNoise'
         call compare_num(aux_value,signaltonoise,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/PixSize_Angstrom',aux_value)
         comstr='PixSize_Angstrom'
         call compare_num(aux_value,pxsize_ang,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/FWHM_Kms',aux_value)
         comstr='FWHM_Kms'
         call compare_num(aux_value,fwhm_kms,comstr)
         
         call hdf5_read_attribute(file_num,'/Header/PixSizeKms_Before_Convolution',aux_value)
         comstr='PixSizekms_Before_convolution'
         call compare_num(aux_value,pxsize_noconv,comstr)
        
        end subroutine header_long_sanity_check
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine header_short_sanity_check(file_num)

!       Compare header values in file 'file_num' to check for consistency
!       with present values (to avoid mixing of fitfiles with different
!       values)

        use hdf5_wrapper         
        use common_strings
        use common_nums

        implicit none
         
        integer :: file_num
  
          call hdf5_read_attribute(file_num,'/Header/Simulation',dummy)
          comstr='Simulation'
          call compare_str(dummy,simulation,comstr)
          
          call hdf5_read_attribute(file_num,'/Header/Specfile',dummy)
          comstr='Specfile'
          call compare_str(dummy,specwizfile,comstr)
          
          call hdf5_read_attribute(file_num,'/Header/Redshift',aux_value)
          comstr='Redshift'
          call compare_num(aux_value,z_box,comstr)
          
          call hdf5_read_attribute(file_num,'/Header/Noise_Added',dummy)
          comstr='Noise_Added'
          call compare_str(dummy,noise_added,comstr)
          
          call hdf5_read_attribute(file_num,'/Header/SignalToNoise',aux_value)
          comstr='SignalToNoise'
          call compare_num(aux_value,signaltonoise,comstr)
        
        end subroutine header_short_sanity_check
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine fitparams_sanity_check(file_num)

!       Compare fitparameter values in file 'file_num' to check for
!       consistency with present used values (to avoid mixing of spectra
!       fitted with different values)

        use hdf5_wrapper         
        use common_strings
        use common_nums
        use ifit_par

        implicit none
         
        integer :: file_num
         
!       ------------------------------------------------------------------------
!       Reading ifit fit parameters (from ~/ifitnot/ifit_parameters.ION)
         
         call inparam(ion_name)
!       ------------------------------------------------------------------------

         groupname='/Header/ifit_Fitparameters'
         
         attributename=trim(groupname)//'/FSigma'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,fsigma,attributename)

         attributename=trim(groupname)//'/Nion_Min_Absolut'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,Nionmin,attributename)

         attributename=trim(groupname)//'/Nion_Max_NonSaturatedLine'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,Nionmaxns,attributename)

         attributename=trim(groupname)//'/Nion_Max_SaturatedLine'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,Nionmaxsat,attributename)

         attributename=trim(groupname)//'/b_value_Max'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,bparmax,attributename)

         attributename=trim(groupname)//'/b_value_Min_Absolut'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,bparmin,attributename)

         attributename=trim(groupname)//'/Profile'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value_str)
         call compare_str(aux_value_str,profile_str,attributename)
         
         attributename=trim(groupname)//'/FWHM_KMpS'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,fwhm_avp,attributename)

         attributename=trim(groupname)//'/N_Split_Min'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value_int)
         call compare_num_int(aux_value_int,nsplitmin,attributename)

         attributename=trim(groupname)//'/N_Sigma_Detection_Threshold'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,N_Sigma,attributename)

         attributename=trim(groupname)//'/Max_Iterations_Marquardt'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value_int)
         call compare_num_int(aux_value_int,maxiter,attributename)

         attributename=trim(groupname)//'/Chi_Square_Bad'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,chisqbad,attributename)

         attributename=trim(groupname)//'/Chi_Square_Good'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,chisqgood,attributename)

         attributename=trim(groupname)//'/Chi_Square_Tol_Convergence'
         call hdf5_read_attribute(file_num,trim(attributename),aux_value)
         call compare_num(aux_value,chisqtol,attributename)

        end subroutine fitparams_sanity_check
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine create_fitgroup(file_num,spec_num)

!       Creates a group 'Line_Parameters' for each spectrum in file
!        'file_num'
!        NEW: includes chi^2 value resulting from fit (by ifit, which
!        uses Levenberg-Marquardt), and a flag 'Check_Fit' which equals
!        'TRUE' in case chi^2 is larger than 'chisqgood' for ANY region
!        in the spectrum. The latter parameter is an input parameter
!        required by ifit (see subroutine include_ifit_fitparams)

        use hdf5_wrapper
        use arrays_fit
        use detected_regions
        use common_strings
        use common_nums
        use fit_quality

        implicit none
        
        integer :: file_num
        
        double precision :: log_int_coldens_ion

        double precision, allocatable :: coldens_ion(:)

        character(len=*) :: spec_num

         groupname='/Spectrum'//trim(spec_num)

!       Check_Fit = 'TRUE' in case reduced chi^2 value resulting from
!        fit is larger than than 'chisqgood' (see subroutine
!        include_ifit_fitparams)

         attributename=trim(groupname)//'/Check_Fit'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(check_fit_str))

!       Reduced (i.e. divided by degrees of freedom), chi^2 value
!        resulting from fit         

         attributename=trim(groupname)//'/Chi^2_Reduced'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         chi_sq_reduced)

!        Spectrum cyclically shifted?

         attributename=trim(groupname)//'/Spectrum_Shifted'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         trim(spectrum_shifted_str))

         attributename=trim(groupname)//'/Spectrum_Shift_Pixel'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         spectrum_shift)

!        Formal completeness limit (in terms of res-frame equivalent
!        width, and column density)
!        check that Nion_min > 0, since dlog10(0) leads to crash
!        without any warning or error message

         attributename=trim(groupname)//'/Detection_Limit_W_r_mA'
         call hdf5_write_attribute(file_num,trim(attributename),
     &         equiv_width_lim_mA)

         attributename=trim(groupname)//'/Detection_Limit_log_Nion'
         if (Nion_lim.gt.0.0d0)
     &         call hdf5_write_attribute(file_num,trim(attributename),
     &         dlog10(Nion_lim))

!        Sum up column density of all detected lines and store (this
!        value should be comparable to the integrated column density
!        LogTotalIonColumnDensity_Sim (see write_spec_hdf5)
         
         allocate(coldens_ion(total_lines))
         coldens_ion(:) = 10**log10_Nion(:)
         if (sum(coldens_ion).gt.0) log_int_coldens_ion =
     &         DLOG10(sum(coldens_ion))
         datasetname=trim(groupname)//'/LogTotalIonColumnDensity_Fit'
         call hdf5_write_data(file_id,trim(datasetname),log_int_coldens_ion)

!       Write total number of lines detected
         attributename=trim(groupname)//'/Number_Of_Lines'
         call hdf5_write_attribute(file_num,trim(attributename),total_lines)

!       Write fitted flux
         datasetname=trim(groupname)//'/Flux_Fit'
         call hdf5_write_data(file_num,trim(datasetname),flux_fit)

!       Create line parameters group (one per spectrum)

         groupname='/Spectrum'//trim(spec_num)//'/Line_Parameters'
         call hdf5_create_group(file_num,trim(groupname))
         
!       Write line parameters to output file (one group per spectrum)

         datasetname=trim(groupname)//'/Absorption_Redshift'
         call hdf5_write_data(file_num,trim(datasetname),z_abs)

         datasetname=trim(groupname)//'/Absorption_Wavelength_Ang'
         call hdf5_write_data(file_num,trim(datasetname),wl_line)

         datasetname=trim(groupname)//'/Absorption_Wavelength_Shifted_Ang'
         call hdf5_write_data(file_num,trim(datasetname),wl_line_shift)

         datasetname=trim(groupname)//'/log10_ColumnDensity'
         call hdf5_write_data(file_num,trim(datasetname),log10_Nion)

         datasetname=trim(groupname)//'/DopplerParameter_Kms'
         call hdf5_write_data(file_num,trim(datasetname),b_value)

         datasetname=trim(groupname)//'/EquivalentWidth_mA'
         call hdf5_write_data(file_num,trim(datasetname),equiv_width_mA)

         datasetname=trim(groupname)//'/Absorption_Velocity_Kms'
         call hdf5_write_data(file_num,trim(datasetname),v_line)

         datasetname=trim(groupname)//'/Absorption_Velocity_Shifted_Kms'
         call hdf5_write_data(file_num,trim(datasetname),v_line_shift)

         datasetname=trim(groupname)//'/Error_ColumnDensity'
         call hdf5_write_data(file_num,trim(datasetname),dNion)

         datasetname=trim(groupname)//'/Error_DopplerParameter_Kms'
         call hdf5_write_data(file_num,trim(datasetname),db_value)

         datasetname=trim(groupname)//'/Error_Absorption_Velocity_Kms'
         call hdf5_write_data(file_num,trim(datasetname),dv_line)

!       Create detected regions group (one per spectrum)

         groupname='/Spectrum'//trim(spec_num)//'/Detected_Regions'
         call hdf5_create_group(file_num,trim(groupname))

!       Write detected regions to output file (one group per spectrum)

!       Write total number of regions detected and their parameters
         attributename=trim(groupname)//'/Number_Of_Regions'
         call hdf5_write_attribute(file_num,trim(attributename),total_regions)

         datasetname=trim(groupname)//'/Lines_In_Region'
         call hdf5_write_data(file_num,trim(datasetname),lines_in_region)
        
         datasetname=trim(groupname)//'/Pixel_Min'
         call hdf5_write_data(file_num,trim(datasetname),pixmin)
        
         datasetname=trim(groupname)//'/Pixel_Max'
         call hdf5_write_data(file_num,trim(datasetname),pixmax)
        
         datasetname=trim(groupname)//'/Wavelength_Ang_Min'
         call hdf5_write_data(file_num,trim(datasetname),wlmin)
        
         datasetname=trim(groupname)//'/Wavelength_Ang_Max'
         call hdf5_write_data(file_num,trim(datasetname),wlmax)
        
         datasetname=trim(groupname)//'/Velocity_Kms_Min'
         call hdf5_write_data(file_num,trim(datasetname),velmin)
        
         datasetname=trim(groupname)//'/Velocity_Kms_Max'
         call hdf5_write_data(file_num,trim(datasetname),velmax)
        
         datasetname=trim(groupname)//'/Significance_Sigma_Units'
         call hdf5_write_data(file_num,trim(datasetname),significance)
        
        end subroutine create_fitgroup
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine dealloc_arrays_read
        
        use common_arrays_read
        

        implicit none

         if(ALLOCATED(wavelength)) DEALLOCATE(wavelength)
         if(ALLOCATED(flux)) DEALLOCATE(flux)
         if(ALLOCATED(flux_noisy)) DEALLOCATE(flux_noisy)
         if(ALLOCATED(opticaldepth)) DEALLOCATE(opticaldepth)
         if(ALLOCATED(noise)) DEALLOCATE(noise)
         if(ALLOCATED(sigma)) DEALLOCATE(sigma)
         if(ALLOCATED(gaussdev)) DEALLOCATE(gaussdev)
         if(ALLOCATED(v_Hubble)) DEALLOCATE(v_Hubble)

        end subroutine dealloc_arrays_read
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine dealloc_arrays_write

!       Destroys allocated arrays when no longer needed

        use arrays_fit

        implicit none

         if(ALLOCATED(flux_fit)) DEALLOCATE(flux_fit)
         if(ALLOCATED(z_abs)) DEALLOCATE(z_abs)
         if(ALLOCATED(log10_Nion)) DEALLOCATE(log10_Nion)
         if(ALLOCATED(dNion)) DEALLOCATE(dNion)
         if(ALLOCATED(b_value)) DEALLOCATE(b_value)
         if(ALLOCATED(db_value)) DEALLOCATE(db_value)
         if(ALLOCATED(v_line)) DEALLOCATE(v_line)
         if(ALLOCATED(v_line_shift)) DEALLOCATE(v_line_shift)
         if(ALLOCATED(dv_line)) DEALLOCATE(dv_line)
         if(ALLOCATED(wl_line)) DEALLOCATE(wl_line)
         if(ALLOCATED(wl_line_shift)) DEALLOCATE(wl_line_shift)
         if(ALLOCATED(equiv_width_mA)) DEALLOCATE(equiv_width_mA)
        
        end subroutine dealloc_arrays_write
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        function size_of_file(filename)

!       Returns the number of lines in file 'filename'

        use common_strings

        implicit none

        integer :: size_of_file, lines
        character(len=256) :: filename

!       ------------------------------------------------------------------------
!       Open file to get dimension
         
         size_of_file = -1
         lines = 0
         open(10,file=trim(filename), status='old')
          do
           read(10, *, end=10)
           lines = lines + 1
          end do
  10         close(10)

         size_of_file = lines
         
         if (size_of_file .le. 0) then
          write(6,*) trim(warnmsg)//'Empty file: ', trim(filename)
         end if

        return
        
        end function size_of_file
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        subroutine hdf5_delete_attribute(filename,grname,attname)

!       The following is a simple procedure written in more "basic" HDF5 to 
!       delete and create an attribute contained in a group
!       The attribute string attname should NOT start with '/'
        
        use hdf5_wrapper
        use common_strings

        implicit none
        
        integer :: error, group_id
        integer :: file_num

        character(len=*) :: filename, grname, attname
         
!       Open fortran interface
         call h5open_f(error)
!        print*, error

!       Open file
         call h5fopen_f(trim(filename),H5F_ACC_RDWR_F,file_num,error)
!        print*, error,file_num

!       Open group
         call h5gopen_f(file_num, trim(grname), group_id, error)
!        print*, error, group_id

!       Delete attribute
         call h5adelete_f(group_id, trim(attname), error)
!        print*, error
        
!       Close fortran interface
         call h5close_f(error)
!        print*, error
        
        end subroutine hdf5_delete_attribute
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!        Stolen from SpecWizard

        subroutine convolve_gaussian()

        use common_nums, only: nveloc, fwhm_kms
        use common_arrays_read
        use common_strings

        implicit none
        ! local variables
        double precision :: sigmakms, vpixsize, b, norm
        integer :: i,j,off, nvpix
        double precision, allocatable, save :: gauss(:)
        double precision, allocatable :: convl_flux(:),
     &        convl_flux_convolved(:)

        write(6,'(a,f6.2)') trim(statmsg)//'Convolving spectrum with '
     &        //'Gaussian PSF using fwhm = ', fwhm_kms

        vpixsize = v_Hubble(2)-v_Hubble(1) !in km/s
        nvpix         = nveloc

!        Compute sigma in km/s
        sigmakms = fwhm_kms / 2.35482

!        Compute sigma in units of pixels
        b = sigmakms / vpixsize
        
!        For convolution with instrumental PSF we need to Fourier 
!        transform, we thus need to increase the array so that it is a
!        power of 2.
        nvpix        = int(2**(aint(log(dble(nvpix))/log(2.)) + 1))
        
!        Create normalized Gaussian in wrap-around order (required by
!        convlv)

        allocate(gauss(nvpix))
        norm = 1d0 / (2d0 * b * b)
        do i = 0, nvpix-1
           if (i .le. nvpix-1) then 
              if (i .le. nvpix/2) then
                 j = i
              else
                 j = i - nvpix
              endif
              if (abs(j) .lt. 10.*b) then
                 gauss(i+1) = exp(-(dble(j)**2)*norm)
              else
                 gauss(i+1) = 0d0
              endif
           else
              gauss(i+1) = 0d0
           endif
        enddo
!        Make sure Gaussian is normalized.
        gauss  = gauss / sum(gauss)

        allocate(convl_flux(nvpix),convl_flux_convolved(2*nvpix))
        convl_flux(:) = 0.0
        convl_flux(1:nveloc) = flux(1:nveloc)

!        Now copy periodic copies of the flux signal into the zero buffer
!        to avoid aliasing effects
        do i=nveloc+1,nvpix
           off = i-nveloc
           if (off .lt. (nvpix-nveloc)/2.) then
              convl_flux(i) = convl_flux(i-nveloc)
           else
              convl_flux(i) = convl_flux(i-(nvpix-nveloc))
           endif
        enddo
        convl_flux_convolved(:) = 0.0

        call convlv(convl_flux,nvpix,gauss,nvpix,1,convl_flux_convolved)

        flux(1:nveloc) = convl_flux_convolved(1:nveloc)

        deallocate(convl_flux,convl_flux_convolved)

        end subroutine convolve_gaussian
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!        Stolen from SpecWizard

        SUBROUTINE convlv(data,n,respns,m,isign,ans)

        use fourier
        
        implicit none 
        integer, intent(in) :: n, m, isign
        double precision, intent(in)    :: data(n)
        double precision, intent(inout) :: respns(n)
        double complex, intent(out):: ans(2*n)
        ! local variables
        INTEGER i,no2
        integer, save :: nfft=-1

        if(nfft .ne. n)then
           allocate(fft(n))
           nfft = n
        endif

        do  i=1,(m-1)/2
           respns(n+1-i)=respns(m+1-i)
        enddo
        do  i=(m+3)/2,n-(m-1)/2
           respns(i)=0.0
        enddo

        call twofft(data,respns,fft,ans,n)
        no2=n/2
        do i=1,no2+1
           if (isign.eq.1) then
              ans(i)=fft(i)*ans(i)/no2
           else if (isign.eq.-1) then
              if (abs(ans(i)).eq.0.0) then
                write(*,*) 'deconvolving at response zero in convlv'
                stop
              end if
              ans(i)=fft(i)/ans(i)/no2
           else
              stop 'no meaning for isign in convlv'
           endif
        enddo
        ans(1)=dcmplx(dble(ans(1)),dble(ans(no2+1)))
        call realft(ans,n,-1)

        return
        end subroutine convlv

!       ------------------------------------------------------------------------
!        Stolen from SpecWizard

        SUBROUTINE twofft(data1,data2,fft1,fft2,n)

        implicit none 
        integer, intent(in) :: n
        double precision, intent(in)    :: data1(n), data2(n)
        double complex                  :: fft1(n), fft2(n)

        ! local variables
        INTEGER j,n2
        double complex ::  h1,h2,c1,c2
        !
        c1=dcmplx(0.5,0.0)
        c2=dcmplx(0.0,-0.5)
        do  j=1,n
           fft1(j)=dcmplx(data1(j),data2(j))
        enddo
        call four1(fft1,n,1)
        fft2(1)=dcmplx(imag(fft1(1)),0d0)
        fft1(1)=dcmplx(dble(fft1(1)),0d0)
        n2=n+2
        do j=2,n/2+1
           h1=c1*(fft1(j)+conjg(fft1(n2-j)))
           h2=c2*(fft1(j)-conjg(fft1(n2-j)))
           fft1(j)=h1
           fft1(n2-j)=conjg(h1)
           fft2(j)=h2
           fft2(n2-j)=conjg(h2)
        enddo
        return
        end subroutine twofft

!       ------------------------------------------------------------------------
!        Stolen from SpecWizard

        SUBROUTINE realft(data,n,isign)

        implicit none
        integer, intent(in) :: n, isign
        double precision, intent(inout) :: data(n)
        ! local variables
        INTEGER i,i1,i2,i3,i4,n2p3
        double precision c1,c2,h1i,h1r,h2i,h2r,wis,wrs
        double precision theta,wi,wpi,wpr,wr,wtemp
        theta=3.141592653589793d0/dble(n/2)
        c1=0.5
        if (isign.eq.1) then
           c2=-0.5
           call four1(data,n/2,+1)
        else
           c2=0.5
           theta=-theta
        endif
        wpr=-2.0d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.0d0+wpr
        wi=wpi
        n2p3=n+3
        do  i=2,n/4
           i1=2*i-1
           i2=i1+1
           i3=n2p3-i2
           i4=i3+1
           wrs=sngl(wr)
           wis=sngl(wi)
           h1r=c1*(data(i1)+data(i3))
           h1i=c1*(data(i2)-data(i4))
           h2r=-c2*(data(i2)+data(i4))
           h2i=c2*(data(i1)-data(i3))
           data(i1)=h1r+wrs*h2r-wis*h2i
           data(i2)=h1i+wrs*h2i+wis*h2r
           data(i3)=h1r-wrs*h2r+wis*h2i
           data(i4)=-h1i+wrs*h2i+wis*h2r
           wtemp=wr
           wr=wr*wpr-wi*wpi+wr
           wi=wi*wpr+wtemp*wpi+wi
        enddo
        if (isign.eq.1) then
           h1r=data(1)
           data(1)=h1r+data(2)
           data(2)=h1r-data(2)
        else
           h1r=data(1)
           data(1)=c1*(h1r+data(2))
           data(2)=c1*(h1r-data(2))
           call four1(data,n/2,-1)
        endif
        return
        end subroutine realft

!       ------------------------------------------------------------------------
!        Stolen from SpecWizard

        SUBROUTINE four1(data,nn,isign)

        implicit none
        integer, intent(in) :: nn, isign
        double precision, intent(inout) :: data(2*nn)
        ! local variables  
        INTEGER i,istep,j,m,mmax,n
        double precision :: tempi,tempr
        double precision ::  theta,wi,wpi,wpr,wr,wtemp

        n=2*nn
        j=1
        do  i=1,n,2
           if(j.gt.i)then
              tempr=data(j)
              tempi=data(j+1)
              data(j)=data(i)
              data(j+1)=data(i+1)
              data(i)=tempr
              data(i+1)=tempi
           endif
           m=n/2
1         if ((m.ge.2).and.(j.gt.m)) then
              j=j-m
              m=m/2
              goto 1
         endif
         j=j+m
        enddo
        mmax=2
2        if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do m=1,mmax,2
           do  i=m,n,istep
              j=i+mmax
              tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
              tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
              data(j)=data(i)-tempr
              data(j+1)=data(i+1)-tempi
              data(i)=data(i)+tempr
              data(i+1)=data(i+1)+tempi
           enddo
           wtemp=wr
           wr=wr*wpr-wi*wpi+wr
           wi=wi*wpr+wtemp*wpi+wi
        enddo
        mmax=istep
        goto 2
        endif
        return
        end subroutine four1

!       ------------------------------------------------------------------------
!       Numerical Recipes: Functions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       RANDOM-Numbers (uniformily distributed 1)
!       ------------------------------------------------------------------------
!       GENERATES UNIfORMILY DISTRIBUTED RANDOM NUMBERS
              
        function ran1(idum)
              integer idum,IA,IM,IQ,IR,NTAB,NDIV
              double precision ran1,AM,EPS,RNMX
              parameter (IA=16807,IM=2147483647,AM=1.0D0/IM,IQ=127773,IR=2836,
     !       NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2D-7,RNMX=1.0D0-EPS)
              integer j,k,iv(NTAB),iy
              save iv,iy
              data iv /NTAB*0/, iy /0/
              if (idum.le.0.or.iy.eq.0) then
         idum=max(-idum,1)
         do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11         continue
         iy=iv(1)
              end if
              k=idum/IQ
              idum=IA*(idum-k*IQ)-IR*k
              if (idum.lt.0) idum=idum+IM
              j=1+iy/NDIV
              iy=iv(j)
              iv(j)=idum
              ran1=min(AM*iy,RNMX)
              return
              end function
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.


!       ------------------------------------------------------------------------
!       RANDOM-Numbers (normal distributed)
!       ------------------------------------------------------------------------
!       GENERATES GAUSSIAN DISTRIBUTED RANDOM NUMBERS

        function gasdev(idum)
        integer idum
        double precision gasdev
CU      USES ran1
        integer iset
        double precision fac,gset,rsq,v1,v2,ran1
        SAVE iset,gset
        DATA iset/0/
        if (iset.eq.0) then
1         v1=2.0D0*ran1(idum)-1.0D0
         v2=2.0D0*ran1(idum)-1.0D0
         rsq=v1**2+v2**2
         if(rsq.ge.1.0D0.or.rsq.eq.0.0D0) goto 1
         fac=sqrt((-2.0D0)*log(rsq)/rsq)
         gset=v1*fac
         gasdev=v2*fac
         iset=1
        else
         gasdev=gset
         iset=0
        end if
        return
        end function
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.

!       ------------------------------------------------------------------------
!       Ordering an array (integer) in ascending order
!       ------------------------------------------------------------------------

              SUBROUTINE sort(n,arr)
              INTEGER n,M,NSTACK
              integer arr(n)
              PARAMETER (M=7,NSTACK=50)
              INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
              integer a,temp
              jstack=0
              l=1
              ir=n
1             if(ir-l.lt.M)then
         do 12 j=l+1,ir
          a=arr(j)
          do 11 i=j-1,1,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
11        continue
          i=0
2         arr(i+1)=a
12       continue
         if(jstack.eq.0)return
         ir=istack(jstack)
         l=istack(jstack-1)
         jstack=jstack-2
              else
         k=(l+ir)/2
         temp=arr(k)
         arr(k)=arr(l+1)
         arr(l+1)=temp
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(l))then
          temp=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
        endif
        i=l+1
        j=ir
        a=arr(l)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        jstack=jstack+2
        if(jstack.gt.NSTACK)print*, 'NSTACK too small in sort'
         if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
         else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
         endif
              endif
              goto 1
              END
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
