        program profiles_hdf5_loop

!       ------------------------------------------------------------------------
!       Serves as an "interface" between auto_fit, profiles_hdf5 and
!       write_fit_hdf5 
!       Formerly a BASH script, now written in f90, to overcome some
!       scripting disadvantages like existense of file checking, etc.
!       Reads in the output of auto_min_hdf5 and inputs it to profiles.f;
!       the output of the latter is the passed on to write_fit_hdf5
!
!       Calls profiles_hdf5, write_fit_hdf5
!       USES hdf5_wrapper module in:
!
!       /home/volans/tepper/hdf5/HDF5_Wrapper/lib/ 
!
!       Makes a single call to HDF5 functions to check the content of
!       file <spectype>_z<redshift>_<ion>_fit_SN<S/N>.hdf5. More speci-
!       fically, it checks the fit status (true/false) of the handled
!       spectrum prior to removing fit files. See write_fit_hdf5 and
!       the last 'subroutine' of this code for more info.
!
        use hdf5_wrapper

!       ------------------------------------------------------------------------
!       Variables declaration
!       ------------------------------------------------------------------------
        implicit none
        
        intrinsic DEXP
        
        integer :: i
        integer :: startid_int, endid_int
        integer :: argcount
        integer :: file_id

        character(len=256) :: simulation, spectype, redshift,
     &  ion, start_id, end_id
        character(len=256) :: ion_name, spec_id, specfile, linesfile,
     &  regionsfile, fitfile
        character(len=256) :: path, homedir, datadir, prefix
        character(len=256) :: errmsg, warnmsg, statmsg, command
        character(len=256) :: signaltonoise_str
        character(len=1) :: addnoise_str
        character(len=5) :: profile_str
        character(len=256) :: groupname, attributename, fstatus, outfile

        logical file_exists, specfile_exists, linesfile_exists,
     &        regionsfile_exists, spectrum_fitted, filehdf5_exists
!       ------------------------------------------------------------------------
!       Define program error, warning, and status messages
!        errmsg='ERROR: profiles_hdf5_loop ... '
!        warnmsg='WARNING: profiles_hdf5_loop ... '
!        statmsg='STATUS: profiles_hdf5_loop ... '

!        The lines below are meant to print the different
!        messages in different colors

!        In red=31
        errmsg=CHAR(27)//'[1;31mERROR: profiles_hdf5_loop ...'//
     &        CHAR(27)//'[0m'
!        In yellow=33
        warnmsg=CHAR(27)//'[1;33mWARNING: profiles_hdf5_loop ...'//
     &        CHAR(27)//'[0m'
!        In green=32
        statmsg=CHAR(27)//'[1;32mSTATUS: profiles_hdf5_loop ...'//
     &        CHAR(27)//'[0m'

!       ------------------------------------------------------------------------
!       Getting arguments

!       Call intrinsic IArgC()
        
        argcount = IArgC()

              if (argcount.ne.7) then
         write(6,*) trim(warnmsg)//'USAGE: profiles_hdf5_loop <simulation>'
     &         //' <short|long> <redshift> <start> <end> <ion> '
     &         //'<S/N (or "inf" for no noise)>'
         stop 1
              end if

        call GetArg(1,simulation)

        call GetArg(2,spectype)

        call GetArg(3,redshift)

        call GetArg(4,start_id)
        read(start_id,'(i5)') startid_int

        call GetArg(5,end_id)
        read(end_id,'(i5)') endid_int
        
        call GetArg(6,ion)

        call GetArg(7,signaltonoise_str)
        if (trim(signaltonoise_str).eq.'inf') then
         addnoise_str = 'F'
        else
         addnoise_str = 'T'        
        end if
!       ------------------------------------------------------------------------
!       Computation
!       ------------------------------------------------------------------------
!       Define home directory and generic data directory
        
        homedir = '/home/volans/tepper/'
        datadir = '/output_data/'
!       ------------------------------------------------------------------------
!       Check spectral type
        if ((trim(spectype).ne.'short').and.(trim(spectype).ne.'long')) then
         write(6,*) trim(errmsg)//'Not a valid spectral type: '//
     &         trim(spectype)
         stop 1
        end if

!       ------------------------------------------------------------------------
!       Converting OWLS ion names into general format (see ~/ifitnot/ions.dat)
!       Only H, O, N and Ne for now
!        Distinction between h1 and h1_bla because ifit fitting parameters
!        are set differently

        select case(ion)
        
        case('h1')
         ion_name = 'HI_Lya'

        case('h1_bla')
         ion_name = 'HI_Lya_BLA'

        case('o6')
         ion_name = 'OVI_1032'

        case('n5')
         ion_name = 'NV_1238'

        case('ne8')
         ion_name = 'NeVIII_770'

        case('c4')
         ion_name = 'CIV_1548'

        case('si3')
         ion_name = 'SiIII_1206'

        case default
         write(6,*) trim(errmsg)//'Ion ', trim(ion),
     &         ' not yet implemented.'
         stop 1

        end select

!       ------------------------------------------------------------------------
!       Start loop to read SpecWizard spectrum in file
!       <spectype>_z<redshift>_<ion>_fit_SN<S/N>.hdf5
!       created by read_spec_hdf5, and corresponding fit files
!       <spec_type>_spec<spec_id>_z<redshift>_<ion>_SN<signatltonoise>.*
!       created by auto_min_hdf5

!       Define path common to all files 

        path=trim(homedir)//'/ifitnot/'//trim(datadir)//trim(simulation)

        do i=startid_int, endid_int
         
         write(6,'(a)') trim(statmsg)
         write(6,'(a)') ' -----------------------------------------------------'
     &         //'--------------------------'
         write(6,'(a,i4,a)') 'Processing sightline: ',i, ' z='//trim(redshift)//
     &         ' '//trim(simulation)//' S/N='//trim(signaltonoise_str)
         write(6,'(a)') ' -----------------------------------------------------'
     &         //'--------------------------'
!       Initialize flag for file existence
        
         file_exists = .true.

!       Convert spectrum ID (integer) to string
          write(spec_id,'(i5)') i

!       Define prefix common to all fit files

        prefix=trim(spectype)//'_spec'//trim(adjustl(spec_id))//'_z'//
     &        trim(redshift)//'_'//trim(ion)//'_SN'//trim(signaltonoise_str)

!       Define line parameters file and copy into <prefix>.fit to be
!       processed by profiles_hdf5
          linesfile=trim(path)//'/'//trim(prefix)//'.fit'

!       File exists?
         inquire(file=linesfile,exist=linesfile_exists)

!       If file exists, copy it, using an UNIQUE name
         if (linesfile_exists) then
          command='cp '//trim(linesfile)//' '//trim(prefix)//'.fit'
          call system(trim(command))
         else
          file_exists = .false.
          write(6,'(a)') trim(warnmsg)//'File '//trim(linesfile)//
     &          ' does not exist'
         end if

!       Define spectrum file and copy into <prefix>.dat to be
!       processed by profiles_hdf5
          specfile=trim(path)//'/'//trim(prefix)//'.dat'

!       File exists?
         inquire(file=specfile,exist=specfile_exists)

!       If file exists, copy it
         if (specfile_exists) then
          command='cp '//trim(specfile)//' '//trim(prefix)//'.dat'
          call system(trim(command))
         else
          file_exists = .false.
          write(6,'(a)') trim(warnmsg)//'File '//trim(specfile)//
     &          ' does not exist'
         end if

!       Define file with detected regions and copy into <prefix>.int to be
!       processed by profiles_hdf5
          regionsfile=trim(path)//'/'//trim(prefix)//'.int'

!       File exists?
         inquire(file=regionsfile,exist=regionsfile_exists)

!       If file exists, copy it
         if (regionsfile_exists) then
          command='cp '//trim(regionsfile)//' '//trim(prefix)//'.int'
          call system(trim(command))
         else
          file_exists = .false.
          write(6,'(a)') trim(warnmsg)//'File '//trim(regionsfile)//
     &          ' does not exist'
         end if

!       ------------------------------------------------------------------------
!       If either of the above files does not exist, skip and continue with next
!        spectrum 

         if (file_exists) then
          
!       open file <spectype>_z<redshift>_<ion>_fit_SN<S/N>.hdf5 
!       generated by read_spec_hdf5 to read profile function (Gauss or Voigt)
!        used to fit;
!        file should exist but it doesn't hurt to check
        
!         Define spectrum filename compatible with write_fit_hdf5
          specfile=trim(spectype)//'_z'//trim(redshift)//'_'//trim(ion)
     &          //'_fit_SN'//trim(signaltonoise_str)//'.hdf5'

          outfile = trim(path)//'/'//trim(specfile)
         
          inquire(file=outfile,exist=filehdf5_exists)
         
!       If file exists, open it and read fit profile flag; stop cleanly
!       otherwise

          if (.not.filehdf5_exists) then
         
           write(6,'(a)') trim(errmsg)//'File '//trim(specfile)//
     &           ' does not exist.'
           stop 1

          else
        
           call hdf5_open_file(file_id, trim(outfile), readonly=.true.)

!       Read fit profile flag
           groupname='/Header/ifit_Fitparameters'
           attributename=trim(groupname)//'/Profile'
           call hdf5_read_attribute(file_id,trim(attributename),profile_str)

           call hdf5_close_file(file_id)
          
          end if

!       Call profiles_hdf5 to generate synthetic spectrum and list of
!       fit parameter

          command='./profiles_hdf5 '//trim(ion_name)//' '//trim(prefix)
          
          write(6,'(a)') trim(statmsg)//'invoking profiles_hdf5...'
          
          call system(trim(command))

!       Call write_fit_hdf5 and write fitted flux and line parameters
!       into existent HDF5 spectrum file
!       <spectype>_z<redshift>_<ion>_fit_SN<S/N>.hdf5
!       created by read_spec_hdf5

!         Redefine spectrum filename compatible with write_fit_hdf5
          specfile=trim(spectype)//'_z'//trim(redshift)//'_'//trim(ion)
     &          //'_fit_SN'//trim(signaltonoise_str)//'.hdf5'

          fitfile= trim(prefix)//'_SynSpec.dat'
           
          linesfile=trim(prefix)//'_linepar.dat'
           
          regionsfile=trim(prefix)//'.int'

!         Call write_fit_hdf5
          command='write_fit_hdf5 '//trim(simulation)//' '//trim(spectype)
     &          //' '//trim(specfile)//' '//trim(spec_id)//' '//trim(fitfile)//' '//
     &          trim(linesfile)//' '//trim(regionsfile)//' '//trim(ion)

          write(6,'(a)') trim(statmsg)//'invoking write_fit_hdf5...'
          call system(trim(command))

!       ------------------------------------------------------------------------
!       End: remove unnecessary files
!       ------------------------------------------------------------------------
!       Removing fit files in current directory

          command='rm '//trim(prefix)//'_SynSpec.dat'
          call system(trim(command))
          command='rm '//trim(prefix)//'_linepar.dat'
          call system(trim(command))
          command='rm '//trim(prefix)//'.fit'
          call system(trim(command))
          command='rm '//trim(prefix)//'.dat'
          call system(trim(command))
          command='rm '//trim(prefix)//'.int'
          call system(trim(command))

!       Open and check file <spectype>_z<redshift>_<ion>_fit_SN<S/N>.hdf5
!       File MUST exists already (generated by read_spec_hdf5 and
!       updated by write_fit_hdf5)
!       but doesn't hurt to check

          outfile = trim(path)//'/'//trim(specfile)

          inquire(file=outfile,exist=filehdf5_exists)

          if (.not.filehdf5_exists) then

           write(6,'(a)') trim(errmsg)//'File '//trim(specfile)//
     &           ' does not exist.'
           stop 1

          else

!       If file exists, open it and perform check of spectrum
!       status; if spectrum already fitted, go on; stop cleanly
!       otherwise
        
           call hdf5_open_file(file_id, trim(outfile), readonly=.true.)

!       Read fitting status
           attributename='/Spectrum'//trim(adjustl(spec_id))//'/Spectrum_Fitted'
           call hdf5_read_attribute(file_id,trim(attributename),fstatus)

           call hdf5_close_file(file_id)

           if (trim(fstatus).eq.'TRUE') then
            spectrum_fitted = .true.
           else
            spectrum_fitted = .false.
           end if

!       If spectrum succesfully fitted, remove fit files in directory
!       ~/ifitnot/output_data/<simulation>; alternatively, we could keep'em
!        to check what could have gone wrong
           
           if (spectrum_fitted) then

            command='rm '//trim(path)//'/'//trim(prefix)//'.dat'
            call system(trim(command))
            command='rm '//trim(path)//'/'//trim(prefix)//'.fit'
            call system(trim(command))
            command='rm '//trim(path)//'/'//trim(prefix)//'.int'
            call system(trim(command))
           
           end if

          end if !for HDF5 file existence
!       End of removing 'subroutine'
!       ------------------------------------------------------------------------

         end if        !if neither fit file exists (see above)

!       ------------------------------------------------------------------------
        end do        !loop over range of spectrum IDs

!       ------------------------------------------------------------------------
        end program
!       ------------------------------------------------------------------------
