        
        program write_fit_hdf5

!        (C) Thorsten Tepper Garcia 2009
!        Last modified: 8.07.2009
!
!       USES hdf5_wrapper module in:
!
!       /home/volans/tepper/hdf5/HDF5_Wrapper/lib/ 
!
!
!       Reads in a short/long spectrum fitted by ifit and processed by
!       profiles.f, as well as the corresponding list of line parameters.
!       The files are called, respectively,
!       <ion>_SynSpectrum.dat and <ion>_LineParam.dat.
!       Also reads in a file <ion>.ewr containing information regarding
!       the spectral region with detected lines and their significance 
!       (see ifit for more information).
!
!       The following:
!
!       -ion info (wavelength, oscillator strength)
!       -fit (flux vs. wavelength vs. v_Hubble) + ifit_parameters info
!       -list of line parameters (z_abs, Nion, b-value) + errors
!
!       is appended to an EXISTING fitfile_hdf5
!       <spectype>_z<redshift>_<ion>_fit_SN<S/N>.hdf5 contained in
!       ./output_data/<simulation>

!       Modules

        use hdf5_wrapper
        use common_strings
        use common_nums
        use common_arrays_rw
        use arrays_fit
        use detected_regions
        use ifit_par
        use fit_quality

!       ------------------------------------------------------------------------
!       Variables declaration
!       ------------------------------------------------------------------------
        implicit none
        
        intrinsic DEXP
        
        integer :: i, j, iAs, dummy_int
        integer :: size_of_file
        integer, dimension(1) :: dim

        double precision :: void
        
        character(len=256) :: dir, atomfile

        logical :: flag, file_exists
!       ------------------------------------------------------------------------
!       Define program error, warning, and status messages
!        errmsg='ERROR: write_fit_hdf5 ... '
!        warnmsg='WARNING: write_fit_hdf5 ... '
!        statmsg='STATUS: write_fit_hdf5 ... '

!        The lines below are meant to print the different
!        messages in different colors

!        In red=31
        errmsg=CHAR(27)//'[1;31mERROR: write_fit_hdf5 ...'//
     &        CHAR(27)//'[0m'
!        In yellow=33
        warnmsg=CHAR(27)//'[1;33mWARNING: write_fit_hdf5 ...'//
     &        CHAR(27)//'[0m'
!        In green=32
        statmsg=CHAR(27)//'[1;32mSTATUS: write_fit_hdf5 ...'//
     &        CHAR(27)//'[0m'

!       ------------------------------------------------------------------------
!       Getting arguments
        
        call read_input_params_w

!       ------------------------------------------------------------------------
!       Computation
!       ------------------------------------------------------------------------
!       Define home directory
        
        homedir = '/home/volans/tepper/'

!       ------------------------------------------------------------------------
!       ------------------------------------------------------------------------
!       Short and long spectra
!       ------------------------------------------------------------------------
!       ------------------------------------------------------------------------
        if ((trim(spectype).eq.'short').or.(trim(spectype).eq.'long')) then
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
         if ((trim(spectype).eq.'short').and.(trim(ion).eq.'all')) then
          write(6,*) trim(errmsg)//'Need to specify an ion for short spectra.'
         end if
         stop 1

        end select
        
!       ------------------------------------------------------------------------
!       Reading Atomic data (Morton+2003)

!       Define atom-data file
        
        atomfile=trim(homedir)//'/ifitnot/ions.dat'
        
!       File exists?
         inquire(file=atomfile,exist=file_exists)

!       If file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,*) trim(errmsg)//'File '//trim(atomfile)//' does not exist'
          stop 1
         end if

!       Reads in restframe wavelength (in Angstroem), oscillator strength
!       and gamma-value (in Hz) of the corresponding transitions
         flag=.true.
         open(20, FILE = trim(atomfile), status='old')
          do while (flag)
           read(20,*,end=5) ion_str, wl_central, f_osc
           if (trim(ion_str) .eq. trim(ion_name)) then
            flag=.false.
           end if
          end do
  5          continue
         close(20)
         if (flag) then
          print*, trim(errmsg)//'Ion ', trim(ion_name), ' not found.'
          stop 1
         end if

!       ------------------------------------------------------------------------
!       Open ascii fit file (written by profiles_hdf5) to get dimension of
!       flux-, etc.-arrays (check with dimension of arrays in already
!       existing HDF5 spectrum file <spectype>_z<redshift>_<ion>_fit_SN<S/N>.hdf5)
         
         infile = trim(fitfile)
        
!       File exists?
         inquire(file=infile,exist=file_exists)

!       If file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,*) trim(errmsg)//'File '//trim(infile)//' does not exist'
          stop 1
         end if

         dim(1) = size_of_file(infile)

!       ------------------------------------------------------------------------
!       Allocating memory for flux_fit array

         allocate (flux_fit(dim(1)), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

!       ------------------------------------------------------------------------
!       Reading model flux from fit file <ion>_SynSpec.dat

         open(10,file=trim(fitfile), status='old')

          do i=1,dim(1)
           read(10,*) void, flux_fit(i)
          end do

         close(10)

!       ------------------------------------------------------------------------
!       Open ascii file with line parameters <ion>_linepar.dat (written by
!        profiles_hdf5) to read total number of absorption lines
         
         infile = trim(linesfile)

!       File exists?
         inquire(file=infile,exist=file_exists)

!       If file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,*) trim(errmsg)//'File '//trim(infile)//' does not exist'
          stop 1
         end if

!        Initialise number of lines
        total_lines = 0

!        Check that file is not empty, and read number of identified lines
        if(size_of_file(linesfile).gt.0) then
        
!        Read number of lines
          open(10,file=trim(linesfile), status='old')
           read(10,*)  ! skip info lines
           read(10,*)
           read(10,*)
           read(10,*)
           read(10,*)
           read(10,*)
           read(10,*)  total_lines
          close(10)

         end if

!       Write flag if no lines were detected
         if (total_lines .ge. 1) then
          lines_detected = .TRUE.
          lines_detected_str = 'TRUE'
         else
          lines_detected = .FALSE.
          lines_detected_str = 'FALSE'
         end if

!       ------------------------------------------------------------------------
        IF (total_lines.ge.1) THEN
!       ------------------------------------------------------------------------
!        Avoid doing unnecessary stuff if no lines have been detected

!       ------------------------------------------------------------------------
!       Allocating memory for all needed arrays

         allocate (z_abs(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (log10_Nion(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (dNion(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (b_value(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (db_value(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (v_line(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (v_line_shift(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (dv_line(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif
         
         allocate (wl_line(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (wl_line_shift(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (equiv_width_mA(total_lines), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif
!       ------------------------------------------------------------------------
!       Reading line parameters from file <ion>_linepar.dat

         open(10,file=trim(linesfile), status='old')

           read(10,*)  ! skip info lines
           read(10,*)
           read(10,*)
           read(10,*)
           read(10,*)
           read(10,*)
           read(10,*)  total_lines

!        Read reduced chi^2 value and fit flag;
!        if flag = TRUE, this indicates a bad fit, e.g. a high chi^2 value;
!        i.e. larger than chisqgood (set in ifit_parameters)
!        See subroutine include_ifit_fitparams which creates group
!        /Header/ifit_FitParameters
          read(10,*)  check_fit_str, chi_sq_reduced

!        Read if spectrunm has been cyclically shifted (to the left),
          read(10,*)  spectrum_shifted_str, spectrum_shift

!        Read formal completeness limit
          read(10,*)  equiv_width_lim_mA, Nion_lim

!        Define check fit flag (false by default)
          if (check_fit_str.eq.'TRUE') then
           check_fit = .true.
          else if (check_fit_str.ne.'FALSE') then
           write(6,*) trim(errmsg)//'Non valid check fit flag '//
     &           trim(check_fit_str)//' in file '//trim(linesfile)
                close(10)
           stop 1

          end if

!        Define spectrum cycling flag (false by default)
          if (spectrum_shifted_str.eq.'TRUE') then
           spectrum_shifted = .true.
          else if (spectrum_shifted_str.ne.'FALSE') then
           write(6,*) trim(errmsg)//'Non valid spectrum cycling flag '//
     &           trim(spectrum_shifted_str)//' in file '//trim(linesfile)
                close(10)
           stop 1
          end if

          do i=1,total_lines
           read(10,*) z_abs(i), wl_line(i), log10_Nion(i), b_value(i),
     &           equiv_width_mA(i),v_line(i), dNion(i), db_value(i),
     &           dv_line(i), wl_line_shift(i), v_line_shift(i)
          end do

         close(10)

!       ------------------------------------------------------------------------
!       Read number of detected regions from file <ion>.ewr (written by
!       ifit)

         infile = trim(ewrfile)

!       File exists?
         inquire(file=infile,exist=file_exists)

!       If file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,*) trim(errmsg)//'File '//trim(infile)//' does not exist'
          stop 1
         end if

         total_regions = size_of_file(infile)

!       ------------------------------------------------------------------------
!       Allocating memory for all needed arrays

         allocate (pixmin(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (pixmax(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (wlmin(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (wlmax(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (velmin(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (velmax(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (significance(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate (lines_in_region(total_regions), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

!       ------------------------------------------------------------------------
!       Open file <ion>.ewr and read region parameters

         open(10,file=trim(infile), status='old')
         
          do i=1,total_regions
           read(10,*) dummy_int, pixmin(i), pixmax(i), wlmin(i), wlmax(i),
     1           velmin(i), velmax(i), significance(i)
          end do

         close(10)

!       ------------------------------------------------------------------------
!       Determine number of lines per region
!        Factor 1.0d3 determines precision to which velocity is considered
!        Conversion to integer done in order to make comparison possible
!
!        CHECK that the resulting number, i.e. vel*1e4, are still within the
!        numerical range of integer numbers. In that case, the numbers can
!        directly be compared to each other
        
!       Initialize counters
        lines_in_region(:) = 0

         do i=1,total_regions
          do j=1,total_lines
          
           if ((v_line(j).ge.floor(velmin(i))).and.
     &            (v_line(j).le.ceiling(velmax(i)))) then
             lines_in_region(i) = lines_in_region(i) + 1
            end if

          end do
         end do

!       Check that all lines have been counted; if not, stop

         if (sum(lines_in_region).ne.total_lines) then
          write(6,*) trim(errmsg)//'Sum of lines in all regions, ', 
     &          sum(lines_in_region),' does not match total number of lines: ',
     &          total_lines
                stop 1
         end if

!       ------------------------------------------------------------------------
        END IF !lines detected?
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Open output fit file in HDF5 format
!       ------------------------------------------------------------------------
!       File MUST exists already (generated by write_spec_hdf5)
!       but doesn't hurt to check
        
         dir = trim(homedir)//'/ifitnot/output_data/'//trim(simulation)
         outfile = trim(dir)//'/'//trim(fitfile_hdf5)
         
         inquire(file=outfile,exist=file_exists)
         
         if (.not.file_exists) then
         
          write(6,*) trim(errmsg)//'File '//trim(fitfile_hdf5)//' does not exist.'
          stop 1

         else

!       If file exists, open it and perform sanity check of spectrum
!       status; if spectrum already fitted, stop cleanly

          call hdf5_open_file(file_id,trim(outfile))
          call spectrum_status_check_w(file_id,spec_id)
          
         end if
        
!       ------------------------------------------------------------------------
!       Flag spectrum as lines-detected or no-lines-detected

         groupname='/Spectrum'//trim(spec_id)
         attributename=trim(groupname)//'/Lines_Detected'
         call hdf5_write_attribute(file_id,trim(attributename),
     &         trim(lines_detected_str))

!       If lines were detected, write fitted flux and line parameters to
!       output file (one group per spectrum)

         if (lines_detected) then

          call create_fitgroup(file_id,spec_id)

         end if
!       ------------------------------------------------------------------------
!       Close output file in HDF5 format

         call hdf5_close_file(file_id)
!       ------------------------------------------------------------------------
!       Destroy arrays when not longer needed to save memory
         call dealloc_arrays_write

!       ------------------------------------------------------------------------
!       By this point, everything should have run smoothly. Hence, reopen
!       HDF5 file and set spectrum-status flag 'Spectrum_Fitted' = 'TRUE'

         call spectrum_status_set(outfile,spec_id,'Spectrum_Fitted','TRUE')
         spectrum_fitted = .true.
         
!       ------------------------------------------------------------------------
!       For non-valid type of spectra
        else
         write(6,'(a)') trim(errmsg)//'Not valid spectral type: '//
     &         trim(spectype)
         stop 1
        end if

        write(6,'(a)') trim(statmsg)//'Done.'
!       ------------------------------------------------------------------------
        end program
!       ------------------------------------------------------------------------
