!       plot_spec_ascii: Subroutines and Functions

!       ----------------------------------------------------------------
        subroutine read_input_params

!       Reads input paramters and performs a sanity check 

        use common_strings
        use common_nums
        
        implicit none
        
        integer :: argcount

!       Call intrinsic IArgC()
        
         argcount = IArgC()

         if (argcount .ne. 3) then
          write(6,*) 'USAGE: plot_spec_ascii '//
     &    '<specfile dir> <specfile prefix> <output (ps/term/_wl/_vel/none)>'
          stop 1
         end if

!       files directory
         call GetArg(1,specfile_dir)

!        spectrum file; filename as prefix MUST be common to all
!        related files (fit, detected regions, etc.)
         call GetArg(2,specfile_prefix)

!        plot output type (term, postscript)
         call GetArg(3,out_type)

!       Perform sanity check (all other input parameters are checked sooner
!       or later...)

         if ((trim(out_type).ne.'term_wl').and.
     &   (trim(out_type).ne.'ps_wl').and.
     &   (trim(out_type).ne.'term_vel').and.
     &   (trim(out_type).ne.'ps_vel').and.
     &   (trim(out_type).ne.'none')) then
          write(6,*) trim(errmsg)//'Not a valid output type: ',trim(out_type)
          write(6,*) 'Choose between {term_wl/_vel or ps_wl/_vel}'
          stop 1
         end if

        end subroutine read_input_params
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine allocate_spectrum_arrays(size)

!       Allocates the necessary memory
        
        use common_nums
        use common_strings
        use spectrum
        use alloc

        implicit none
        
        integer :: size

!       ----------------------------------------------------------------
!       Allocating memory for needed arrays

!       Comoving physical quantitites

         allocate(flux(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(flux_nonoise(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(fit(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(sigma(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(gauss_random(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(wavelength(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if

         allocate(v_Hubble(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
        end subroutine allocate_spectrum_arrays
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine allocate_fit_arrays(size)

!       Allocates the necessary memory
        
        use common_nums
        use common_strings
        use arrays_fit
        use alloc

        implicit none
        
        integer :: size

!       ----------------------------------------------------------------
!       Allocating memory for needed arrays
!       ----------------------------------------------------------------
!       Allocate fit quantities

         allocate(z_abs(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if

         allocate(wl_line(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(log10_Nion(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(b_value(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
         allocate(equiv_width_mA(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(v_line(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(dNion(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
        
         allocate(db_value(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if
         allocate(dv_line(size), stat=iAs)
        
!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         end if

        end subroutine allocate_fit_arrays
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine allocate_regions_arrays(size)

!       Allocates the necessary memory
        
        use common_nums
        use common_strings
        use detected_regions
        use alloc

        implicit none
        
        integer :: size

!       ----------------------------------------------------------------
!       Allocating memory for needed arrays
!       ----------------------------------------------------------------
!       Allocating memory for detected regions arrays
        
         allocate(pixmin(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(pixmax(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(wlmin(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(wlmax(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(velmin(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(velmax(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(significance(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(lines_in_region(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

!       Velocity and wavelength of strongest line in region
         allocate(v_central_region(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

         allocate(wl_central_region(size), stat=iAs)

!       iAs = 0 if succeeded, otherwise non-zero error code
         if (iAs .ne. 0) then
           write (6,*) trim(errmsg)//'Allocation failed: ', iAs
           stop 1
         endif

        end subroutine allocate_regions_arrays
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine dealloc_arrays

!       Destroys arrays when no longer needed
        
        use spectrum
         
         if(ALLOCATED(flux)) DEALLOCATE(flux)
         if(ALLOCATED(fit)) DEALLOCATE(fit)
         if(ALLOCATED(wavelength)) DEALLOCATE(wavelength)
         if(ALLOCATED(v_Hubble)) DEALLOCATE(v_Hubble)
         if(ALLOCATED(sigma)) DEALLOCATE(sigma)
         if(ALLOCATED(gauss_random)) DEALLOCATE(gauss_random)
        
        end subroutine dealloc_arrays
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        function size_of_file(filename)

!       Returns the number of lines in file 'filename'

        use common_strings

        implicit none

        integer :: size_of_file, lines
        character(len=256) :: filename

!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine create_datafile_spec(outfile,name,points)

!       Creates a file 'outfile' containing three [four] columns:
!       wavelength | flux | fit | [Hubble velocity]
!       where [] indicates optional for short spectra

        use common_strings
        use common_nums
        use spectrum

        implicit none
        
        integer, intent(in) :: points

        integer :: i
        
        character(len=*), intent(in) :: outfile, name

!       ----------------------------------------------------------------
!       Create file

        open(60,file='output_data/'//trim(outfile))

         do        !reading until end of file
          read(60,*,end=70)
         end do
  70     continue
         backspace(60) ! avoid end-of-file (EOF) marker

         write(60,*)                 

!       Block separation required by gnuplot multiple data

!       SHORT spectrum
         if (trim(name) .eq. 'short') then
         
          write(60,*)                 
          write(60,*) '# obs. wavelength, flux, fit, Hubble velocity, '//
     &    'flux_nonoise, noise'
          write(60,*)
          
          do i=1,points
           write(60,*) wavelength(i), flux(i), fit(i), v_Hubble(i),
     &     flux_nonoise(i), sigma(i)*gauss_random(i), sigma(i)
          end do
         
         else

!       LONG spectrum
          write(60,*)                 
          write(60,*) '# obs. wavelength, flux, fit, flux_nonoise, noise'
          write(60,*)
          
          do i=1,points
           write(60,*) wavelength(i), flux(i), fit(i),
     &     flux_nonoise(i), sigma(i)*gauss_random(i), sigma(i)
          end do
         
         end if

        close(60)
        
        end subroutine create_datafile_spec
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine create_datafile_lineparams(outfile,name,lines)

!       Creates a file 'outfile' containing three [four] columns:
!       wavelength | flux | fit | [Hubble velocity]
!       where [] indicates optional for short spectra

        use common_strings
        use common_nums
        use arrays_fit

        implicit none
        
        intrinsic :: DLOG10

        integer, intent(in) :: lines

        integer :: i
        real :: pos(0:1)
        
        character(len=256) :: ch_b, ch_zabs, ch_ew, ch_Nion, ch_b_err,
     &  ch_Nion_err

        character(len=*), intent(in) :: outfile, name

!       ----------------------------------------------------------------
!       Define labels positons (later for plot with gnuplot)
!       Alternate between values (2 for now), to plot labels at different
!       levels and avoid overlapping
!       Since LOADS of things depend on the number of parameters plotted
!       (4 for now), include a parameter that accounts for this!

         pos(0) = 1.16
         pos(1) = pos(0) + 0.16
          
!       ----------------------------------------------------------------
!       Create file

        open(60,file='output_data/'//trim(outfile))
         
         do        !reading until end of file
          read(60,*,end=70)
         end do
  70     continue
         backspace(60) ! avoid end-of-file (EOF) marker

         write(60,*)                 

!       Block separation required by gnuplot multiple data
        
         if (trim(name).eq.'short') then
!       For SHORT spectrum
        
          write(60,*)                 
          write(60,*) '# Line center: in v_Hubble, in Redshift'
          write(60,*)
          write(60,*)
         
!       These are the line parameters which should in turn be included
!       in the spectra plots; the last columns are used to set the po-
!       sitions of the labels in the plot

          do i=1,lines

!       Write line parameters into strings to facilitate label positioning
           write(ch_zabs,'(f8.4)') z_abs(i)
           write(ch_b,'(f6.2)') b_value(i)
           write(ch_b_err,'(f6.2)') db_value(i)
           write(ch_ew,'(f6.2)') equiv_width_mA(i)
           write(ch_Nion,'(f6.2)') log10_Nion(i)
           write(ch_Nion_err,'(f6.2)') DLOG10(dNion(i))

!       The entries below are needed to include in the plot the line
!       parameters +/- errors at the right positions. The encoding
!       {/CMSY10 \\006} for +/- only works if label is enclosed in quotation
!       marks, and the gnuplot files loads the required font cmsy10.pfb
!       (see create_plotfile_ps below)
!       The line ID is now included as last (10) entry as an alternative way
!       of labelling lines, since for crowded regions this is not working
!       properly as it is because of extreme overlapping. The idea is to
!       include the line center '|', redshift and line ID as usual, and
!       include a legend with the line ID's and corresponding parameters

!       Distinguish between output 'term' and 'ps'
           
           if ((trim(out_type).eq.'term_wl').or.(trim(out_type).eq.
     &     'term_vel')) then
           
            write(60,'(f12.4,4(1x,a),4(1x,f5.2),1x,i4)') v_line(i),
     &      'z='//trim(adjustl(ch_zabs)),
     &      '"b='//trim(adjustl(ch_b))//' ('//
     &      trim(adjustl(ch_b_err))//')"',
     &      'EW='//trim(adjustl(ch_ew)),
     &      '"log N='//trim(adjustl(ch_Nion))//' ('
     &      //trim(adjustl(ch_Nion_err))//')"',
     &      pos(mod(i,2)),pos(mod(i,2))+0.04,pos(mod(i,2))+0.08,
     &      pos(mod(i,2))+0.12,
     &      i
           
           else
           
            write(60,'(f12.4,4(1x,a),4(1x,f5.2),1x,i2)') v_line(i),
     &      'z='//trim(adjustl(ch_zabs)),
     &      '"b='//trim(adjustl(ch_b))//'{/CMSY10 \\006}'//
     &      trim(adjustl(ch_b_err))//'"',
     &      'EW='//trim(adjustl(ch_ew)),
     &      '"N=10^{'//trim(adjustl(ch_Nion))//'}{/CMSY10 \\006}10^{'
     &      //trim(adjustl(ch_Nion_err))//'}"',
     &      pos(mod(i,2)), pos(mod(i,2))+0.04, pos(mod(i,2))+0.08,
     &      pos(mod(i,2))+0.12,
     &      i
           
           end if

          end do !i-loop over lines

          write(60,*)                 
          write(60,*) '# Line center in Velocity, Redshift and Line ID'
          write(60,*)
          write(60,*)
         
!       These block includes he line parameters used when alternate
!       labelling is switched on (see create_plotfile_ps_wl below)

          do i=1,lines

!       Write line parameters into strings to facilitate label positioning
           write(ch_zabs,'(f8.4)') z_abs(i)

           write(60,'(f12.4,1x,a,2(1x,f5.2),1x,i4)') v_line(i),
     &     trim(adjustl(ch_zabs)), pos(0), pos(1), i

          end do
        
         else
!       For LONG spectrum

          write(60,*)                 
          write(60,*) '# Line center: in Wavelength, in Redshift'
          write(60,*)
          write(60,*)

!       These are the line parameters which should in turn be included
!       in the spectra plots; the second to last columns are used to set
!       the positions of the labels in the plot

          do i=1,lines

!       Write line parameters into strings to facilitate label positioning
           write(ch_zabs,'(f8.4)') z_abs(i)
           write(ch_b,'(f6.2)') b_value(i)
           write(ch_b_err,'(f6.2)') db_value(i)
           write(ch_ew,'(f6.2)') equiv_width_mA(i)
           write(ch_Nion,'(f6.2)') log10_Nion(i)
           write(ch_Nion_err,'(f6.2)') DLOG10(dNion(i))

!       The entries below are needed to include in the plot the line
!       parameters +/- errors at the right positions. The encoding
!       {/CMSY10 \\006} for +/- only works if label is enclosed in quotation
!       marks, and the gnuplot file loads the required font cmsy10.pfb (see
!       create_plotfile_ps below)
!       The line ID is now included as last (10th) entry as an alternative way
!       of labelling lines, since for crowded regions this is not working
!       properly as it is because of extreme overlapping. The idea is to
!       include the line center '|', redshift and line ID as usual, and
!       include a legend with the line ID's and corresponding parameters

           write(60,'(f12.4,4(1x,a),4(1x,f5.2),1x,i4)') wl_line(i),
     &     'z='//trim(adjustl(ch_zabs)),
     &     '"b='//trim(adjustl(ch_b))//'{/CMSY10 \\006}'//
     &     trim(adjustl(ch_b_err))//'"',
     &     'EW='//trim(adjustl(ch_ew)),
     &     '"N=10^{'//trim(adjustl(ch_Nion))//'}{/CMSY10 \\006}10^{'
     &     //trim(adjustl(ch_Nion_err))//'}"',
     &     pos(mod(i,2)), pos(mod(i,2))+0.04, pos(mod(i,2))+0.08,
     &     pos(mod(i,2))+0.12,
     &     i

          end do
        
          write(60,*)                 
          write(60,*) '# Line center in Wavelength, Redshift and Line ID'
          write(60,*)
          write(60,*)
         
!       This block includes the line parameters used when alternate
!       labelling is switched on (default; see create_plotfile_ps_wl below)

          do i=1,lines

!       Write line parameters (line centre and id) into strings to facilitate
!       label positioning
           write(ch_zabs,'(f8.4)') z_abs(i)

           write(60,'(f12.4,1x,a,2(1x,f5.2),1x,i4)') wl_line(i),
     &     trim(adjustl(ch_zabs)), pos(0), pos(1), i

          end do
         end if

        close(60)
        
        end subroutine create_datafile_lineparams
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine create_datafile_residuals(outfile)

!       Creates a file 'outfile' containing two columns:
!       resiudals | noise 

        use common_strings
        use common_nums
        use spectrum
        use detected_regions

        implicit none
        
        integer :: i, j
        
        character(len=*), intent(in) :: outfile

!       ----------------------------------------------------------------
!       Create file

        open(60,file='output_data/'//trim(outfile))

         do        !reading until end of file
          read(60,*,end=70)
         end do
  70     continue
         backspace(60) ! avoid end-of-file (EOF) marker

         write(60,*)                 

!       Block separation required by gnuplot multiple data

         write(60,*)                 
         write(60,*) '# residuals, noise'
         write(60,*)
          
         do i=1,total_regions
          if (lines_in_region(i).gt.0) then
           do j=pixmin(i),pixmax(i)
            write(60,*) flux(j)-fit(j), sigma(j)*gauss_random(j)
           end do
          end if
         end do
         

        close(60)
        
        end subroutine create_datafile_residuals
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine create_plotfile_term_wl(file,name,indxx,maxval)

!       Creates a file to plot a spectrum as a function of wavelength (long)
!       or Hubble velocity (short). 'name' can be 'short' or 'long'.
!       Datafiles contain two blocks: (wavelength,flux,fit[,Hubble velocity])
!       and (line center in wavelength/redshift), respectively.
!       Loaded it within gnuplot. Output is to term.
        
        use common_strings
        use common_nums
        use detected_regions
        use spectrum

        implicit none
        
        integer, intent(in) :: indxx, maxval
        integer :: start, low, high, bound, i
        integer, parameter :: offset = 75 !offset defining plotting range

        double precision :: vel_low, vel_hi, wl_low, wl_hi
        
        character(len=*), intent(in) :: file, name
        character(len=256) :: datafile, title, xlabel, ch_indxx, ch_sim,
     +  ch_lines, ch_part_lines, ch_regions, S2N

!       ----------------------------------------------------------------
!       Replace underscore from simulation string (assuming it contains
!       only one) by a minus sign
!       This is because gnuplot interpretates the character following
!       and underscore as and index

        start=scan(simulation,'_')
        ch_sim=simulation(1:start-1)//'_'//simulation(start+1:)

        do i=1,len_trim(specfile_prefix)
         if (specfile_prefix(i:i).eq.'_') specfile_prefix(i:i)='-'
        end do

!       Convert line number, region number, and region index to string

         write(ch_indxx,'(i6)') indxx
         write(ch_lines,'(i6)') total_lines
         if (indxx.gt.0) write(ch_part_lines,'(i6)') lines_in_region(indxx)
         write(ch_regions,'(i6)') total_regions - skip_regions

!       Noise? Then include Signal-to-Noise
        if (trim(noise_added).eq.'TRUE') then
         write(S2N, '(f8.3)') average_sn
        else
         S2N = '1000'
        end if

!       Define datafile to read from
        
        datafile='output_data/'//trim(file)

!       open(50,file='output_data/plotspec'//trim(str_inset)//'_'//trim(name)//'_SN'//
!     &  trim(adjustl(average_sn_str))//'_'//
!     &  trim(adjustl(ch_indxx))//'.plot')
        open(50,file='output_data/plotspec'//trim(str_inset)//'_'//
     &  trim(adjustl(ch_indxx))//'.plot')
        
!       Set specific fontpath for using fancy symbols (see below)

         write(50,*) '# Set font path'
         write(50,*) "set fontpath '/usr/share/texmf/fonts/type1/public/amsfonts/cm/'"

!       Set fancy encoding

         write(50,*) '# For fancy symbol fonts'
         write(50,*) 'set encoding iso_8859_1'
         write(50,*)

!       Set terminal to x11 persist

         write(50,*) 'set term x11 enhanced persist'
         write(50,*) 'set key bottom right'
         write(50,*)

!       Define alternative x2 axis for short spectra
          
         write(50,*) 'set xtics nomirror'
         write(50,*) 'set mxtics'
         
         if (trim(name).eq.'short') then
         
          write(50,*) 'set x2tics'
          write(50,*) 'set mx2tics'
          write(50,*)
         
         end if

         write(50,*) 'set ytics format "%5.2f" '
         
!       Set title and axis labels
         
         if (indxx.gt.0) then
!       For individual regions         
         
          title=trim(specfile_prefix)//
     &    ' '//trim(ion)//' S/N= '//
     &    trim(adjustl(S2N))
          write(50,*) 'set title "'//trim(title)//' region: '//
     &    trim(adjustl(ch_indxx))//' / '//trim(adjustl(ch_regions))//
     &    '  lines: '//trim(adjustl(ch_part_lines))//' / '//
     &    trim(adjustl(ch_lines))//'"'
          write(50,*)
        
        else
!       For full spectrum and residuals
          
          title=trim(specfile_prefix)//
     &    ' '//trim(ion)//' S/N= '//
     &    trim(adjustl(S2N))
          write(50,*) 'set title "'//trim(title)//' regions: '//
     &    trim(adjustl(ch_regions))//'  lines: '//trim(adjustl(ch_lines))//'"'

        end if

!       Define plot xranges according to detected regions; watch out for array
!       boundaries; full range for indxx = 0 -> full spectrum
         
!       In pixels:

         bound = size(wavelength)
         
         if (indxx.gt.0) then
          
          low = pixmin(indxx)-offset
          high = pixmax(indxx)+offset

!       In velocity: (only for short spectra)
          if (trim(name).eq.'short') then
           vel_low = v_Hubble(max(1,low))
           vel_hi = v_Hubble(min(high,bound))
          end if

!       In wavelength:

          wl_low = wavelength(max(1,low))
          wl_hi = wavelength(min(high,bound))

         end if !indxx > 0

         if (trim(name).eq.'short') then
!       For SHORT spectrum

          if (indxx.gt.0) then
!       For individual regions         

            write(50,*) 'set xrange[',vel_low,':',vel_hi,']'
            write(50,*) 'set x2range[',wl_low,':',wl_hi,']'
          
          else if (indxx.eq.-2) then
!       For residuals histogram

           write(50,*) 'set xrange[*:*]'

          else
!       For full spectrum and residuals

           write(50,*) 'set xrange[',v_Hubble(1),':',v_Hubble(bound),']'
           write(50,*) 'set x2range[',wavelength(1),':',wavelength(bound),']'

          end if

         else if (trim(name).eq.'long') then
!       For LONG spectrum

          if (indxx.gt.0) then
!       For individual regions         

            write(50,*) 'set xrange[',wl_low,':',wl_hi,']'
          
          else if (indxx.eq.-2) then
!       For residuals histogram
           
           write(50,*) 'set xrange[*:*]'

          else
!       For full spectrum and residuals
           
           write(50,*) 'set xrange[',wavelength(1),':',wavelength(bound),']'

          end if

         end if
         
         if (trim(name).eq.'short') then
          xlabel = 'v_{Hubble} [km/s]'
         else
          xlabel = 'Wavelength [\305]'
         end if

         if (indxx.ne.-2) write(50,*) 'set xlabel "'//trim(xlabel)//'"'

         write(50,*)
         if (indxx.ge.0) then ! full spectrum and regions
          write(50,*) 'set ytics format "%5.2f"'
          write(50,*) 'set yrange[-0.05:1.5]'
          write(50,*) 'set ylabel "Normalised Flux"'
         else if (indxx.eq.-2) then ! residuals
          write(50,*) 'set ytics format "%5.2f"'
          write(50,*) 'set yrange[*:*]'
          write(50,*) 'set ylabel "PDF"'
          write(50,*) 'set xlabel "Residuals"'
         else if (indxx.eq.-1) then ! residuals
          write(50,*) 'set ytics format "%8.2e"'
          write(50,*) 'set yrange[*:*]'
          write(50,*) 'set ylabel "Residuals"'
         end if


!       ----------------------------------------------------------------
!       Actually plot

         if (trim(name).eq.'short') then

          if (indxx.eq.-2) then  ! residuals histogram
          
           write(50,*)
           write(50,*) '# Box width'
           write(50,*) 'bw = ', 2.0d-1/average_sn
           write(50,*) 'set boxwidth bw'

           write(50,*) '# Maximum value (to normalise histogram)'
           write(50,*) 'maxval = ',maxval

           write(50,*) '# Binning function'
           write(50,*) 'binc(x,s) = s*(ceil(x/s))'
           write(50,*) 'binr(x,s) = s*(ceil(x/s)+0.5)'

           write(50,*) '# Plot histogram '
           write(50,*) 'unset style'
           write(50,*) 'plot '''//trim(datafile)//''' index 3 using '
     &     //'(binc(($1),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lc rgb "white" title "residuals" \'
           write(50,*) ', '''//trim(datafile)//''' index 3 using '
     &     //'(binr(($2),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lc rgb "red" title "noise" \'

          else  if (indxx.eq.-1) then! residuals
          
           write(50,*)
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):(($2)-($3)) w histeps lt 1 lw 1.5 lc rgb "white" title "residuals" \'
!       input noise
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'($4):($6) w histeps lt 1 lw 1.5 lc rgb "yellow" title "noise" \'
           write(50,*) ', 0 w l lt 3 lw 1.5 lc rgb "red" notitle \'

          else if (indxx.eq.0) then ! spectrum

!       data | model vs. Hubble velocity
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2-$7) w boxes lc rgb "black" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "yellow" title "Data" \'
!       Fit
           write(50,*) ', "" index 0 using '
     &     //'($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \'
!       pseudo continuum
           write(50,*) ', 1 w l lw 1.5 lt 3 lc rgb "white" notitle \'
          
          else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then !regions

!       data | model vs. Hubble velocity
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2-$7) w boxes lc rgb "black" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "yellow" title "Data" \'
!       Fit
           write(50,*) ', "" index 0 using '
     &     //'($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \'
!       pseudo continuum
           write(50,*) ', 1 w l lw 1.5 lt 3 lc rgb "white" notitle \'
          
          end if ! spectrum or residuals?

         else !long spectrum

          if (indxx.eq.-2) then  ! residuals histogram
          
           write(50,*)
           write(50,*) '# Box width'
           write(50,*) 'bw = ', 2.0d-1/average_sn
           write(50,*) 'set boxwidth bw'

           write(50,*) '# Maximum value (to normalise histogram)'
           write(50,*) 'maxval = ',maxval

           write(50,*) '# Binning function'
           write(50,*) 'binc(x,s) = s*(ceil(x/s))'
           write(50,*) 'binr(x,s) = s*(ceil(x/s)+0.5)'

           write(50,*) '# Plot histogram '
           write(50,*) 'unset style'
           write(50,*) 'plot '''//trim(datafile)//''' index 3 using '
     &     //'(binc(($1),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lc rgb "white" title "residuals" \'
           write(50,*) ', '''//trim(datafile)//''' index 3 using '
     &     //'(binr(($2),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lc rgb "red" title "noise" \'

          else if (indxx.eq.-1) then ! residuals
          
           write(50,*)
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($1):(($2)-($3)) w histeps lt 1 lw 1.5 lc rgb "white" title "residuals" \'
!       input noise
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'($1):($5) w histeps lt 1 lw 1.5 lc rgb "yellow" title "noise" \'
           write(50,*) ', 0 w l lt 3 lw 1.5 lc rgb "red" notitle \'

          else if (indxx.eq.0) then ! spectrum

!       data | model vs. Wavelength
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2-$7) w boxes lc rgb "black" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "yellow" title "Data" \'
!       Fit
           write(50,*) ', "" index 0 using '
     &     //'($1):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \'
!       pseudo continuum
           write(50,*) ', 1 w l lw 1.5 lt 3 lc rgb "white" notitle \'

          
          else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then !regions

!       data | model vs. Wavelength
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2-$7) w boxes lc rgb "black" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "yellow" title "Data" \'
!       Fit
           write(50,*) ', "" index 0 using '
     &     //'($1):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \'
!       pseudo continuum
           write(50,*) ', 1 w l lw 1.5 lt 3 lc rgb "white" notitle \'

          
          end if ! spectrum or residuals?

         end if
         
!       Include line parameters in plot

         if (indxx.ge.0) then

         if (indxx.eq.0) then
         
!       line centroid
          write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &    //'($1):(1.1):("|") w labels tc rgb "green" notitle'

         else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

!       line centroid
          write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &    //'($1):(1.1):("|") w labels tc rgb "green" notitle \'
!       line redshift
          write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &    //'($1):($6):2 w labels font "Helvetica,9" tc rgb "green" notitle \'
!       b-value
          write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &    //'($1):($7):3 w labels font "Helvetica,9" tc rgb "green" notitle \'
!       equivalent width
          write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &    //'($1):($8):4 w labels font "Helvetica,9" tc rgb "green" notitle \'
!       column density
          write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &    //'($1):($9):5 w labels font "Helvetica,9" tc rgb "green" notitle'

         end if
        
         end if
        
        close(50)

        end subroutine create_plotfile_term_wl
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine create_plotfile_ps_wl(file,name,indxx,maxval)

!       Creates a file to plot a spectrum as a function of wavelength (long)
!       or Hubble velocity (short). 'name can be 'short' or 'long'.
!       Datafiles contain two blocks: (wavelength,flux,fit[,Hubble velocity])
!       and (line center in wavelength/redshift), respectively.
!       Loaded it within gnuplot. Output is to term.
        
        use common_strings
        use common_nums
        use detected_regions
        use spectrum
        use arrays_fit

        implicit none
        
        integer, intent(in) :: indxx, maxval
        integer :: i, low, high, bound
        integer, parameter :: offset = 75 !offset in pixel for plotting range

        real :: posit

        double precision :: vel_low, vel_hi, wl_low, wl_hi

        character(len=*), intent(in) :: file, name
        character(len=256) :: datafile, title, xlabel, x2label, ch_indxx,
     &  ch_sim, ch_lines, ch_part_lines, ch_regions, S2N

        character(len=32) :: ch_b, ch_zabs, ch_ew, ch_Nion, ch_b_err,
     &  ch_Nion_err

        logical :: alternative_labelling = .true.

!       ----------------------------------------------------------------
!       Replace underscore from simulation string (assuming it contains only
!       one) by a minus sign
!       This is because gnuplot interpretates the character following and
!       underscore as and index

        ch_sim=trim(simulation)
        do i=1,len_trim(ch_sim)
         if (ch_sim(i:i).eq.'_') ch_sim(i:i)='-'
        end do
        
        do i=1,len_trim(specfile_prefix)
         if (specfile_prefix(i:i).eq.'_') specfile_prefix(i:i)='-'
        end do

!       Convert line number, region number, and region index to string

         write(ch_indxx,'(i6)') indxx
         if (indxx.gt.0) write(ch_part_lines,'(i6)') lines_in_region(indxx)
         write(ch_lines,'(i6)') total_lines
         write(ch_regions,'(i6)') total_regions - skip_regions

!       Noise? Then include Signal-to-Noise
        if (trim(noise_added).eq.'TRUE') then
         write(S2N, '(f8.3)') average_sn
        else
         S2N = '1000'
        end if

!       Define datafile to read from
        
        datafile='output_data/'//trim(file)

        open(50,file='output_data/plotspec'//trim(str_inset)//'.plot')

!       read until end of file
         do
          read(50,*,end=80)
         end do
  80     continue
         backspace(50) ! avoid end-of-file (EOF) marker

!       Write header only once

         if (indxx.eq.-2) then

!       Set specific fontpath for using fancy symbols (see below)

          write(50,*) '# Set font path'
          write(50,*) "set fontpath '/usr/share/texmf/fonts/type1/public/amsfonts/cm/'"

!       Set term x11 and font cmsy10.pfb for special symbols (CMSY10 font) in
!       OMS encoding (see table ~/ps_fontfile_doc.ps)

          write(50,*) '# Set term x11 and font cmsy10.pfb for special symbols '
     &    //'(CMSY10 font) in OMS encoding (see table ~/ps_fontfile_doc.ps)'
          write(50,*) 'set term post enhanced color landscape fontfile "cmsy10.pfb" '
     &    //'font 20 dashlength 4'

!       Set output file and plot legend

!         write(50,*) 'set output "output_data/plotspec'//trim(str_inset)//'_'//trim(name)//'_SN'//
!     &    trim(adjustl(average_sn_str))//'.ps"'
          write(50,*) 'set output "output_data/plotspec'//trim(str_inset)//'.ps"'
          write(50,*) 'set key bottom right samplen 2'
          write(50,*)

!       Set further fancy encoding (e.g. for "Angstroem" symbol)

          write(50,*) '# For fancy symbol fonts'
          write(50,*) 'set encoding iso_8859_1'
          write(50,*)

!       Set plot margins

          write(50,*) '# Fixed position using screen coordinates where:'
          write(50,*) '# (0,0)=bottom-left corner of PAGE'
          write(50,*) '# (1,1)=top-right corner of PAGE'
          write(50,*) '# The following setting are such that the printing'
          write(50,*) '# area is maximed, reducing the margin around the plot'
          write(50,*) '# to a bearable minimum (for this type of plot)'
          write(50,*) 'set lmargin screen 0.12'
          write(50,*) 'set rmargin screen 0.99'
          write(50,*) 'set bmargin screen 0.08'
          write(50,*) 'set tmargin screen 0.90'
          write(50,*)

!       Define alternative x2 axis for short spectra
          
          if (trim(name).eq.'short') then
          
           write(50,*) '# Set alternative x-axis'
           write(50,*) 'set xtics nomirror'
           write(50,*) 'set mxtics'
           write(50,*) 'set x2tics'
           write(50,*) 'set mx2tics'
           write(50,*)

           write(50,*) '# Set y-axis format'
           write(50,*) 'set ytics format "%5.2f"'
          
          end if

         end if !indxx = -2, i.e. for one single header

!       Set title

         write(50,*) '# Set title'
         if (indxx.gt.0) then

!       For individual regions         
         
          write(50,*)
          title=trim(specfile_prefix)//
     &    ' '//trim(ion)//' S/N= '//
     &    trim(adjustl(S2N))
          write(50,*) 'set title "{/=10 '//trim(title)//' region: '//
     &    trim(adjustl(ch_indxx))//' / '//trim(adjustl(ch_regions))//
     &    '  lines: '//trim(adjustl(ch_part_lines))//' / '//
     &    trim(adjustl(ch_lines))//'}"'
          write(50,*)
        
        else

!       For full spectrum
          
          write(50,*)
          title=trim(specfile_prefix)//
     &    ' '//trim(ion)//' S/N= '//
     &    trim(adjustl(S2N))
          write(50,*) 'set title "{/=10 '//trim(title)//' regions: '//
     &    trim(adjustl(ch_regions))//'  lines: '//trim(adjustl(ch_lines))//
     &    ', SHIFT = ', spectrum_shift,'}"'

        end if

!       Define plot xranges according to detected regions; watch out for array
!       boundaries; full range for indxx = 0 -> full spectrum

!       In pixels:

         bound = size(wavelength)

         if (indxx.gt.0) then
          
          low = pixmin(indxx)-offset
          high = pixmax(indxx)+offset

!       In velocity: (only for short spectrum)

          if (trim(name).eq.'short') then
           vel_low = v_Hubble(max(1,low))
           vel_hi = v_Hubble(min(high,bound))
          end if

!       In wavelength:

          wl_low = wavelength(max(1,low))
          wl_hi = wavelength(min(high,bound))
         
         end if !indxx > 0

         if (trim(name).eq.'short') then
!       For SHORT spectrum

          if (indxx.gt.0) then
!       For individual regions         

            write(50,*) 'set xrange[',vel_low,':',vel_hi,']'
            write(50,*) 'set x2range[',wl_low,':',wl_hi,']'
          
          else if (indxx.eq.-2) then
!       For residuals histogram

           write(50,*) 'set xrange[*:*]'

          else
!       For full spectrum and residuals

           write(50,*) 'set xrange[',v_Hubble(1),':',v_Hubble(bound),']'
           write(50,*) 'set x2range[',wavelength(1),':',wavelength(bound),']'

          end if

         else if (trim(name).eq.'long') then
!       For LONG spectrum

          if (indxx.gt.0) then
!       For individual regions         

            write(50,*) 'set xrange[',wl_low,':',wl_hi,']'

          else if (indxx.eq.-2) then
!       For residuals histogram
           
           write(50,*) 'set xrange[*:*]'

          else
!       For full spectrum and residuals
           
           write(50,*) 'set xrange[',wavelength(1),':',wavelength(bound),']'

          end if

         end if

!       Set y-plotting range and axes labels
         
         if (indxx.ne.-2) then
         
          if (trim(name).eq.'short') then
          
           xlabel = 'v_{Hubble} [km/s]'
           x2label = 'Wavelength [\305]'
           write(50,*) 'set xlabel "'//trim(xlabel)//'"'
           write(50,*) 'set x2label "'//trim(x2label)//'"'
           write(50,*)
          
          else
          
           xlabel = 'Wavelength [\305]'
           write(50,*) 'set xlabel "'//trim(xlabel)//'"'
           write(50,*)
         
          end if
         
         end if

         if (indxx.ge.0) then ! full spectrum and regions
          write(50,*) 'set ytics format "%5.2f"'
          write(50,*) 'set yrange[-0.05:1.5]'
          write(50,*) 'set ylabel "Normalised Flux"'
         else if (indxx.eq.-2) then ! residuals
          write(50,*) 'set ytics format "%5.2f"'
          write(50,*) 'set yrange[*:*]'
          write(50,*) 'set ylabel "PDF"'
          write(50,*) 'set xlabel "Residuals"'
         else if (indxx.eq.-1) then ! residuals
          write(50,*) 'set ytics format "%8.2e"'
          write(50,*) 'set yrange[*:*]'
          write(50,*) 'set ylabel "Residuals"'
         end if
         
!       ----------------------------------------------------------------
!       Alternative line labelling (default)
!       ----------------------------------------------------------------
         if (indxx.gt.0) then
 
          if (alternative_labelling) then

!       These are the line parameters which should in turn be included
!       in the spectra plots; the last columns are used to set the po-
!       sitions of the labels in the plot

           posit = 0.1
 
           do i=1,total_lines

!       Write line parameters into strings to facilitate label positioning
!       Only for lines in current region

            if ((v_line(i).ge.velmin(indxx)).and.(v_line(i).le.velmax(indxx)))
     &      then
             
             write(ch_zabs,'(f8.4)') z_abs(i)
             write(ch_b,'(f6.2)') b_value(i)
             write(ch_b_err,'(f6.2)') db_value(i)
             write(ch_ew,'(f6.2)') equiv_width_mA(i)
             write(ch_Nion,'(f6.2)') log10_Nion(i)
             write(ch_Nion_err,'(f6.2)') DLOG10(dNion(i))

!       The entries below are needed to include in the plot the line
!       parameters +/- errors at the right positions. The encoding
!       {/CMSY10 \\006} for +/- only works if label is enclosed in quotation
!       marks, and the gnuplot files loads the required font cmsy10.pfb
!       (see create_plotfile_ps below)

             write(50,*) 'set label front "{/=9 ',i,'}" at graph 0.02, ', posit,
     &       'tc rgb "black"'
             write(50,*) 'set label front "{/=8 N_{'//trim(ion)//'}=10^{'//
     &       trim(adjustl(ch_Nion))//'}{/CMSY10 \\006}10^{'//trim(adjustl(ch_Nion_err))
     &       //'}" at graph 0.05, ', posit, 'tc rgb "royalblue"'
             write(50,*) 'set label front "{/=8 b='//trim(adjustl(ch_b))//
     &       '{/CMSY10 \\006}'//trim(adjustl(ch_b_err))//'}" at graph 0.20, ', posit,
     &       'tc rgb "royalblue"'
             write(50,*) 'set label front "{/=8 EW'
     &       //'='//trim(adjustl(ch_ew))//'}" at graph 0.29, ', posit,
     &       'tc rgb "royalblue"'
             write(50,*) 'set label front "{/=8 z'
     &       //'='//trim(adjustl(ch_zabs))//'}" at graph 0.37, ', posit,
     &       'tc rgb "royalblue"'
          
!       Increment positioning
             posit = posit + 0.04

            end if !line in region

           end do
                
          end if !alternative labelling
         
         end if ! indxx > 0

         write(50,*)
!       ----------------------------------------------------------------

!       Actually plot

         if (trim(name).eq.'short') then
          
          if (indxx.eq.-2) then  ! residuals histogram
          
           write(50,*)
           write(50,*) '# Box width'
           write(50,*) 'bw = ', 2.0d-1/average_sn
           write(50,*) 'set boxwidth bw'

           write(50,*) '# Maximum value (to normalise histogram)'
           write(50,*) 'maxval = ',maxval

           write(50,*) '# Binning function'
           write(50,*) 'binc(x,s) = s*(ceil(x/s))'
           write(50,*) 'binr(x,s) = s*(ceil(x/s)+0.5)'

           write(50,*) '# Plot histogram '
           write(50,*) 'unset style'
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 3 using '
     &     //'(binc(($1),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lc rgb "black" title "residuals" \'
           write(50,*) ', "" index 3 using '
     &     //'(binr(($2),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lw 1.5 lc rgb "red" title "noise" \'

          else if (indxx.eq.-1) then ! residuals
          
           write(50,*)
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):(($2)-($3)) w histeps lt 1 lw 1.5 lc rgb "black" title '//
     &     '"residuals" \'
!       input noise
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'($4):($6) w histeps lt 1 lw 1.5 lc rgb "red" title "noise" \'
           write(50,*) ', 0 w l lt 3 lw 1.5 lc rgb "royalblue" notitle \'

          else if (indxx.eq.0) then ! full spectrum

!       data | model vs. Hubble velocity
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \'
!       Fit
           write(50,*) ', "" index 0 using '
     &     //'($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \'
!       pseudo-continuum
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'
          
          else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then
          ! individual regions

!       data | model vs. Hubble velocity
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \'
!       Fit
           write(50,*) ', "" index 0 using '
     &     //'($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \'
!       pseudo-continuum
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'
          
          end if ! spectrum or residuals?
          
         else !long spectrum
          
          if (indxx.eq.-2) then  ! residuals histogram
          
           write(50,*)
           write(50,*) '# Box width'
           write(50,*) 'bw = ', 2.0d-1/average_sn
           write(50,*) 'set boxwidth bw'

           write(50,*) '# Maximum value (to normalise histogram)'
           write(50,*) 'maxval = ',maxval

           write(50,*) '# Binning function'
           write(50,*) 'binc(x,s) = s*(ceil(x/s))'
           write(50,*) 'binr(x,s) = s*(ceil(x/s)+0.5)'

           write(50,*) '# Plot histogram '
           write(50,*) 'unset style'
           write(50,*) 'plot '''//trim(datafile)//''' index 3 using '
     &     //'(binc(($1),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lc rgb "black" title "residuals" \'
           write(50,*) ', '''//trim(datafile)//''' index 3 using '
     &     //'(binr(($2),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lw 1.5 lc rgb "red" title "noise" \'

          else if (indxx.eq.-1) then ! residuals
          
           write(50,*)
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($1):(($2)-($3)) w histeps lt 1 lw 1.5 lc rgb "black" title "residuals" \'
!       input noise
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'($1):($5) w histeps lt 1 lw 1.5 lc rgb "red" title "noise" \'
           write(50,*) ', 0 w l lt 3 lw 1.5 lc rgb "royalblue" notitle \'

          else if (indxx.gt.0) then
           
           if (lines_in_region(indxx).gt.0) then ! individual regions
          
!       data | model vs. Wavelength
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \'
            write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &      //'($1):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \'
            write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'
           
           end if
           
          end if ! spectrum or residuals?
         
         end if ! short or long spectrum

!       Include line parameters in plot

         if (indxx.ge.0) then

         if (alternative_labelling) then

          if (indxx.eq.0) then
         
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):(1.1):("|") w labels notitle'

          else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

!       line centroid
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):(1.1):("|") w labels notitle \'
           write(50,*) ', '''//trim(datafile)//''' index 2 using '
!       column density
     &     //'($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \'
!       line redshift
           write(50,*) '#, '''//trim(datafile)//''' index 2 using '
     &     //'($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \'

          end if
        
         else
        
          if (indxx.eq.0) then
         
!       line centroid
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):(1.1):("|") w labels notitle'

          else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

!       line centroid
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):(1.1):("|") w labels notitle \'
!       line redshift
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):($6):2 w labels font "Helvetica,9" tc lt 3 notitle \'
!       b-value
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):($7):3 w labels font "Helvetica,9" tc lt 3 notitle \'
!       equivalent width
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):($8):4 w labels font "Helvetica,9" tc lt 3 notitle \'
!       column density
           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'($1):($9):5 w labels font "Helvetica,9" tc lt 3 notitle'

          end if
         
         end if !alternative labelling
         
         end if
         
         write(50,*)

!       Unset labels for next panel
         write(50,*) '# Unset labels for next panel'
         write(50,*) 'unset label'
         write(50,*)

        close(50)

        end subroutine create_plotfile_ps_wl
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine create_plotfile_ps_vel(file,name,indxx,maxval)

!       Creates a file to plot a spectrum as a function of wavelength
!       (long) or Hubble velocity (short). 'name can be 'short' or 'long'.
!       Datafiles contain two blocks: (wavelength,flux,fit[,Hubble
!       velocity]) and (line center in wavelength/redshift), respectively.
!       Loaded it within gnuplot. Output is to term.
        
        use common_strings
        use common_nums
        use detected_regions
        use spectrum
        use arrays_fit

        implicit none
        
        integer, intent(in) :: indxx, maxval
        integer :: i, low, high, bound

        real :: posit

        double precision :: vel_low, vel_hi, wl_low, wl_hi
        double precision :: radius

        character(len=*), intent(in) :: file, name
        character(len=256) :: datafile, title, xlabel, ch_indxx,
     &  ch_sim, ch_lines, ch_part_lines, ch_regions, S2N

        character(len=256) :: ch_b, ch_zabs, ch_ew, ch_Nion, ch_b_err,
     &  ch_Nion_err

        logical :: alternative_labelling = .true.

!       ----------------------------------------------------------------
!       Replace underscore from simulation string (assuming it contains only one)
!       by a minus sign
!       This is because gnuplot interpretates the character following and underscore
!       as and index

        ch_sim=trim(simulation)
        do i=1,len_trim(ch_sim)
         if (ch_sim(i:i).eq.'_') ch_sim(i:i)='-'
        end do

        do i=1,len_trim(specfile_prefix)
         if (specfile_prefix(i:i).eq.'_') specfile_prefix(i:i)='-'
        end do

!       Convert line number, region number, and region index to string

         write(ch_indxx,'(i6)') indxx
         if (indxx.gt.0) write(ch_part_lines,'(i6)') lines_in_region(indxx)
         write(ch_lines,'(i6)') total_lines
         write(ch_regions,'(i6)') total_regions - skip_regions

!       Noise? Then include Signal-to-Noise
        if (trim(noise_added).eq.'TRUE') then
         write(S2N, '(f8.3)') average_sn
        else
         S2N = '1000'
        end if

!       Define datafile to read from
        
        datafile='output_data/'//trim(file)

        open(50,file='output_data/plotspec'//trim(str_inset)//'.plot')
         
         do        !reading until end of file
          read(50,*,end=80)
         end do
  80     continue
         backspace(50) ! avoid end-of-file (EOF) marker
        
!       Write header only once

         if (indxx.eq.-2) then

!       Set specific fontpath for using fancy symbols (see below)

          write(50,*) '# Set font path'
          write(50,*) "set fontpath '/usr/share/texmf/fonts/type1/public/amsfonts/cm/'"

!       Set term x11 and font cmsy10.pfb for special symbols (CMSY10 font) 
!       in OMS encoding (see table ~/ps_fontfile_doc.ps)

          write(50,*) '# Set term x11 and font cmsy10.pfb for special symbols '//
     &    '(CMSY10 font) in OMS encoding (see table ~/ps_fontfile_doc.ps)'
          write(50,*) 'set term post enhanced color landscape fontfile "cmsy10.pfb" '
     &    //'font 20 dashlength 4'

!       Set output file and plot legend

!         write(50,*) 'set output "output_data/plotspec'//trim(str_inset)//'_'//trim(name)//'_SN'//
!     &    trim(adjustl(average_sn_str))//'.ps"'
          write(50,*) 'set output "output_data/plotspec'//trim(str_inset)//'.ps"'
          write(50,*) 'set key bottom right samplen 2'
          write(50,*)

!       Set further fancy encoding (e.g. for "Angstroem" symbol)

          write(50,*) '# For fancy symbol fonts'
          write(50,*) 'set encoding iso_8859_1'
          write(50,*)

!       Set plot margins

          write(50,*) '# Fixed position using screen coordinates where:'
          write(50,*) '# (0,0)=bottom-left corner of PAGE'
          write(50,*) '# (1,1)=top-right corner of PAGE'
          write(50,*) '# The following setting are such that the printing'
          write(50,*) '# area is maximed, reducing the margin around the plot'
          write(50,*) '# to a bearable minimum (for this type of plot)'
          write(50,*) 'set lmargin screen 0.12'
          write(50,*) 'set rmargin screen 0.99'
          write(50,*) 'set bmargin screen 0.08'
          write(50,*) 'set tmargin screen 0.90'
          write(50,*)

!       Define alternative x2 axis for short spectra
          
          if (trim(name).eq.'short') then
          
           write(50,*) '# Set alternative x-axis'
           write(50,*) 'set xtics nomirror'
           write(50,*) 'set mxtics'
           write(50,*)
          
           write(50,*) '# Set y-axis format'
           write(50,*) 'set ytics format "%5.2f"'

          end if

         end if !indxx = -2, i.e. for one single header

!       Set title

         write(50,*) '# Set title'
         if (indxx.gt.0) then

!       For individual regions         
         
          title=trim(specfile_prefix)//
     &    ' '//trim(ion)//' S/N= '//
     &    trim(adjustl(S2N))
          write(50,*) 'set title "{/=10 '//trim(title)//' region: '//
     &    trim(adjustl(ch_indxx))//' / '//trim(adjustl(ch_regions))//
     &    '  lines: '//trim(adjustl(ch_part_lines))//' / '//
     &    trim(adjustl(ch_lines))//'}"'
          write(50,*)
        
        else

!       For full spectrum
          
          title=trim(specfile_prefix)//
     &    ' '//trim(ion)//' S/N= '//
     &    trim(adjustl(S2N))
          write(50,*) 'set title "{/=10 '//trim(title)//' regions: '//
     &    trim(adjustl(ch_regions))//'  lines: '//trim(adjustl(ch_lines))//
     &    ', SHIFT = ', spectrum_shift,'}"'
          write(50,*)
        
        end if

!       Define plot xranges according to detected regions; watch out for array
!       boundaries; full range for indxx = 0 -> full spectrum

!       In pixels:

         bound = size(v_Hubble)

         if (indxx.gt.0) then
          
          low = pixmin(indxx)
          high = pixmax(indxx)

!       Define radius around strongest feature

!         radius = max(v_central_region(indxx)-v_Hubble(low), v_Hubble(high)
!     &    - v_central_region(indxx))+500.
          radius = 200.

!       In velocity (including velocity offset with respect to
!       strongest line):

          vel_low = max(v_Hubble(1),v_central_region(indxx)-radius) -
     &    v_central_region(indxx)
          vel_hi = min(v_Hubble(bound),v_central_region(indxx)+radius) -
     &    v_central_region(indxx)

!       In wavelength (including wavelength offset with respect to strongest
!       line):

          wl_low = max(wavelength(1),wavelength(low)) - wl_central_region(indxx)
          wl_hi = min(wavelength(high),wavelength(bound)) -
     &    wl_central_region(indxx)
         
         end if ! indxx > 0

         if (trim(name).eq.'short') then
!       For SHORT spectrum

          if (indxx.gt.0) then
!       For individual regions         

            write(50,*) 'set xrange[',vel_low,':',vel_hi,']'
          
          else if (indxx.eq.-2) then
!       For residuals histogram

           write(50,*) 'set xrange[*:*]'

          else
!       For full spectrum and residuals

           write(50,*) 'set xrange[',v_Hubble(1),':',v_Hubble(bound),']'

          end if

         else if (trim(name).eq.'long') then
!       For LONG spectrum

          if (indxx.gt.0) then
!       For individual regions         

            write(50,*) 'set xrange[',wl_low,':',wl_hi,']'
          
          else if (indxx.eq.-2) then
!       For residuals histogram
           
           write(50,*) 'set xrange[*:*]'

          else
!       For full spectrum and residuals
           
           write(50,*) 'set xrange[',wavelength(1),':',wavelength(bound),']'

          end if

         end if

!       Set y-plotting range and axes labels
         
         if (indxx.ge.0) then ! full spectrum and regions
          write(50,*) 'set ytics format "%5.2f"'
          write(50,*) 'set yrange[-0.05:1.5]'
          write(50,*) 'set ylabel "Normalised Flux"'
         else if (indxx.eq.-2) then ! residuals
          write(50,*) 'set ytics format "%5.2f"'
          write(50,*) 'set yrange[*:*]'
          write(50,*) 'set ylabel "PDF"'
          write(50,*) 'set xlabel "Residuals"'
         else if (indxx.eq.-1) then ! residuals
          write(50,*) 'set ytics format "%8.2e"'
          write(50,*) 'set yrange[*:*]'
          write(50,*) 'set ylabel "Residuals"'
         end if

!       Set axes labels
         
         if (trim(name).eq.'short') then
          
          if (indxx.eq.-1) then

           xlabel = 'Hubble Velocity [kms^{-1}]'
           write(50,*) 'set xlabel "'//trim(xlabel)//'"'
          
          else if (indxx.eq.0) then

           xlabel = 'Hubble Velocity [kms^{-1}]'
           write(50,*) 'set xlabel "'//trim(xlabel)//'"'
          
          else if (indxx.gt.0) then

           xlabel = 'Rest-frame Velocity [kms^{-1}]'
           write(50,*) 'set xlabel "'//trim(xlabel)//'"'
          
         end if

          write(50,*)
          
         else
          
          xlabel = 'Wavelength [\305]'
          write(50,*) 'set xlabel "'//trim(xlabel)//'"'
          write(50,*)
         
         end if

!       ----------------------------------------------------------------
!       Alternative line labelling
!       ----------------------------------------------------------------
         if (indxx.gt.0) then
 
          if (alternative_labelling) then

!       These are the line parameters which should in turn be included
!       in the spectra plots; the last columns are used to set the po-
!       sitions of the labels in the plot

           posit = 0.1
 
           do i=1,total_lines

!       Write line parameters into strings to facilitate label positioning
!       Only for lines in current region

            if ((v_line(i).ge.velmin(indxx)).and.(v_line(i).le.velmax(indxx)))
     &      then
             
             write(ch_zabs,'(f8.4)') z_abs(i)
             write(ch_b,'(f6.2)') b_value(i)
             write(ch_b_err,'(f6.2)') db_value(i)
             write(ch_ew,'(f6.2)') equiv_width_mA(i)
             write(ch_Nion,'(f6.2)') log10_Nion(i)
             write(ch_Nion_err,'(f6.2)') DLOG10(dNion(i))

!       The entries below are needed to include in the plot the line
!       parameters +/- errors at the right positions. The encoding {/CMSY10 \\006}
!       for +/- only works if label is enclosed in quotation marks, and the gnuplot
!       files loads the required font cmsy10.pfb (see create_plotfile_ps below)

             write(50,*) 'set label front "{/=9 ',i,'}" at graph 0.02, ', posit,
     &       'tc rgb "black"'
             write(50,*) 'set label front "{/=8 N_{'//trim(ion)//'}=10^{'//
     &       trim(adjustl(ch_Nion))//'}{/CMSY10 \\006}10^{'//trim(adjustl(ch_Nion_err))
     &       //'}" at graph 0.05, ', posit, 'tc rgb "royalblue"'
             write(50,*) 'set label front "{/=8 b='//trim(adjustl(ch_b))//
     &       '{/CMSY10 \\006}'//trim(adjustl(ch_b_err))//'}" at graph 0.20, ', posit,
     &       'tc rgb "royalblue"'
             write(50,*) 'set label front "{/=8 EW'
     &       //'='//trim(adjustl(ch_ew))//'}" at graph 0.29, ', posit,
     &       'tc rgb "royalblue"'
             write(50,*) 'set label front "{/=8 z'
     &       //'='//trim(adjustl(ch_zabs))//'}" at graph 0.37, ', posit,
     &       'tc rgb "royalblue"'
          
!       Increment positioning
             posit = posit + 0.04

            end if !line in region

           end do
                
          end if !alternative labelling
         
         end if ! indxx = 0

         write(50,*)
!       ----------------------------------------------------------------
!       Actually plot

         if (trim(name).eq.'short') then
!       data | model vs. Hubble velocity

          if (indxx.eq.0) then

           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'(($4)):($2) w histeps lt 1 lw 1.5 lc rgb "black" title '
     &     //'"Data" \'
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'(($4)):($3) w l lt 1 lw 1.5 lc rgb "red" title '
     &     //'"Model (ifit!)" \'
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'

          else if (indxx.eq.-2) then  ! residuals histogram
          
           write(50,*)
           write(50,*) '# Box width'
           write(50,*) 'bw = ', 2.0d-1/average_sn
           write(50,*) 'set boxwidth bw'

           write(50,*) '# Maximum value (to normalise histogram)'
           write(50,*) 'maxval = ', maxval

           write(50,*) '# Binning function'
           write(50,*) 'binc(x,s) = s*(ceil(x/s))'
           write(50,*) 'binr(x,s) = s*(ceil(x/s)+0.5)'

           write(50,*) '# Plot histogram '
           write(50,*) 'unset style'
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 3 using '
     &     //'(binc(($1),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lc rgb "black" title "residuals" \'
           write(50,*) ', '''//trim(datafile)//''' index 3 using '
     &     //'(binr(($2),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lw 1.5 lc rgb "red" title "noise" \'

          else if (indxx.eq.-1) then  ! residuals
          
           write(50,*)
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):(($2)-($3)) w histeps lt 1 lw 1.5 lc rgb "black" title "residuals" \'
!       input noise
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'($4):($6) w histeps lt 1 lw 1.5 lc rgb "red" title "noise" \'
           write(50,*) ', 0 w l lt 3 lw 1.5 lc rgb "royalblue" notitle \'

          else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'(($4)-', v_central_region(indxx), '):($2+$7) w boxes'
     &     //' lc rgb "cyan" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'(($4)-', v_central_region(indxx), '):($2-$7) w boxes'
     &     //' lc rgb "white" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'(($4)-', v_central_region(indxx), '):($2) w histeps'
     &     //' lc rgb "black" title "Data" \'
           write(50,*) ', "" index 0 using '
     &     //'(($4)-', v_central_region(indxx), '):($3) w l lw 1.5 lt 1'
     &     //' lc rgb "red" title "Model (ifit!)" \'
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'

          end if !indxx > 0

         else !long spectrum
!       data | model vs. Wavelength

          if (indxx.eq.0) then ! full spectrum
          
           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'(($1)):($2) w histeps lt 1 lw 1.5 lc rgb "black" title '
     &     //'"Data" \'
           write(50,*) ', "" index 0 using '
     &     //'(($1)):($3) w l lt 1 lw 1.5 lc rgb "red" title '
     &     //'"Model (ifit!)" \'
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'
         
          else if (indxx.eq.-2) then  ! residuals histogram
          
           write(50,*)
           write(50,*) '# Box width'
           write(50,*) 'bw = ', 2.0d-1/average_sn
           write(50,*) 'set boxwidth bw'

           write(50,*) '# Maximum value (to normalise histogram)'
           write(50,*) 'maxval = ',maxval

           write(50,*) '# Binning function'
           write(50,*) 'binc(x,s) = s*(ceil(x/s))'
           write(50,*) 'binr(x,s) = s*(ceil(x/s)+0.5)'

           write(50,*) '# Plot histogram '
           write(50,*) 'unset style'
           write(50,*) 'plot '''//trim(datafile)//''' index 3 using '
     &     //'(binc(($1),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lc rgb "black" title "residuals" \'
           write(50,*) ', '''//trim(datafile)//''' index 3 using '
     &     //'(binr(($2),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lw 1.5 lc rgb "red" title "noise" \'

          else if (indxx.eq.-1) then ! residuals
          
           write(50,*)
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'($4):(($2)-($3)) w histeps lt 1 lw 1.5 lc rgb "black" title "residuals" \'
!       input noise
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'($4):($6) w histeps lt 1 lw 1.5 lc rgb "red" title "noise" \'
           write(50,*) ', 0 w l lt 3 lw 1.5 lc rgb "royalblue" notitle \'

          else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then
          ! individual regions

           write(50,*) 'unset boxwidth'
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) 'plot '''//trim(datafile)//''' index 0 using '
     &     //'(($1)-', wl_central_region(indxx), '):($2+$7) w boxes '
     &     //'lc rgb "cyan" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'(($1)-', wl_central_region(indxx), '):($2-$7) w boxes '
     &     //'lc rgb "white" notitle \'
           write(50,*) ', "" index 0 using '
     &     //'(($1)-', wl_central_region(indxx), '):($2) w histeps lt 1 lw 1.5 lc rgb "black" title '
     &     //'"Data" \'
           write(50,*) ', '''//trim(datafile)//''' index 0 using '
     &     //'(($1)-', wl_central_region(indxx), '):($3) w l lt 1 lw 1.5 lc rgb "red" title '
     &     //'"Model (ifit!)" \'
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'
         
          end if! spectrum or residuals?
         
         end if !short/long spectrum

!       Include line parameters in plot

!       ----------------------------------------------------------------
!       SHORT spectrum: labelling

         if (trim(name).eq.'short') then

          if (indxx.ge.0) then

          if (alternative_labelling) then

           if (indxx.eq.0) then
         
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)):(1.1):("|") w labels notitle'

           else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

           write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', v_central_region(indxx), '):(1.1):("|") w labels notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 2 using '
     &     //'(($1)-', v_central_region(indxx), '):($3):5 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) '#, '''//trim(datafile)//''' index 2 using '
     &     //'(($1)-', v_central_region(indxx), '):($4):2 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'

           end if
        
          else !non alternative labelling, short spectrum

           if (indxx.eq.0) then
         
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)):(1.1):("|") w labels notitle'

           else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', v_central_region(indxx), '):(1.1):("|") w labels notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', v_central_region(indxx), '):($6):2 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', v_central_region(indxx), '):($7):3 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', v_central_region(indxx), '):($8):4 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', v_central_region(indxx), '):($9):5 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle'

           end if
        
          end if !alternative labelling, short spectrum

          end if

!       Unset labels for next panel
          write(50,*) '# Unset labels for next panel'
          write(50,*) 'unset label'
          write(50,*)

!       ----------------------------------------------------------------
!       LONG spectrum: labelling

         else
          
          if (indxx.ge.0) then

          if (alternative_labelling) then

           if (indxx.eq.0) then

            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)):(1.1):("|") w labels notitle'

           else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', wl_central_region(indxx), '):(1.1):("|") w labels notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 2 using '
     &     //'(($1)-', wl_central_region(indxx), '):($3):2 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 2 using '
     &     //'(($1)-', wl_central_region(indxx), '):($4):5 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'

           end if !indxx = 0, long spectrum
          
          else !non-alternate labelling, long spectrum
          
           if (indxx.eq.0) then
         
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)):(1.1):("|") w labels notitle'

           else if ((indxx.gt.0).and.(lines_in_region(indxx).gt.0)) then

            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', wl_central_region(indxx), '):(1.1):("|") w labels notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', wl_central_region(indxx), '):($6):2 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', wl_central_region(indxx), '):($7):3 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', wl_central_region(indxx), '):($8):4 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle \'
            write(50,*) ', '''//trim(datafile)//''' index 1 using '
     &     //'(($1)-', wl_central_region(indxx), '):($9):5 w labels font "Helvetica,9"'
     &     //' tc lt 3 notitle'

           end if !indxx = 0, long spectrum
          
          end if !alternative labelling, long spectrum

          end if

         end if !long spectrum

!       ----------------------------------------------------------------
        close(50)

        end subroutine create_plotfile_ps_vel
!       ----------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Numerical Recipes
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       RANDOM-Numbers (uniformily distributed 1)
!       ------------------------------------------------------------------------

!       Minimal random number generator of Park and Miller with Bays-Durham shuffle and added
!       safeguards. Returns a uniform random deviate between 0.0 and 1.0 (exclusive of the endpoint
!       values). Call with idum a negative integer to initialize; thereafter, do not alter idum
!       between successive deviates in a sequence. RNMX should approximate the largest floating
!       value that is less than 1.
  
        FUNCTION ran1(idum)
        INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
        double precision :: ran1,AM,EPS,RNMX
        PARAMETER (IA=16807,IM=2147483647,AM=1.0d0/IM,IQ=127773,IR=2836,
     !  NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2d-7,RNMX=1.0d0-EPS)
        INTEGER j,k,iv(NTAB),iy
        SAVE iv,iy
        DATA iv /NTAB*0/, iy /0/
        if (idum.le.0.or.iy.eq.0) then
         idum=max(-idum,1)
         do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11         continue
         iy=iv(1)
        endif
        k=idum/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum=idum+IM
        j=1+iy/NDIV
        iy=iv(j)
        iv(j)=idum
        ran1=min(AM*iy,RNMX)
        return
        END FUNCTION
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.

!       ------------------------------------------------------------------------
!       RANDOM-Numbers (normal distributed)
!       ------------------------------------------------------------------------
!       Returns a normally distributed deviate with zero mean and unit variance, using
!       ran1(idum) as the source of uniform deviates.

        FUNCTION gasdev(idum)
        INTEGER idum
        double precision :: gasdev
CU      USES ran1
        INTEGER iset
        double precision :: fac,gset,rsq,v1,v2,ran1
        SAVE iset,gset
        DATA iset/0/
        if (iset.eq.0) then
1         v1=2.0d0*ran1(idum)-1.0d0
         v2=2.0d0*ran1(idum)-1.0d0
         rsq=v1**2+v2**2
         if(rsq.ge.1.0d0.or.rsq.eq.0.0d0) goto 1
         fac=sqrt((-2.0d0)*log(rsq)/rsq)
         gset=v1*fac
         gasdev=v2*fac
         iset=1
        else
         gasdev=gset
         iset=0
        endif
        return
        END FUNCTION
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
