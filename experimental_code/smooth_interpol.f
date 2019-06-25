!       ========================================================================
        program smooth_interpol

!       - reads in a spectrum (flux vs. wavelength) and:
!
!       1) smooths the flux using a Savitzky-Golay filter of order m=4 and full
!         width equal to 2x the *input* instrumental FHWM
!
!       2) interpolates both the input and smoothed flux to a given (arbitrary)
!       pixel size** across the entire wavelength range;
!       interpolation is ignored if desired pixel size is set to 0
!       
!       ** note that *constant* velocity bins are computed using:
!
!       dv/c = dlambda / lambda_0
!
!       with dlambda = lambda(2) - lambda(1),
!       and lambda_0 = 1216.67 Angstroem (HI Lyman-Alpha transition)
!
!       INPUT: spectrum ascii file***, resolution (FWHM), pixel size (ang)
!
!       *** spectrum ascii file in the form:
!       
!       wavelength | flux | noise
       
!
!       OUTPUT:
!       1) smoothed spectrum in the form:
!       
!       wavelength | velocity | flux | smoothed flux
!
!       2) interpolated input / smoothed spectrum in the form:
!       
!       wavelength | velocity | interp. input flux | interp. smoothed flux

!       ------------------------------------------------------------------------
!       GLOBAL variables

        implicit none

        integer :: pixel
        integer :: argcount
        integer :: size_of_file, num_pixel, num_pixel_int

        double precision, parameter :: lyalpha_Ang=1215.6701d0
        double precision, parameter :: clight_kms=2.9979d+5
        double precision :: fwhm_kms, pixel_size_kms, pixel_size_Ang
        double precision,allocatable :: wave(:), flux(:), sigma(:),
     &  flux_smooth(:)
        double precision,allocatable :: d2flux_dw2(:), wave_int(:),
     &  flux_int(:), flux_smooth_int(:)

        character(len=12) :: dummy_str
        character(len=256) :: specfile
        character(len=256) :: statmsg, warnmsg, errmsg

!       ------------------------------------------------------------------------
!       define program error and warning messages
!       the lines below are meant to print the different
!       messages in different colours

!       In red=31
        errmsg=CHAR(27)//'[1;31mERROR: smooth_interpol ...'//
     &  CHAR(27)//'[0m'
!       In yellow=33
        warnmsg=CHAR(27)//'[1;33mWARNING: smooth_interpol ...'//
     &  CHAR(27)//'[0m'
!       In green=32
        statmsg=CHAR(27)//'[1;32mSTATUS: smooth_interpol ...'//
     &  CHAR(27)//'[0m'

!       ------------------------------------------------------------------------
!       get arguments

        argcount = IArgC()
        if (argcount.ne.3) then
         write(6,*) trim(warnmsg)//'USAGE: smooth_interpol <specfile> '//
     &   '<fwhm (km/s)> <pixel size (Ang)>'
         stop 1
        end if

!       ------------------------------------------------------------------------
!       get the input data: filename containing flux / error vs. wavelength
        call getarg(1,specfile)

        call getarg(2,dummy_str)
        read (dummy_str,'(F5.2)') fwhm_kms

        call getarg(3,dummy_str)
        read (dummy_str,'(F6.4)') pixel_size_Ang

!       get number of pixels (existence of file is checked implicitly)
        num_pixel = size_of_file(specfile)

!       allocate space
        allocate(wave(1:num_pixel))
        allocate(flux(1:num_pixel))
        allocate(sigma(1:num_pixel))
        allocate(flux_smooth(1:num_pixel))
        allocate(d2flux_dw2(1:num_pixel)) !second derivatives for interpolation

!       initialise
        wave(:) = 0.
        flux(:) = 0.
        sigma(:) = 0.
        flux_smooth(:) = 0.
        d2flux_dw2(:) = 0.

!       read spectrum (flux / noise vs. wavelength)
        open(10,file=trim(specfile))
         
         do pixel=1,num_pixel
          read(10,*) wave(pixel), flux(pixel), sigma(pixel)
         end do

        close(10)

!       define (average) pixel size in km/s (relative to HI Lyman-Alpha)
        
        do pixel=1,num_pixel-1
         pixel_size_kms = pixel_size_kms +
     &   ((wave(pixel+1) - wave(pixel)) / wave(pixel))
        end do
        pixel_size_kms = (pixel_size_kms / dble(num_pixel - 1)) * clight_kms

!       output info
        print*, 'average resolution (R): ', clight_kms / pixel_size_kms
        print*, 'average pixel size (km/s): ', pixel_size_kms

!       ------------------------------------------------------------------------
!       smooth spectrum
!       ------------------------------------------------------------------------
!       last parameter sets padding value
        call
     &  smooth_sg(flux_smooth,flux,1,num_pixel,pixel_size_kms,fwhm_kms,1.0d0)

!       output (smooth) spectrum; 'smooth' is appended to original file name

        open(10,file=specfile(1:scan(specfile,'.',back=.true.)-1)//'_smooth'//
     &  specfile(scan(specfile,'.',back=.true.):len(trim(specfile))))
         
         do pixel=1,num_pixel
          write(10,*) wave(pixel), pixel_size_kms*(pixel - 1),
     &    flux(pixel), flux_smooth(pixel), sigma(pixel)
         end do

        close(10)

!       ------------------------------------------------------------------------
!       interpolation
!       ------------------------------------------------------------------------
!       only if required, i.e. input pixel_size_Ang > 0
        if (pixel_size_Ang.le.0.0d0) stop

!       calculate number of pixels for new pixel size
        
        num_pixel_int = int((maxval(wave) - minval(wave))/pixel_size_Ang)

!       define new pixel size in km/s (relative to HI Lyman-Alpha)

        pixel_size_kms = (pixel_size_Ang / lyalpha_Ang) * (clight_kms)

!       allocate space
        allocate(wave_int(1:num_pixel_int))
        allocate(flux_int(1:num_pixel_int))
        allocate(flux_smooth_int(1:num_pixel_int))

!       initialise
        wave_int(:) = 0.
        flux_int(:) = 0.
        flux_smooth_int(:) = 0.

!       ------------------------------------------------------------------------
!       interpolate *original* flux

!       compute second-derivative of *original* flux at each wavelength
        call spline(wave,flux,num_pixel,1.d31,1.d31,d2flux_dw2)

        do pixel=1,num_pixel_int        

!       compute new wavelength point
         wave_int(pixel) = wave(1) + (pixel - 1)*pixel_size_Ang

!       interpolate flux at new wavelength
         call splint(wave,flux,d2flux_dw2,num_pixel,wave_int(pixel),
     &        flux_int(pixel))

        end do ! over pixels

!       ------------------------------------------------------------------------
!       interpolate *smoothed* flux

!       re-initialise array with second derivatives
        d2flux_dw2(:) = 0.

!       compute second-derivative of *smoothed* flux at each wavelength
        call spline(wave,flux_smooth,num_pixel,1.d31,1.d31,d2flux_dw2)

        do pixel=1,num_pixel_int        

!       interpolate flux at new wavelength
         call splint(wave,flux_smooth,d2flux_dw2,num_pixel,wave_int(pixel),
     &        flux_smooth_int(pixel))
              
        end do ! over pixels

!       ------------------------------------------------------------------------
!       output interpolated input and smoothed fluxes

        open(10,file=specfile(1:len(trim(specfile))-4)//'_smooth_interpol'//
     &        specfile(len(trim(specfile))-3:len(trim(specfile))))
         
         do pixel=1,num_pixel_int
          write(10,*) wave_int(pixel), pixel_size_kms*(pixel - 1),
     &    flux_int(pixel), flux_smooth_int(pixel), sigma(pixel)
         end do

        close(10)


!       ========================================================================
        end program smooth_interpol
!       ========================================================================

!       ========================================================================
!       Subroutines and Functions
!       ========================================================================

!       ========================================================================
        function size_of_file(filename)

!        Returns the number of lines in file 'filename'

        implicit none

        integer :: size_of_file, lines
        character(len=256) :: filename
        
        logical :: file_exists = .false.
!       ------------------------------------------------------------------------
!       check for existence

        inquire(file=trim(filename),exist=file_exists)

        if (.not.file_exists) then
         write(6,*) 'File '//trim(filename)//' does not exist!'
         stop 1
        end if

!        Open file to get dimension
         
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
          write(6,*) 'Empty file: ', trim(filename)
         end if

!       ------------------------------------------------------------------------
        return
        
        end function size_of_file
!       ========================================================================

!       ========================================================================
        subroutine
     &  smooth_sg(arr_out,arr_in,min_px,max_px,px_size_kms,fwhm_kms,padding)

!       smooths a spectrum by convolution with a Savitzky-Golay filter
!       using a polynomial of order m = 4, a derivative of order n = 0,
!       and a window with a full width equal to 2x the instrumental FWHM (in
!       pixel) 

!       IMPORTANT: strictly, SG filtering should only be applied to
!       equally spaced data points!

        implicit none

        integer :: i, j, min_px, max_px, size
        integer :: run_indx
        integer :: nl, nr !points to the left/right wrt point
        integer :: order_poly, order_der
        double precision :: signal, fwhm_kms, px_size_kms, padding
        double precision, allocatable :: coeff(:)
        double precision :: arr_out(min_px:max_px)
        double precision :: arr_in(min_px:max_px)

!       ------------------------------------------------------------------------
        order_poly = 4
        order_der = 0
        nl = max(4,ceiling(fwhm_kms/px_size_kms))
        nr = max(4,ceiling(fwhm_kms/px_size_kms))
        size = nl + nr + 1
        
        arr_out(min_px:max_px) = 0.0d0

        if (allocated(coeff)) deallocate(coeff)
        allocate(coeff(1:size))
        coeff(1:size) = 0.0d0
        signal = 0.0d0

        call savgol(coeff,size,nl,nr,order_der,order_poly)

!       sg output is in wrap-around order; cyclically shift array to
!       get entries in 'right' order

        coeff(1:size) = cshift(coeff(1:size),-nr)

!       do i=min_px, max_px
!        do j=size, 1, -1
!         run_indx = nr + 1 - j
!         print*, i, i+run_indx, run_indx, coeff(j)
!        end do
!         print*
!       end do

!       convolve data with filter;
!       note that filter response is returned in wrap-around order
        do i=min_px, max_px

         do j=size, 1, -1
          
          run_indx = i + nr + 1 - j

!       the next lines takes care of boundary conditions
!       padding done with input value

          if (run_indx.lt.min_px) then
           signal = padding
          else if (run_indx.gt.max_px) then
           signal = padding
          else
           signal = arr_in(run_indx)
          endif

          arr_out(i) = arr_out(i) + coeff(j)*signal
         
         end do
         
        end do

!       ------------------------------------------------------------------------
        return

        end subroutine smooth_sg
!       ========================================================================

!       ------------------------------------------------------------------------
!        Numerical Recipes
!       ------------------------------------------------------------------------

!       ========================================================================
        SUBROUTINE savgol(c,np,nl,nr,ld,m)
        integer :: ld,m,nl,np,nr,MMAX
        double precision ::c(np)
        PARAMETER (MMAX=6)
CU    USES lubksb,ludcmp
        integer :: imj,ipj,j,k,kk,mm,indx(MMAX+1)
        double precision ::d,fac,sum,a(MMAX+1,MMAX+1),b(MMAX+1)
        if(np.lt.nl+nr+
     *        1.or.nl.lt.0.or.nr.lt.0.or.ld.gt.m.or.m.gt.MMAX.or.nl+nr.lt.m) 
     *        write(6,*) 'bad args in savgol'
        do 14 ipj=0,2*m
          sum=0.
          if(ipj.eq.0)sum=1.
          do 11 k=1,nr
            sum=sum+float(k)**ipj
11        continue
          do 12 k=1,nl
            sum=sum+float(-k)**ipj
12        continue
          mm=min(ipj,2*m-ipj)
          do 13 imj=-mm,mm,2
            a(1+(ipj+imj)/2,1+(ipj-imj)/2)=sum
13        continue
14        continue
        call ludcmp(a,m+1,MMAX+1,indx,d)
        do 15 j=1,m+1
          b(j)=0.
15        continue
        b(ld+1)=1.
        call lubksb(a,m+1,MMAX+1,indx,b)
        do 16 kk=1,np
          c(kk)=0.
16        continue
        do 18 k=-nl,nr
          sum=b(1)
          fac=1.
          do 17 mm=1,m
            fac=fac*k
            sum=sum+b(mm+1)*fac
17        continue
          kk=mod(np-k,np)+1
          c(kk)=sum
18        continue
        return
        END

!       ========================================================================

!       ========================================================================
        SUBROUTINE lubksb(a,n,np,indx,b)
        integer :: n,np,indx(n)
        double precision ::a(np,np),b(n)
        integer :: i,ii,j,ll
        double precision ::sum
        ii=0
        do 12 i=1,n
          ll=indx(i)
          sum=b(ll)
          b(ll)=b(i)
          if (ii.ne.0)then
            do 11 j=ii,i-1
                sum=sum-a(i,j)*b(j)
11          continue
          else if (sum.ne.0.) then
            ii=i
          endif
          b(i)=sum
12        continue
        do 14 i=n,1,-1
          sum=b(i)
          do 13 j=i+1,n
            sum=sum-a(i,j)*b(j)
13        continue
          b(i)=sum/a(i,i)
14        continue
        return
        END

!       ========================================================================

!       ========================================================================
        SUBROUTINE ludcmp(a,n,np,indx,d)
        integer :: n,np,indx(n)

        double precision ::d,a(np,np)
        double precision, parameter :: TINY=1.0d-20
        integer :: i,imax,j,k
        double precision ::aamax,dum,sum,vv(n)
        d=1.0d0
        imax=0
        do 12 i=1,n
          aamax=0.
          do 11 j=1,n
            if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11        continue
          if (aamax.eq.0.) write(6,*) 'singular matrix in ludcmp'
          vv(i)=1./aamax
12        continue
        do 19 j=1,n
          do 14 i=1,j-1
            sum=a(i,j)
            do 13 k=1,i-1
                sum=sum-a(i,k)*a(k,j)
13          continue
            a(i,j)=sum
14        continue
          aamax=0.
          do 16 i=j,n
            sum=a(i,j)
            do 15 k=1,j-1
                sum=sum-a(i,k)*a(k,j)
15          continue
            a(i,j)=sum
            dum=vv(i)*abs(sum)
            if (dum.ge.aamax) then
                imax=i
                aamax=dum
            endif
16        continue
          if (j.ne.imax)then
            do 17 k=1,n
                dum=a(imax,k)
                a(imax,k)=a(j,k)
                a(j,k)=dum
17          continue
            d=-d
            vv(imax)=vv(j)
          endif
          indx(j)=imax
          if(a(j,j).eq.0.)a(j,j)=TINY
          if(j.ne.n)then
            dum=1./a(j,j)
            do 18 i=j+1,n
                a(i,j)=a(i,j)*dum
18          continue
          endif
19        continue
        return
        END
!       ========================================================================

!       ========================================================================
        SUBROUTINE spline(x,y,n,yp1,ypn,y2)

!       Given arrays x(1:n) and y(1:n) containing a tabulated function, i.e.,
!       yi = f(xi), with x1 < x2 < .. . < xN, and given values yp1 and ypn for
!       the first derivative of the interpolating function at points 1 and n,
!       respectively, this routine returns an array y2(1:n) of length n which
!       contains the second derivatives of the interpolating function at the
!       tabulated points xi. If yp1 and/or ypn are equal to 1e30 or larger,
!       the routine is signaled to set the corresponding boundary condition for
!       a natural spline, with zero second derivative on that boundary.

!       IMPORTANT: the program spline is called only once to process an entire
!       tabulated function in arrays xi and yi

        integer :: n
        double precision :: yp1,ypn,x(n),y(n),y2(n)
        integer :: i,k
!       double precision :: p,qn,sig,un,u(n)
        double precision :: p,qn,sig,un,u(n)
        if (yp1.gt..99d30) then
          y2(1)=0.
          u(1)=0.
        else
          y2(1)=-0.5
          u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
        endif
        do 11 i=2,n-1
          sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
          p=sig*y2(i-1)+2.
          y2(i)=(sig-1.)/p
          u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *          1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *          u(i-1))/p
  11    continue
        if (ypn.gt..99e30) then
          qn=0.
          un=0.
        else
          qn=0.5
          un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
        endif
        y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
        do 12 k=n-1,1,-1
          y2(k)=y2(k)*y2(k+1)+u(k)
  12    continue
        return
        END
!       ========================================================================

!       ========================================================================
        SUBROUTINE splint(xa,ya,y2a,n,x,y)

!       Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a
!       function (with the xa_i in order), and given the array y2a(1:n), which
!       is the output from spline above, and given a value of x, this routine
!       returns a cubic-spline interpolated value y.
        
        integer :: n
        double precision :: x,y,xa(n),y2a(n),ya(n)
        integer :: k,khi,klo
        double precision :: a,b,h
        klo=1
        khi=n
  1     if (khi-klo.gt.1) then
          k=(khi+klo)/2
          if(xa(k).gt.x)then
            khi=k
          else
            klo=k
          endif
        goto 1
        endif
        h=xa(khi)-xa(klo)
        if (h.eq.0.) write(6,*) 'bad xa input in splint'
        a=(xa(khi)-x)/h
        b=(x-xa(klo))/h
        y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *        2)/6.
        return
        END
!       ========================================================================

