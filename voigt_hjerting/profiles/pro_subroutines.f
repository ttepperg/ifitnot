!       profiles_hdf5: Subroutines and Functions
        
!       ========================================================================
        subroutine load_ion(ionname)

!       ----------------------------------------------------------------
!       BEWARE: the atomic data thus loaded might not be consistent with
!       abs_line_profile and ifitnot!

!       ------------------------------------------------------------------------

        use common_nums, only: wl_central, fvalue, gamma
        use common_strings, only: errmsg

        implicit none

        character(len=*), intent(in) :: ionname

!       ------------------------------------------------------------------------
        select case(ionname)

!       ------------------------------------------------------------------------
!       parameters for neutral hydrogen atom (HI); Lyman-Alpha ONLY (for now!)
         case('h1')

          wl_central = 1215.6701
          fvalue = 0.416400
          gamma = 6.26500e+08

!       ------------------------------------------------------------------------
!       parameters for five-times ionised oxygen atom (OVI);
!       strongest transition ONLY (for now!)
         case('o6')

          wl_central = 1031.9261
          fvalue = 0.132541
          gamma = 4.10000e+08

         case default
          write(6,*) trim(errmsg)
          write(6,*) 'Non-valid ion: '//trim(ionname)
          stop 1

        end select
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine load_ion
!       ========================================================================

!       ========================================================================
        function voigt_hjerting(a,x)

!       Returns the value of the Voigt-Hjerting function H(a,x) for a given
!       damping parameter a = Gamma / 4 pi Delta_nu, and reduced wavelength
!       x = (lambda - lambda0)/ Delta_lambda, with:
!
!       Gamma =  natural line width
!       (Delta_nu / nu0) = (Delta_lambda / lambda0) = (b / c)
!       b -> Doppler parameter (in km/s)
!       lambda0 -> central wavelength (in Angstroem) of the corresponding
!       transition.
!
!       Algorithm is from Tepper-Garcia, Thorsten 2006
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025
        
        use set_precision
        use physical, only: pi

        implicit none

        real(kind=doubleR) :: voigt_hjerting

        real(kind=doubleR) :: a
        real(kind=doubleR) :: x, x2, h0, Q, P
        
!       Check numerical precision
!       See ~/voigt_hjerting/code/precision (dir)

        if (dabs(x).gt.4.0d-4) then ! numerically stable

         x2 = x*x

!       avoid underflow of exp(-x*x)
         if (dabs(x).lt.(dsqrt(-dlog(dble(tiny(real_single)))))) then
          h0 = dexp(-1.0d0*x2)
         else
          h0 = tiny(real_single)
         end if
        
!       avoid over/underflow of Q
         if ((dlog(x2) + dlog(dble(tiny(real_single)))).gt.dlog(1.5d0)) then
          Q = tiny(real_single)
         else
          Q = 1.5d0/x2
         end if
        
!       avoid over/underflow of P
         if ((dlog(x2) + dlog(dble(tiny(real_single)))).gt.
     &   (dlog(a)-dlog(dsqrt(pi)))) then
          P = tiny(real_single)
         else
          P = a/dsqrt(pi)/x2
         end if
        
         voigt_hjerting = h0 - P*
     &   (h0*h0*(4.0d0*x2*x2 + 7.0d0*x2 + 4.0d0 + Q) - 1.0d0 - Q)

        else                         !limit for x -> 0

         voigt_hjerting = 1.0d0 - 2.0d0*a/dsqrt(pi)

        endif
        
!       ------------------------------------------------------------------------
        return

        end function voigt_hjerting
!       ========================================================================

!       ========================================================================
        function gauss(x)

!       Returns the value of the Gaussian function as a function of
!       wavelength for a given Doppler parameter (in km/s), and central
!       wavelength (in Angstroem) of the corresponding transition.
        
        use set_precision
        
        implicit none

        real(kind=doubleR) :: gauss

        real(kind=doubleR) :: x

!       avoid underflow of exp(-x*x)
        if (dabs(x).lt.(dsqrt(-1.0d0*dlog(dble(tiny(real_single)))))) then
         gauss = dexp(-1.0d0*x*x)
        else
         gauss = tiny(real_single)
        end if

!       ------------------------------------------------------------------------
        return

        end function gauss
!       ========================================================================

!       ----------------------------------------------------------------
        function size_of_file(filename)

!       Returns the number of lines in file 'filename'

        use common_strings

        implicit none

        integer size_of_file, lines
        character(len=256) filename

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
        
        end function
!       ----------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Taken (!) from SpecWizard

        subroutine convolve_gaussian(flx,num_px,px_size,fwhm)

!       USES convlv

!       flx = signal
!       num_px = number of pixels
!       px_size = size of pixel in km/s
!       fwhm_kms = FWHM of Gaussian response in km/s

        use set_precision

        implicit none

        real(kind=doubleR) :: sigmakms, b, norm, fwhm, px_size
        integer(kind=singleI) :: i,j,off, nvpix, num_px
        real(kind=doubleR), allocatable, save :: gauss(:)
        real(kind=doubleR) :: flx(*)
        real(kind=doubleR), allocatable  :: convl_flux(:),
     &  convl_flux_convolved(:)

!       write(*,*) 'Convolving with Gaussian PSF using fwhm = ', fwhm
!       write(*,*)

!       determine number of pixels
        nvpix = num_px

!       compute Gaussian width of convolution kernel (in km/s)
        sigmakms = fwhm / (2.0d0 * dsqrt( 2.0d0 * dlog(2.0d0)))

!       express Gaussian width (in pixels)
        b = sigmakms / px_size
        
!       For convolution with instrumental PSF we need to Fourier 
!       transform, we thus need to increase the array so that it is a
!       power of 2.
        nvpix = int(2**(aint(dlog(dble(nvpix))/log(2.)) + 1))
        
!       Create normalized Gaussian in wrap-around order (required by
!       convlv)
        
        if (allocated(gauss)) deallocate(gauss)
        allocate(gauss(nvpix))

        norm = 1.d0 / (2.d0 * b * b)
        do i = 0, nvpix-1
        if (i .le. nvpix-1) then 
        if (i .le. nvpix/2) then
           j = i
        else
           j = i - nvpix
        endif
        if (abs(j) .lt. 10.*b) then
           gauss(i+1) = exp(-(dble(j)**2.)*norm)
        else
           gauss(i+1) = 0.d0
        endif
        else
        gauss(i+1) = 0.d0
        endif
        enddo

!       normalise Gaussian
        gauss  = gauss / sum(gauss)

        allocate(convl_flux(nvpix),convl_flux_convolved(2*nvpix))
        convl_flux(:) = 0.0d0
        convl_flux(1:num_px) = flx(1:num_px)

!       copy periodic copies of the flux signal into the zero buffer
!       to avoid aliasing (or end) effects
        do i=num_px+1,nvpix
        off = i-num_px
        if (off .lt. (nvpix-num_px)/2.) then
        convl_flux(i) = convl_flux(i-num_px)
        else
        convl_flux(i) = convl_flux(i-(nvpix-num_px))
        endif
        enddo
        convl_flux_convolved(:) = 0.0

        call convlv(convl_flux,nvpix,gauss,nvpix,1,convl_flux_convolved)

        flx(1:num_px) = convl_flux_convolved(1:num_px)

        deallocate(convl_flux,convl_flux_convolved)

        return
        
        end subroutine convolve_gaussian
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Taken (!) from SpecWizard

!       NOTE:
!       ans is complex here, but it is real when routine is called by
!       convolve_gaussian
        
        subroutine convlv(data,n,respns,m,isign,ans)

!       Convolves or deconvolves a real data set data(1:n) (including any
!       user-supplied zero padding) with a response function respns, stored in
!       wrap-around order in a real array of length m <=n. (m should be an odd
!       integer.) Wrap-around order means that the first half of the array
!       respns contains the impulse response function at positive times, while
!       the second half of the array contains the impulse response function at
!       negative times, counting down from the highest element respns(m). On
!       input isign is +1 for convolution, -1 for deconvolution. The answer is
!       returned in the first n components of ans. However, ans must be supplied
!       in the calling program with length at least 2*n, for consistency with
!       twofft. n MUST be an integer power of two.

!       USES twofft, realft

        use fourier ! declares complex array fft
        use set_precision

        implicit none 

        integer(kind=singleI), intent(in) :: n, m, isign
        real(kind=doubleR), intent(in)    :: data(n)
        real(kind=doubleR), intent(inout) :: respns(n)
!        double complex, intent(out):: ans(2*n)
        complex(kind=doubleR), intent(out):: ans(2*n)

        integer(kind=singleI) :: i,no2
        integer(kind=singleI), save :: nfft=-1

        if(nfft .ne. n)then
         if (allocated(fft)) deallocate(fft)
         allocate(fft(n))
         nfft = n
        endif

        do  i=1,(m-1)/2
         respns(n+1-i)=respns(m+1-i)
        enddo
        do  i=(m+3)/2,n-(m-1)/2
         respns(i)=0.0
        enddo

        call twofft(data,respns,fft,ans,n) ! discrete fourier transforms:
                                           ! data -> fft
                                           ! respns -> ans
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
        call realft(ans,n,-1)              ! inverse fourier transforms ans

        return
        end subroutine convlv
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Taken (!) from SpecWizard

        subroutine twofft(data1,data2,fft1,fft2,n)

!       Given two real input arrays data1(1:n) and data2(1:n), this routine
!       calls four1 and returns two complex output arrays, fft1(1:n) and
!       fft2(1:n), each of complex length n (i.e., real length 2*n), which
!       contain the discrete Fourier transforms of the respective data arrays.
!       n MUST be an integer power of 2.

!       USES four1

        use set_precision

        implicit none 

        integer(kind=singleI), intent(in) :: n
        real(kind=doubleR), intent(in) :: data1(n), data2(n)
!        double complex :: fft1(n), fft2(n)
        complex(kind=doubleR) :: fft1(n), fft2(n)

        ! local variables
        integer(kind=singleI) :: j,n2
!        double complex ::  h1,h2,c1,c2
        complex(kind=doubleR) ::  h1,h2,c1,c2

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

!       ------------------------------------------------------------------------
!       Taken (!) from SpecWizard
!       USES four1

        subroutine realft(data,n,isign)

!       Calculates the Fourier transform of a set of n real-valued data points.
!       Replaces this data (which is stored in array data(1:n)) by the positive
!       frequency half of its complex Fourier transform. The real-valued first
!       and last components of the complex transform are returned as elements
!       data(1) and data(2), respectively. n must be a power of 2. This routine
!       also calculates the inverse transform of a complex data array if it is
!       the transform of real data. (Result in this case must be multiplied by
!       2/n.)

        use set_precision

        implicit none

        integer(kind=singleI), intent(in) :: n, isign
        real(kind=doubleR), intent(inout) :: data(n)
        ! local variables
        integer(kind=singleI) :: i,i1,i2,i3,i4,n2p3
        real(kind=doubleR) c1,c2,h1i,h1r,h2i,h2r,wis,wrs
        real(kind=doubleR) theta,wi,wpi,wpr,wr,wtemp
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

!       ------------------------------------------------------------------------
!       Taken (!) from SpecWizard

        subroutine four1(data,nn,isign)

!       Replaces data(1:2*nn) by its discrete Fourier transform, if isign is
!       input as 1; or replaces data(1:2*nn) by nn times its inverse discrete
!       Fourier transform, if isign is input as  -1. data is a complex array of
!       length nn or, equivalently, a real array of length 2*nn. nn MUST be an
!       integer power of 2 (this is not checked for!). 

!       NOTE:
!       data is real here, but it is complex when routine is called by twofft
!       gfortran-mo-4.7 compiler does not accept this (in contrast to previous
!       compiler versions)

        use set_precision

        implicit none

        integer(kind=singleI), intent(in) :: nn, isign
        real(kind=doubleR), intent(inout) :: data(2*nn)
        ! local variables  
        integer(kind=singleI) :: i,istep,j,m,mmax,n
        real(kind=doubleR) :: tempi,tempr
        real(kind=doubleR) ::  theta,wi,wpi,wpr,wr,wtemp

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

!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
!       Numerical Recipes: Functions
!       ----------------------------------------------------------------

!         ----------------------------------------------------------------
!         Create an index in ascending order for a given array
!         ----------------------------------------------------------------
        SUBROUTINE indexx(n,arr,indx)
          INTEGER n,indx(n),M,NSTACK
          double precision arr(n)
          PARAMETER (M=7,NSTACK=50)
          INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
          double precision a
          do 11 j=1,n
         indx(j)=j
11        continue
          jstack=0
          l=1
          ir=n
1        if(ir-l.lt.M)then
         do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13       continue
         if(jstack.eq.0)return
         ir=istack(jstack)
         l=istack(jstack-1)
         jstack=jstack-2
          else
         k=(l+ir)/2
         itemp=indx(k)
         indx(k)=indx(l+1)
         indx(l+1)=itemp
         if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
         endif
         if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
         endif
         if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
         endif
         i=l+1
         j=ir
         indxt=indx(l)
         a=arr(indxt)
3        continue
          i=i+1
         if(arr(indx(i)).lt.a)goto 3
4        continue
          j=j-1
         if(arr(indx(j)).gt.a)goto 4
         if(j.lt.i)goto 5
         itemp=indx(i)
         indx(i)=indx(j)
         indx(j)=itemp
         goto 3
5        indx(l)=indx(j)
         indx(j)=indxt
         jstack=jstack+2
         if(jstack.gt.NSTACK)print*, 'NSTACK too small in indexx'
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
