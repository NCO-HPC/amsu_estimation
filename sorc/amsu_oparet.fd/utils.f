      subroutine jdayi(julday,iyear,imon,iday)
c     This routine calculates the month (imon) and day (iday)
c     from the Julian day (julday) and year (iyear).
c     The appropriate correction is made for leap year.
c
      dimension ndmon(12),nsum(13)
c
c     Specify the number of days in each month
      ndmon(1)  = 31
      ndmon(2)  = 28
      ndmon(3)  = 31
      ndmon(4)  = 30
      ndmon(5)  = 31
      ndmon(6)  = 30
      ndmon(7)  = 31
      ndmon(8)  = 31
      ndmon(9)  = 30
      ndmon(10) = 31
      ndmon(11) = 30
      ndmon(12) = 31
c
c     Correct for leap year
      if (mod(iyear,4) .eq. 0) ndmon(2)=29
c
c     Check for illegal input
      if (mod(iyear,4) .eq. 0) then
         mxjul = 366
      else
         mxjul = 365
      endif
c
      if (julday .lt. 1 .or. julday .gt. mxjul) then
         imon = -1
         iday = -1
         return
      endif
c
c     Calculate the month and day
      nsum(1) = 0
      do 10 i=1,12
         nsum(i+1) = nsum(i) + ndmon(i)
   10 continue
c
      do 20 i=2,13
         if (julday .le. nsum(i)) then
            imon = i-1
            go to 1000
         endif
   20 continue
 1000 continue
c
      iday = julday - nsum(imon)
c
      return
      end
      subroutine jday(imon,iday,iyear,julday)
c     This routine calculates the Julian day (julday) from
c     the month (imon), day (iday), and year (iyear). The
c     appropriate correction is made for leap year.
c
      dimension ndmon(12)
c
c     Specify the number of days in each month
      ndmon(1)  = 31
      ndmon(2)  = 28
      ndmon(3)  = 31
      ndmon(4)  = 30
      ndmon(5)  = 31
      ndmon(6)  = 30
      ndmon(7)  = 31
      ndmon(8)  = 31
      ndmon(9)  = 30
      ndmon(10) = 31
      ndmon(11) = 30
      ndmon(12) = 31
c
c     Correct for leap year
      if (mod(iyear,4) .eq. 0) ndmon(2)=29
c
c     Check for illegal input
      if (imon .lt. 1 .or. imon .gt. 12) then
         julday=-1
         return
      endif
c
      if (iday .lt. 1 .or. iday .gt. ndmon(imon)) then
         julday=-1
         return
      endif
c
c     Calculate the Julian day
      julday = iday
      if (imon .gt. 1) then
         do 10 i=2,imon
            julday = julday + ndmon(i-1)
   10    continue
      endif
c
      return
      end
      subroutine llintsp(f1,slon1,slat1,dlon1,dlat1,im1,jm1,i1,j1,
     +                  fsp,rlon,rlat,izp,ierr)
c
c     This routine linearly interpolates f1 on an evenly spaced
c     lat,lon grid to obtain fsp at the single point (rlon,rlat). 
c     The subroutine arguments are defined as follows:
c
c     f1(im1,jm1):  Contains the function to be interpolated. The
c                   first index represents longitude (increasing
c                   eastward) and the second index represents 
c                   latitude (increasing northward).
c
c     fsp:          The interpolated value of f1.
c
c     slon1         The first longitude of f1 (deg E positive)
c     slat1         The first latitude  of f1 (deg N positive)
c     dlon1         The longitude increment of f1 (deg)
c     dlat1         The latitude  increment of f1 (deg)
c     rlon,rlat     The longitude,latitude of the point to interpolate f1.
c
c     im1,jm1       The dimensions of f1
c
c     i1,j1         The number of longitude,latitude points of f1
c                   to use in the interpolation
c
c     izp           Zonal Periodic flag: 
c                       =1 when f1 is periodic in the zonal direction
c                          (normally used only when f1 spans 360 deg
c                          of longitude, starting at 0)
c                       =0 if not periodic
c
c     ierr          Error flag: =0 for normal return
c                               =1 if fsp is outside of the f1 domain
c                                  (non fatal)
c                               =2 if indices i1,j1 exceed
c                                  dimension of f1 (fatal) 
c                               =3 if dlon or dlat .le. 0.0 (fatal) 
c
      dimension f1(im1,jm1)
c
c     Initialize error flag
      ierr=0
c
c     Check indices and dlat,dlon
      if (i1 .gt. im1  .or.  j1 .gt. jm1) then  
          ierr=2
          return
      endif
c
      if (dlat1 .le. 0.0 .or. dlon1 .le. 0.0) then 
         ierr=3
         return
      endif
c
c     Specify needed constants
      pi   = 3.14159265
      dtr  = pi/180.0
      erad = 6371.0
      adtr = erad*dtr
c
c     Calculate min and max long,lat of f1 domain
      rlonn1 = slon1
      rlonx1 = slon1 + dlon1*float(i1-1)
      rlatn1 = slat1
      rlatx1 = slat1 + dlat1*float(j1-1)
c
c     Check if fsp point is outside of f1 domain.
c     If yes, move the fsp point to the nearest point in the
c     f1 domain and set error flag. 
c
      if (izp .ne. 1) then
c        Adjust fsp longitude for case without zonal periodicity
         if (rlon .gt. rlonx1) then
            rlon = rlonx1
            ierr=1
         endif
c
         if (rlon .lt. rlonn1) then
            rlon = rlonn1
            ierr=1
         endif
      else
c        Zonal periodic case
         if (rlon .ge. 360.0) rlon=rlon-360.0
      endif
c
      if (rlat .gt. rlatx1) then
         rlat = rlatx1
         ierr=1
      endif
c
      if (rlat .lt. rlatn1) then
         rlat = rlatn1
         ierr=1
      endif
c
c     Find the indices of the f1 point closest to,
c     but with lon,lat less than the fsp point.
      i00 = 1 + ifix( (rlon-rlonn1)/dlon1 )
      j00 = 1 + ifix( (rlat-rlatn1)/dlat1 )
      if (i00 .lt.    1)    i00=   1
      if (izp .ne. 1) then
         if (i00 .gt. i1-1) i00=i1-1
      endif
      if (j00 .lt.    1)    j00=   1
      if (j00 .gt. j1-1)    j00=j1-1
c
c     Define the four f1 values to be used in the linear
c     interpolation.
      f00 = f1(i00  ,j00  )
      f01 = f1(i00  ,j00+1)
c
      if (izp .eq. 1 .and. i00 .eq. i1) then
         f10 = f1(    1,j00  )
         f11 = f1(    1,j00+1)
      else
         f10 = f1(i00+1,j00  )
         f11 = f1(i00+1,j00+1)
      endif
c
c     Calculate the lon,lat of the point i00,j00
      rlon00 = rlonn1 + dlon1*float(i00-1)
      rlat00 = rlatn1 + dlat1*float(j00-1)
c
c     Calculate the x,y distances between the four f1 points
c     where x,y = 0,0 at i00,j00
      dx0 = dlon1*adtr*cos( dtr*(rlat00        ) )
      dx1 = dlon1*adtr*cos( dtr*(rlat00 + dlat1) )
      dy  = dlat1*adtr 
c
c     Calculate the x,y coordinates of the current f2 point
      x = adtr*(rlon-rlon00)*cos(dtr*rlat)
      y = adtr*(rlat-rlat00)
c
c     Calculate the coefficients for the linear interpolation
      a = f00
      b = (f10-f00)/dx0
      c = (f01-f00)/dy
      d = (f11 - f00 - b*dx1 - c*dy)/(dx1*dy)
c
c     Perform interpolation 
      fsp = a + b*x + c*y + d*x*y
c
      return
      end
      subroutine llintp(f1,slon1,slat1,dlon1,dlat1,im1,jm1,i1,j1,
     +                  f2,slon2,slat2,dlon2,dlat2,im2,jm2,i2,j2,
     +                  izp,ierr)
c
c     This routine linearly interpolates f1 on an evenly spaced
c     lat,lon grid to obtain f2 on a different lat,lon grid. 
c     The subroutine arguments are defined as follows:
c
c     f1(im1,jm1):  Contains the function to be interpolated. The
c                   first index represents longitude (increasing
c                   eastward) and the second index represents 
c                   latitude (increasing northward).
c
c     f2(im2,jm2):  The interpolated function with indices defined 
c                   the same as for f1.
c
c     slon1         The first longitude of f1 (deg E positive)
c     slat1         The first latitude  of f1 (deg N positive)
c     dlon1         The longitude increment of f1 (deg)
c     dlat1         The latitude  increment of f1 (deg)
c     slon2         The first longitude of f2 (deg E positive)
c     slat2         The first latitude  of f2 (deg N positive)
c     dlon2         The longitude increment of f2 (deg)
c     dlat2         The latitude  increment of f2 (deg)
c
c     im1,jm1       The dimensions of f1
c     im2,jm2       The dimensions of f2
c
c     i1,j1         The number of longitude,latitude points of f1
c                   to use in the interpolation
c     i2,j2         The number of longitude,latitude points of f2
c                   to interpolate
c
c     izp           Zonal Periodic flag: 
c                       =1 when f1 is periodic in the zonal direction
c                          (normally used only when f1 spans 360 deg
c                          of longitue)
c                       =0 if not periodic
c
c     ierr          Error flag: =0 for normal return
c                               =1 if f2 domain exceeds f1 domain
c                                  (non fatal)
c                               =2 if indices i1,j1 or i2,j2 exceed
c                                  dimension of f1 or f2 (fatal) 
c                               =3 if dlon or dlat .le. 0.0 (fatal) 
c
      dimension f1(im1,jm1),f2(im2,jm2)
c
c     Initialize error flag
      ierr=0
c
c     Check indices and dlat,dlon
      if (i1 .gt. im1  .or.  j1 .gt. jm1  .or. 
     +    i2 .gt. im2  .or.  j2 .gt. jm2)  then
          ierr=2
          return
      endif
c
      if (dlat1 .le. 0.0 .or. dlon1 .le. 0.0 .or.
     +    dlat2 .le. 0.0 .or. dlon2 .le. 0.0) then
         ierr=3
         return
      endif
c
c     Specify needed constants
      pi   = 3.14159265
      dtr  = pi/180.0
      erad = 6371.0
      adtr = erad*dtr
c
c     Calculate min and max long,lat of f1 domain
      rlonn1 = slon1
      rlonx1 = slon1 + dlon1*float(i1-1)
      rlatn1 = slat1
      rlatx1 = slat1 + dlat1*float(j1-1)
c
c     Start loop for f2 points
      do 10 j=1,j2
      do 10 i=1,i2
         rlon = slon2 + dlon2*float(i-1)
         rlat = slat2 + dlat2*float(j-1)
c
c        Check if current f2 point is outside of f1 domain.
c        If yes, move the f2 point to the nearest point in the
c        f1 domain and set error flag. 
c
         if (izp .ne. 1) then
c           Adjust f2 longitude for case without zonal periodicity
            if (rlon .gt. rlonx1) then
               rlon = rlonx1
               ierr=1
            endif
c
            if (rlon .lt. rlonn1) then
               rlon = rlonn1
               ierr=1
            endif
         else
c           Zonal periodic case
            if (rlon .ge. 360.0) rlon=rlon-360.0
         endif
c
         if (rlat .gt. rlatx1) then
            rlat = rlatx1
            ierr=1
         endif
c
         if (rlat .lt. rlatn1) then
            rlat = rlatn1
            ierr=1
         endif
c
c        Find the indices of the f1 point closest to,
c        but with lon,lat less than the current f2 point.
         i00 = 1 + ifix( (rlon-rlonn1)/dlon1 )
         j00 = 1 + ifix( (rlat-rlatn1)/dlat1 )
         if (i00 .lt.    1)    i00=   1
         if (izp .ne. 1) then
            if (i00 .gt. i1-1) i00=i1-1
         endif
         if (j00 .lt.    1)    j00=   1
         if (j00 .gt. j1-1)    j00=j1-1
c
c        Define the four f1 values to be used in the linear
c        interpolation.
         f00 = f1(i00  ,j00  )
         f01 = f1(i00  ,j00+1)
c
         if (izp .eq. 1 .and. i00 .eq. i1) then
            f10 = f1(    1,j00  )
            f11 = f1(    1,j00+1)
         else
            f10 = f1(i00+1,j00  )
            f11 = f1(i00+1,j00+1)
         endif
c
c        Calculate the lon,lat of the point i00,j00
         rlon00 = rlonn1 + dlon1*float(i00-1)
         rlat00 = rlatn1 + dlat1*float(j00-1)
c
c        Calculate the x,y distances between the four f1 points
c        where x,y = 0,0 at i00,j00
         dx0 = dlon1*adtr*cos( dtr*(rlat00        ) )
         dx1 = dlon1*adtr*cos( dtr*(rlat00 + dlat1) )
         dy  = dlat1*adtr 
c
c        Calculate the x,y coordinates of the current f2 point
         x = adtr*(rlon-rlon00)*cos(dtr*rlat)
         y = adtr*(rlat-rlat00)
c
c        Calculate the coefficients for the linear interpolation
         a = f00
         b = (f10-f00)/dx0
         c = (f01-f00)/dy
         d = (f11 - f00 - b*dx1 - c*dy)/(dx1*dy)
c
c        Perform interpolation and then go to the next f2 point
         f2(i,j) = a + b*x + c*y + d*x*y
   10 continue
c
      return
      end
      subroutine maxmin (par,i1,i2,parmax,parmin)
c     This routine finds the max and min of the one-dimensional
c     array par. If the max is equal to the min, then the max is set
c     to a value a little higher than the min.
c
      dimension par(*)
c
      parmax = -1.0e+10
      parmin =  1.0e+10
c
      do 10 m = i1,i2
         if (par(m) .gt. parmax) parmax = par(m)
         if (par(m) .lt. parmin) parmin = par(m)
   10 continue
c
      if (parmax .eq. parmin) parmax = parmin + 0.1*abs(parmin) + 1.0
c
      return
      end
      subroutine tstcod (par,i1,i2,parmax,parmin,bsub,smpy,cod)
c
      character code*2
      character*2 cod(*)
      dimension par(*)
c
      bsub = -parmin
      rix = 32**2 - 1
      rax = amax1 (parmax + bsub,0.)
      smpy = rax / rix
      scap = 1. / smpy
c
      do 10 m = i1,i2
         k = m - i1 + 1
         iz = nint ((par(m) + bsub) * scap)
c
         call encod (iz,code)
         cod(k) = code
   10 continue
c
      return
      end
      subroutine encod (iz,code)
c     hp version
c
      character*(*) code
      dimension idgt(2)
c
      ibase = 32
      idgt(2) = iz / ibase
      idgt(1) = iz - idgt(2) * ibase
c
      do 10 i = 1,2
         j = 3 - i
         if (idgt(i) .le. 9) then
            code(j:j) = char (idgt(i) + ichar ('0'))
         else
            code(j:j) = char (idgt(i) + (ichar ('A') - 10))
         end if
   10 continue
c
      return
      end
      integer function idecod (code)
c     hp version
c
      character*(*) code
      character dgtb*(31)
      parameter (dgtb = '123456789ABCDEFGHIJKLMNOPQRSTUV')
c
      idecod = index (dgtb,code(1:1)) * 32
     1  + index (dgtb,code(2:2))
c
      return
      end
      subroutine pstcal(z1,z2,t1,t2,p1,ps,ts)
c     This routine calculates the surface pressure (psfc) 
c     from thermodynamic variables near the surface.
c
c     level 1 = level closest to the surface (usually 1000 mb)
c     level 2 = next level up (usually 850 or 925 mb)
c
c     input: p1 = pressure (Pa) 
c            zi = geopotential height (m) (i=1,2)
c            ti = temperature (K)         (i=1,2)
c     
c     output: ps = surface pressure (Pa)
c             ts = surface temperature (K)
c
c     Specify physical constants (mks units)
      rd = 287.0
      g  = 9.81
c    
c     Calculate the lapse rate
      gamma = -(t2-t1)/(z2-z1)
c
      if (gamma .gt. 0.0) then
c        Assume constant lapse rate atmosphere
         ps = p1*( (t1/(t1-gamma*z1))**(g/(rd*gamma)) )
         ts = t1 + gamma*z1
      else
c        Assume isothermal atmosphere
         ps = p1*exp(g*z1/(rd*t1))
         ts = t1
      endif
c
      return
      end
      subroutine smooth (fld,temp,idim,jdim,idm,jdm,izp)
c -------------------------------------------------------------------
c --- smooths field in zonal and meridional directions 
c --- smoothing function from Shapiro (Shapiro, Ralph,1975: 
c --- Linear Filtering, Mathematics of Computation, Vol 29, No. 132,
c --- p. 1094-1097 
c --- 3 point filter
c --- 01/30/96  ---  Fiona Horsfall
c -------------------------------------------------------------------
c     Modified 2/7/96 by MDM
c
c     This version modified 8/99 to include temp array in argument list
c
c     izp = 1 if field is zonally periodic with no overlapping point
c         = 0 if not periodic
c
c -------------------------------------------------------------------
c
      real temp(idim,jdim),fld(idim,jdim)
c
c --- set weights 
c
      wt1=.5
      wt2=.25
c
c --- apply filter zonally
c
      do 20 j=1,jdm
      do 20 i=1,idm
         im1=i-1
         ip1=i+1 
         if (i.eq.1) then
            if (izp .eq. 1) then
               im1=idm
            else
               im1=1
            endif
         endif
c
         if (i.eq.idm) then
            if (izp .eq. 1) then
               ip1=1
            else
               ip1=idm
            endif
         endif
c
         temp(i,j)=fld(i,j)*wt1+(fld(ip1,j)+fld(im1,j))*wt2
   20 continue
c
c --- apply filter meridionally
c
      do 30 i=1,idm
      do 30 j=1,jdm
         jm1=j-1
         jp1=j+1 
c
         if (j.eq.  1) jm1=1
         if (j.eq.jdm) jp1=jdm 
c
         fld(i,j)=temp(i,j)*wt1+(temp(i,jp1)+temp(i,jm1))*wt2
   30 continue
c
      return 
      end
      subroutine stndz(p,z,t,theta)
c     This routine calculates the standard height z (m) from the
c     pressure p (mb). The temperature t (K) and potential temperature
c     theta (K) at p are also calculated.
C
      g   = 9.80665
      r   = 287.05
      cp  = 1004.0
      b   = 0.0065
      p0  = 1013.25
      t0  = 288.15
      p00 = 1000.0
      p1  = 226.32
      t1  = 216.65
      z1  = 11000.0
      cap = r/cp
      a   = r*b/g
C
      z2  = 20000.0
      b2  = -0.0010
      p2  = 54.75
      t2  = t1
      a2  = r*b2/g
C
      if     (p .ge. p1) then
         z = (t0/b)*(1.0 - (p/p0)**a)
         t = t0 - b*z
      elseif (p .lt. p1 .and. p .ge. p2) then
         z = z1 + (r*t1/g)*alog(p1/p)
         t = t1
      else
	 z = z2 + (t2/b2)*(1.0 - (p/p2)**a2)
	 t = t2 - b2*(z-z2)
      endif
C
      theta = t*( (p00/p)**cap )
c
      return
      end
      subroutine tdiff(iy2,im2,id2,it2,iy1,im1,id1,it1,idelt)
c     This routine calculates the number of hours (delt) between
c     two date/times.
c
c     Note: Times are in hours
c
      dimension nday(12)
c
      data nday /0,31,59,90,120,151,181,212,243,273,304,334/
c
c     Calculate reference year
      iry = iy1-2
      if (iy2 .lt. iry) iry=iy2-2
c
c     Calculate the number of hours from 00 Jan. 1 of the reference year
      ity1 = 0
      do 10 i=iry,iy1-1
         if (mod(i,4) .eq. 0) then
            ity1 = ity1 + 24*366
         else
            ity1 = ity1 + 24*365
         endif
   10 continue
c
      ity2 = 0
      do 15 i=iry,iy2-1
         if (mod(i,4) .eq. 0) then
            ity2 = ity2 + 24*366
         else
            ity2 = ity2 + 24*365
         endif
   15 continue
c
      ity1 = ity1 + 24*nday(im1)
      if ((mod(iy1,4) .eq. 0) .and. im1 .gt. 2) ity1=ity1+24
c
      ity2 = ity2 + 24*nday(im2)
      if ((mod(iy2,4) .eq. 0) .and. im2 .gt. 2) ity2=ity2+24
c
      ity1 = ity1 + 24*id1 + it1
      ity2 = ity2 + 24*id2 + it2
c
      idelt = ity2 - ity1
c
      return
      end
      subroutine ctor(x,y,r,theta)
c     This routine converts from Cartesion coordinates
c     to radial coordinates, where theta is in
c     degrees measured counter-clockwise from
c     the +x-axis.
c
      r = sqrt(x*x + y*y)
c
      if (r .le. 0.0) then
         theta = 0.0
         return
      endif
c
      rtd = 57.296
      theta = rtd*acos(x/r)
      if (y .lt. 0.0) theta = 360.0 - theta
c
      return
      end
      subroutine rtoc(r,theta,x,y)
c     This routine converts from radial coordinates
c     to Cartesian coordinates, where theta is in
c     degrees measured counter-clockwise from
c     the +x-axis.
c
      rtd = 57.296
      x = r*cos(theta/rtd)
      y = r*sin(theta/rtd)
c
      return
      end
      subroutine ucase(char,len)
c     This routine converts all letters in the character char 
c     of length len to upper case
c
      character *(*) char
c
      character *1 ucl(26)
      character *1 lcl(26)
c
      data ucl /'A','B','C','D','E','F','G','H','I','J','K','L','M',
     +          'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
c
      data lcl /'a','b','c','d','e','f','g','h','i','j','k','l','m',
     +          'n','o','p','q','r','s','t','u','v','w','x','y','z'/
c
      do m=1,len
	 do k=1,26
	    if (char(m:m) .eq. lcl(k)) char(m:m)=ucl(k)
         enddo
      enddo
c
      return
      end
