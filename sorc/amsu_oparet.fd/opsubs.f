      subroutine zalcals(t,ts,ps,p,z,lulog)
c     This routine calculates the height z at all AMSU levels,
c     and the surface pressure ps at the domain interior points.
c
c     This version assumes z at the top level has been specified
c     and is used as an upper boundary condition
c
      parameter (nx=61,ny=61)
      parameter (np=23)
c
      dimension t(nx,ny,np),z(nx,ny,np)
      dimension ts(nx,ny),ps(nx,ny)
      dimension p(np)
c
c     Integrate z down to lowest AMSU level
      do 25 j=1,ny
      do 25 i=1,nx
	 do 27 k=2,np
	    t1 = t(i,j,k-1)
	    t2 = t(i,j,k)
	    p1 = p(k-1)
	    p2 = p(k)
	    call tkness(p1,p2,t1,t2,delz)
	    z(i,j,k) = z(i,j,k-1) - delz
   27    continue
   25 continue
c
c     Calculate surface pressure by integrating from lowest 
c     AMSU level to the surface
      do 30 j=1,ny
      do 30 i=1,nx
	 t1 = t(i,j,np)
	 z1 = z(i,j,np)
	 p1 = p(np)
	 t2 = ts(i,j)
	 z2 = 0.0
	 call p2cal(z1,z2,t1,t2,p1,p2)
	 ps(i,j) = p2
   30 continue
c
      return
      end
      subroutine zalcal(t,ts,ps,p,z,lulog)
c     This routine calculates the height z at all AMSU levels,
c     and the surface pressure ps at the domain interior points.
c
      parameter (nx=61,ny=61)
      parameter (np=23)
c
      dimension t(nx,ny,np),z(nx,ny,np)
      dimension ts(nx,ny),ps(nx,ny)
      dimension p(np)
c
c     Local work arrays
      dimension ta1(nx,ny),ta2(nx,ny)
c
c     Calculate height of lowest AMSU level at boundary points
      do 10 i=1,nx
c        Bottom edge of domain
         j=1
         t1 = ts(i,j)
         t2 =  t(i,j,np)
         p1 = ps(i,j)
         p2 = p(np)
         call tkness(p1,p2,t1,t2,delz)
         z(i,j,np) = delz
c
c        Top edge of domain
         j=ny
         t1 = ts(i,j)
         t2 =  t(i,j,np)
         p1 = ps(i,j)
         p2 = p(np)
         call tkness(p1,p2,t1,t2,delz)
         z(i,j,np) = delz
   10 continue
c
      do 12 j=2,ny-1
c        Left edge of domain
         i=1
         t1 = ts(i,j)
         t2 =  t(i,j,np)
         p1 = ps(i,j)
         p2 = p(np)
         call tkness(p1,p2,t1,t2,delz)
         z(i,j,np) = delz
c
c        Right edge of domain
         i=nx
         t1 = ts(i,j)
         t2 =  t(i,j,np)
         p1 = ps(i,j)
         p2 = p(np)
         call tkness(p1,p2,t1,t2,delz)
         z(i,j,np) = delz
   12 continue
c
c     Calculate z at the rest of the AMSU levels at the boundary points
      do 15 k=np-1,1,-1
         do 17 i=1,nx
c           Bottom edge of domain
            j=1
            t1 = t(i,j,k+1)
            t2 = t(i,j,k  )
            p1 = p(k+1)
            p2 = p(k)
            call tkness(p1,p2,t1,t2,delz)
            z(i,j,k) = z(i,j,k+1) + delz
c
c           Top edge of domain
            j=ny
            t1 = t(i,j,k+1)
            t2 = t(i,j,k  )
            p1 = p(k+1)
            p2 = p(k)
            call tkness(p1,p2,t1,t2,delz)
            z(i,j,k) = z(i,j,k+1) + delz
            if (k .eq. 1) then
            endif
   17    continue
c
         do 19 j=2,ny-1
c           Left edge of domain
            i=1
            t1 = t(i,j,k+1)
            t2 = t(i,j,k  )
            p1 = p(k+1)
            p2 = p(k)
            call tkness(p1,p2,t1,t2,delz)
            z(i,j,k) = z(i,j,k+1) + delz
c
c           Right edge of domain
            i=nx
            t1 = t(i,j,k+1)
            t2 = t(i,j,k  )
            p1 = p(k+1)
            p2 = p(k)
            call tkness(p1,p2,t1,t2,delz)
            z(i,j,k) = z(i,j,k+1) + delz
            if (k .eq. 1) then
            endif
   19    continue
   15 continue
c
c     Interpolate z (top pressure level) at the boundary points
c     to the domain interior using a Laplacian filter
      do 20 j=1,ny
      do 20 i=1,nx
	 ta1(i,j) = 0.0
	 ta2(i,j) = z(i,j,1)
   20 continue
c
      call pson(ta1,ta2,z(1,1,1),ierr)
      if (ierr .ne. 0) then
	 write(lulog,960)
  960    format(/,' pson routine did not converge in z calculation')
	 stop
      endif
c
c     Integrate z down to lowest AMSU level at interior points
      do 25 j=2,ny-1
      do 25 i=2,nx-1
	 do 27 k=2,np
	    t1 = t(i,j,k-1)
	    t2 = t(i,j,k)
	    p1 = p(k-1)
	    p2 = p(k)
	    call tkness(p1,p2,t1,t2,delz)
	    z(i,j,k) = z(i,j,k-1) - delz
   27    continue
   25 continue
c
c     Calculate surface pressure by integrating from lowest 
c     AMSU level to the surface
      do 30 j=2,ny-1
      do 30 i=2,nx-1
	 t1 = t(i,j,np)
	 z1 = z(i,j,np)
	 p1 = p(np)
	 t2 = ts(i,j)
	 z2 = 0.0
	 call p2cal(z1,z2,t1,t2,p1,p2)
	 ps(i,j) = p2
   30 continue
c
      return
      end
      subroutine ncepget(pn,un,vn,zn,tn,tn1000,rlond,rlatd,nx,ny,npn,
     +                   dlona,dlata,dayx,utcx,lundat,lulog,ierr)
c     This routine gets u,v,z and t from the packed ncep analysis file
c
      parameter (ixmax=181,iymax=91)
c
      dimension pn(npn)
      dimension un(nx,ny,npn),vn(nx,ny,npn),zn(nx,ny,npn),tn(nx,ny,npn)
      dimension rlond(nx),rlatd(ny)
c
      dimension tn1000(ixmax,iymax)
c
c     Local variables
c
c     Data for unpacking
      parameter(imax=20000)
      dimension tra(imax)
      character *1 type
      character *2 code(imax)
c
      dimension t2d(ixmax,iymax)
c
c     Array for checking that all requested NCEP levels were found
      parameter (nvar=4)
      dimension ncheck(100,nvar)
      character *1 ccheck(nvar)
c
      data ccheck /'U','V','Z','T'/      
c
      common /ncepfg/ rlonln,rlatbn,dlonn,dlatn,nlonn,nlatn
c
c     Set interp=1 to allow option of interpolating between 
c     levels of NCEP data. There must be data above and below 
c     the levels requiring interpolation. 
      interp=1
c
c     Set checking array to zero
      do 5 k=1,npn
      do 5 i=1,nvar
	 ncheck(k,i) = 0
    5 continue
c
      lu    = lundat
      linen = 0
c
c     Read main header on packed data file
      read(lu,200,err=901,end=901)
     +            wx,dayx,utcx,rlatmn,rlatmx,rlonmn,rlonmx,dlatn,dlonn
  200 format(1x,f3.0,f7.0,f5.0,4f8.3,1x,2f4.2)
      linen = linen + 1
c
c     Calculate lat and lon intervals and number of points on NCEP
c     data file
      epsil = 0.0001
      nlat1 = 1 + ifix((rlatmx-rlatmn)/dlatn + epsil)
      nlon1 = 1 + ifix((rlonmx-rlonmn)/dlonn + epsil)
      ipts  = nlat1*nlon1
      rlatb = rlatmn
      rlatt = rlatmx
      rlonl = -rlonmx
      rlonr = -rlonmn
c
      rlonln = rlonl
      rlatbn = rlatb
      nlonn  = nlon1
      nlatn  = nlat1
c
      write(lulog,300) rlonl,rlonr,dlonn,rlatb,rlatt,dlatn,
     +                 nlon1,nlat1,ipts
  300 format(/,' Data read from NCEP file',/,
     +       ' rlonl=',f6.1,' rlonr=',f6.1,'  dlon=',f4.2,/,
     +       ' rlatb=',f6.1,' rlatt=',f6.1,'  dlat=',f4.2,/,
     +       ' nlon1=',i6,' nlat1=',i6,'  ipts=',i6)
c
c     Check array size
      if (ipts .gt. imax) then
         ierr = 2
         return
      endif
c
      if (nlon1 .gt. ixmax .or. nlat1 .gt. iymax) then
	 ierr = 2
	 return
      endif
c
c     Read the rest of the data file
      nrow = 1 + (ipts-1)/36
c
  500 continue
         read(lu,202,err=901,end=600) type,ptmp,bsub,smpy
  202    format(1x,a1,1x,f6.1,2(1x,g15.9))
         linen = linen + 1
c
c        write(lulog,320) type,ptmp,linen,bsub,smpy
c 320    format(1x,a1,' p=',f6.1,' read from line ',i5,
c    +          ' of ncep file, b,s: ',2(e10.3))
c
c        Read packed data
         do 10 n=1,nrow
            is = 1 + (n-1)*36
            ie = is + 35
            read(lu,204,end=901,err=901) (code(i),i=is,ie)
            linen = linen + 1
  204       format(36(a2))
   10    continue
c
c        Search for required NCEP pressure level and data type
         ii = 0
	 do 11 i=1,nvar
	    if (type .eq. ccheck(i)) then
	       ii = i
            endif
   11    continue
c
         kk = 0
	 do 12 k=1,npn
	    if (ptmp .eq. pn(k)) then
	       kk = k 
            endif
   12    continue
c
         if (kk .eq. 0 .or. ii .eq. 0) go to 500
c
c        This variable is need, so update checking array and unpack it.
	 ncheck(kk,ii) = 1
c
c        Unpack current variable
	 do 15 i=1,ipts
	    idec = idecod(code(i))
	    tra(i) = float(idec)*smpy - bsub
   15    continue
c
c        Add standard height to height perturbation
	 if (type .eq. 'Z') then
	    call stndz(ptmp,zstd,tstd,thstd)
	    do 16 i=1,ipts
	       tra(i) = tra(i) + zstd
   16       continue
	 endif
c
c        Put data in 2-D lon/lat array
         do 20 j = 1,nlat1
	 do 20 i = 1,nlon1
	    ij = i + (j-1)*nlon1
	    t2d(i,j) = tra(ij)
   20    continue
c
         alonl = rlond(1)
         if (rlonr .eq. 360.0 .and. alonl .lt. 0.0) then
            alonl = alonl + 360.0
         endif
c
	 alatb = rlatd(1)
	 izp   = 0
c
c        write(6,990) rlonl,rlatb,dlonn,dlatn,ixmax,iymax,nlon1,nlat1
c 990    format(/,' rlonl=',f6.1,' rlatb=',f6.1,' dlonn=',f6.1,' dlatn=',
c    +          f6.1,/,' ixmax=',i4,' iymax=',i4,' nlon1=',i4,
c    +          ' nlat1=',i4)
c
c        write(6,991) alonl,alatb,dlona,dlata,nx,ny
c 991    format(' alonl=',f6.1,' alatb=',f6.1,' dlona=',f6.1,' dlata=',
c    +          f6.1,/,' nx=',i4,' ny=',i4)
c
c        Interpolate data from NCEP to AMSU grid points
	 if      (type .eq. 'U') then
            call llintp(       t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax,
     +                                                     nlon1,nlat1,
     +                  un(1,1,kk),alonl,alatb,dlona,dlata,  nx,  ny,
     +                                                     nx,  ny, 
     +                                                         izp,lerr)
	 else if (type .eq. 'V') then
            call llintp(       t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax,
     +                                                     nlon1,nlat1,
     +                  vn(1,1,kk),alonl,alatb,dlona,dlata,  nx,  ny,
     +                                                     nx,  ny,
     +                                                         izp,lerr)
	 else if (type .eq. 'T') then
            call llintp(       t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax,
     +                                                     nlon1,nlat1,
     +                  tn(1,1,kk),alonl,alatb,dlona,dlata,  nx,  ny,
     +                                                     nx,  ny,
     +                                                         izp,lerr)
c
            if (ptmp .eq. 1000.0) then
c              Save 1000 mb temperature on for tcorsv routine
	       do j=1,nlat1
	       do i=1,nlon1
		  tn1000(i,j) = t2d(i,j)
               enddo
	       enddo
            endif
c
	 else if (type .eq. 'Z') then
            call llintp(       t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax,
     +                                                     nlon1,nlat1,
     +                  zn(1,1,kk),alonl,alatb,dlona,dlata,  nx,  ny,
     +                                                     nx,  ny,
     +                                                         izp,lerr)
	 else
	    ierr = 5
	    return
	 endif
c
         go to 500
  600 continue
c
      if (interp .eq. 1) then
c        Check to see if data at missing pressure levels can
c        be filled by interpolation. Top and bottom levels can
c        not be interpolated. 
         do k=2,npn-1
	 do m=1,nvar
            if (ncheck(k,m) .eq. 0 .and. ncheck(k+1,m) .eq. 1
     +                             .and. ncheck(k-1,m) .eq. 1) then 
c
	       ncheck(k,m) = 1
c
               pk  = pn(k) 
	       pkp = pn(k+1)
	       pkm = pn(k-1)
c 
	       wtm = (pkp-pk)/(pkp-pkm)
	       wtp = (pk-pkm)/(pkp-pkm)
c
	       if     (m .eq. 1) then
		  do j=1,ny
		  do i=1,nx
                     un(i,j,k) = wtm*un(i,j,k-1) + wtp*un(i,j,k+1)
		  enddo
		  enddo
	       elseif (m .eq. 2) then
		  do j=1,ny
		  do i=1,nx
                     vn(i,j,k) = wtm*vn(i,j,k-1) + wtp*vn(i,j,k+1)
		  enddo
		  enddo
	       elseif (m .eq. 3) then
		  do j=1,ny
		  do i=1,nx
                     zn(i,j,k) = wtm*zn(i,j,k-1) + wtp*zn(i,j,k+1)
		  enddo
		  enddo
	       elseif (m .eq. 4) then
		  do j=1,ny
		  do i=1,nx
                     tn(i,j,k) = wtm*tn(i,j,k-1) + wtp*tn(i,j,k+1)
		  enddo
		  enddo
	       endif
	    endif
	 enddo
	 enddo
      endif    
   
c     Check to make sure all levels have been found
      do 90 k=1,npn
      do 90 i=1,nvar
	 if (ncheck(k,i) .eq. 0) then
	    write(lulog,950) ccheck(i),pn(k)
  950       format(/,' Required NCEP variable not found, type= ',a1,
     +               '  P=',f6.1)
	    ierr = 4
	 endif
   90 continue
c
      write(lulog,330) nvar,npn 
  330 format(/,'All ',i1,' NCEP variables found at all ',i2,
     +         ' requested levels')
c
      ierr = 0
      return
c
  901 continue
      ierr = 1
      return
c
      end
      subroutine aprint(a,nx,ny,plev,iscale,inc,lulog,label)
c     This routine prints the 2-d array a.
c     If iscale=1, the array is scaled to a number between 0 and 100.
c     If iscale<0, the array is scaled by abs(iscale)
c
      dimension a(nx,ny)
      character *20 label
c
      slope  = 1.0
      offset = 0.0
c
c     Find max and min of a
      aamax = -1.0e+20
      aamin =  1.0e+20
      do 10 j=1,ny
      do 10 i=1,nx
         if (a(i,j) .gt. aamax) aamax=a(i,j)
         if (a(i,j) .lt. aamin) aamin=a(i,j)
   10 continue
c
      if (aamax .ne. aamin .and. iscale .eq. 1) then
         offset = aamin
         slope  = 100.0/(aamax-aamin)
      endif
c
      if (iscale .lt. 0) then
         offset = 0.0
         slope = 1.0/float(-iscale)
      endif
c
      write(lulog,200) label,plev/100.0,aamax,aamin,iscale
  200 format(/,1x,a20,' p(mb)=',f6.1,' max,min= ',e11.4,1x,e11.4,
     +                ' iscale=',i6)
c
      istart = 1 
      iend   = nx
c
      if (nx .gt. 26) then
	 icut = 1 + (nx-27)/2 
	 istart = istart + icut
	 iend   = iend   - icut
      endif
c
      if (inc .gt. 0) then
         do 20 j=1,ny,inc
            write(lulog,210) j,( (slope*(a(i,j)-offset)),i=istart,iend )
  210       format(1x,i2,1x,26(f6.1),/,4x,26(f6.1))
   20    continue
      elseif (inc .lt. 0) then
         do 30 j=ny,1,inc
            write(lulog,210) j,( (slope*(a(i,j)-offset)),i=istart,iend)
   30    continue
      endif
c
      write(lulog,220) (i,i=istart,iend)
  220 format(6x,26(i2,4x),/,6x,26(i2,4x))
c
      return
      end
      subroutine distk(rlon1,rlat1,rlon2,rlat2,dx,dy,rad)
c     This routine calculates the distance in km (rad) between the
c     points (rlon1,rlat1) and (rlon2,rlat2) using an approximate
c     formula. The lon and lat are in deg E and N. The east and
c     north components of the distance (dx,dy) are also calculated.
c
      common /cons/ pi,g,rd,dtr,erad,erot
c
      dtk = 111.1
      cfac = cos(0.5*dtr*(rlat1+rlat2))
c
      dx  = dtk*(rlon2-rlon1)*cfac
      dy  = dtk*(rlat2-rlat1)
      rad = sqrt(dx*dx + dy*dy)
c
      return
      end
      subroutine wpof(u,v,t,z,ps,iyr,imon,iday,itime,luout)
c     This routine writes u,v,t,z and ps to a packed output file
c
      parameter (nx=61,ny=61)
      parameter (npn=12)
      dimension u(nx,ny,npn),v(nx,ny,npn),t(nx,ny,npn),z(nx,ny,npn)
      dimension ps(nx,ny)
c
c     NCEP pressure levels
      dimension pn(npn)
c
c     Lat/Lon arrays
      dimension rlond(nx),rlatd(ny)
      dimension rlonr(nx),rlatr(ny)
      dimension sinlat(ny),coslat(ny),tanlat(ny)
c
c     Local variables
      parameter (imax=20000)
      dimension tra(imax)
      character *1 type
      character *2 code(imax)
c
      common /log/  lulog
      common /ncepll/ rlond,rlonr,rlatd,rlatr,dlon,dlat,
     +                dlonr,dlatr,sinlat,coslat,tanlat
      common /ncepp/ pn
c
c     Write header line on the file
      wx = 12.
      rdate = float(10000*iyr + 100*imon + iday)
      rtime = float(itime)
      ft    = 0.0
c
      write(luout,200) wx,rdate,rtime,rlatd( 1), rlatd(ny),
     +                               -rlond(nx),-rlond( 1),dlat,dlon,ft
  200 format(1x,f3.0,f7.0,f5.0,4f8.3,2f4.2,f6.0)
c
c     Calculate number of grid points
      ipts = nx*ny
      nrow = 1 + (ipts-1)/36
c
      if (ipts .gt. imax) then
	 write(lulog,900) 
  900    format(/,' AMSU analysis domain too large, increase imax ',
     +          /,' in routine wpof. ')
	 stop
      endif
c
c     Fill in code array with 1s before packing
      do 10 i=1,ipts+36
	 code(i) = '11'
   10 continue
c
      epsil = 0.001
c     Pack sea-level pressure (in mb)
      type = 'S'
c
      icount = 0
      do 15 j=1,ny
      do 15 i=1,nx
	 icount = icount + 1
	 tra(icount) = ps(i,j)/100.0
   15 continue
c
      call maxmin(tra,1,ipts,tmax,tmin)
      call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)
c
      plevx = 1070.0
      write(luout,210) type,plevx,bsub,smpy
  210 format(1x,a1,1x,f6.1,2(1x,g15.9))
c
      do 20 n=1,nrow
	 is = 1 + (n-1)*36
	 ie = is+35
	 write(luout,220) (code(i),i=is,ie)
  220    format(36(a2))
   20 continue
c
c     Pack the rest of the variables
      do 25 k=1,npn
	 plevx = pn(k)/100.0
	 call stndz(plevx,zstd,tstd,thstd)
c
         type = 'U'
	 icount = 0
	 do 31 j=1,ny
	 do 31 i=1,nx
	    icount = icount + 1
	    tra(icount) = u(i,j,k)
   31    continue
c
         call maxmin(tra,1,ipts,tmax,tmin)
         call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)
c
         write(luout,210) type,plevx,bsub,smpy
c
         do 32 n=1,nrow
            is = 1 + (n-1)*36
            ie = is+35
            write(luout,220) (code(i),i=is,ie)
   32    continue
c
         type = 'V'
	 icount = 0
	 do 41 j=1,ny
	 do 41 i=1,nx
	    icount = icount + 1
	    tra(icount) = v(i,j,k)
   41    continue
c
         call maxmin(tra,1,ipts,tmax,tmin)
         call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)
c
         write(luout,210) type,plevx,bsub,smpy
c
         do 42 n=1,nrow
            is = 1 + (n-1)*36
            ie = is+35
            write(luout,220) (code(i),i=is,ie)
   42    continue
c
         type = 'Z'
	 icount = 0
	 do 51 j=1,ny
	 do 51 i=1,nx
	    icount = icount + 1
	    tra(icount) = z(i,j,k) - zstd
   51    continue
c
         call maxmin(tra,1,ipts,tmax,tmin)
         call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)
c
         write(luout,210) type,plevx,bsub,smpy
c
         do 52 n=1,nrow
            is = 1 + (n-1)*36
            ie = is+35
            write(luout,220) (code(i),i=is,ie)
   52    continue
c
         type = 'T'
	 icount = 0
	 do 61 j=1,ny
	 do 61 i=1,nx
	    icount = icount + 1
	    tra(icount) = t(i,j,k)
   61    continue
c
         call maxmin(tra,1,ipts,tmax,tmin)
         call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)
c
         write(luout,210) type,plevx,bsub,smpy
c
         do 62 n=1,nrow
            is = 1 + (n-1)*36
            ie = is+35
            write(luout,220) (code(i),i=is,ie)
   62    continue
   25 continue
c
      return
      end
      subroutine tcors(mxas,nxas,t,clw,aslon,aslat,
     +                 sslon,sslat,works,dt,dtred,tcrmax)
c     This routine reduces the temperature anomaly below the
c     threshold dt by a factor dtred. This version uses the
c     pre-analyzed swath temperature data.
c
      dimension t(mxas),clw(mxas)
      dimension aslon(mxas),aslat(mxas),works(mxas)
c
c     Calculate radius of each swath point
      do 5 i=1,nxas
	   call distk(sslon,sslat,aslon(i),aslat(i),dx,dy,rad)
	   works(i) = rad
    5 continue
c
c     Find mean temperature
      tbar = 0.0
      rpts = 0.0
      do 10 i=1,nxas
	 if (works(i) .gt. tcrmax) go to 10
	 tbar = tbar + t(i)
	 rpts = rpts + 1.0
   10 continue
c
      tbar = tbar/rpts
c
      do 20 i=1,nxas
	 if (works(i) .gt. tcrmax) go to 20
	 dtt = t(i)-tbar
	 if (dtt .lt. dt) then
	    t(i) = tbar + dt + (dtt-dt)*dtred
         endif
   20 continue
c
      return
      end
      subroutine tcorsv(mxas,nxas,mpas,tas,tn1000,pamsu,
     +                  clwas,aslon,aslat,clwth1,clwth2,kp1,kp2,lulog)
c     This routine adjusts the temperature profile t to a constant
c     lapse rate profile in regions where clw > clwth1. For clw > clwth2,
c     The original profile is completely replaced by the constant lapse
c     rate profile between pressure levels kp1 and kp2. The constant
c     lapse rate is determined from the amsu temperature at pressure
c     level kp1 and the NCEP analysis temperature at 1000 mb.
c
      parameter (ixmax=181,iymax=91)
      dimension tas(mxas,mpas),clwas(mxas),aslon(mxas),aslat(mxas)
      dimension tn1000(ixmax,iymax)
      dimension pamsu(mpas)
c
      common /cons/ pi,g,rd,dtr,erad,erot
      common /ncepfg/ rlonln,rlatbn,dlonn,dlatn,nlonn,nlatn
c
c     Assign pressure levels for constant lapse rate atmosphere
      p1 = 1000.0e+2 
      p2 = 100.0*pamsu(kp1)
c
c     Search for points that require adjustment
      icnt = 0
      do 10 i=1,nxas
	 if (clwas(i) .gt. clwth1) then
c
c           Calculate temperature profile weights
            clwt = clwas(i)
	    if (clwt .gt. clwth2) then
	       wtam = 0.0
	       wtcl = 1.0
            else
	       wtam = (clwth2-clwt)/(clwth2-clwth1)
	       wtcl = (clwt-clwth1)/(clwth2-clwth1)
            endif
c
c           Calculate parameters for the constant lapse rate
c           atmosphere
	    t2 = tas(i,kp1)
	    izp = 0
            call llintsp(tn1000,rlonln,rlatbn,dlonn,dlatn,ixmax,iymax,
     +                   nlonn,nlatn,t1,aslon(i),aslat(i),izp,ierr) 
	    gmma = (g/rd)*alog(t2/t1)/alog(p1/p2)
	    aa   = -rd*gmma/g
c
            icnt = icnt + 1
            write(lulog,300) icnt,aslat(i),aslon(i),clwt,gmma
  300       format(i4,' lat,lon: ',f6.2,1x,f7.2,' clw: ',f5.2,
     +                ' gmma:',e11.4)
c
	    do 20 k=kp1,kp2
	       tcl = t1*(100.0*pamsu(k)/p1)**aa
	       tas(i,k) = wtam*tas(i,k) + wtcl*tcl
   20       continue
         endif
   10 continue

      return
      end
      subroutine tcorsr(mxas,nxas,t,clw)
c     This routine reduces the temperature anomaly by using the 
c     slope relationship between cloud liquid water and temperature
c     anomalies (base temperature comes from regions with no clw).
c     This program also adjusts the temperatures over the land areas 
c     where clw is not available. In this case, if negative temperature 
c     anomalies exist over land they are replaced with a temperature .2 
c     degrees colder than the mean + twenty percent of the original 
c     temperature anomaly.
c     This version uses the pre-analyzed swath temperature data.
c
      dimension t(mxas),clw(mxas)
c
c     Find mean temperature
      tbar = 0.0
      npts=0
      rpts = float(nxas)
      do 10 i=1,nxas
         if (clw(i) .le. 0.001 .and. clw(i) .gt. -1.0) then
            tbar = tbar + t(i)
            npts=npts+1
         endif
   10 continue
c
      tbar = tbar/float(npts) ! mean
      sxy=0.0      ! Sum of x*y
      sxx=0.0      ! Sum of x^2
c
      do i=1,nxas
         if (clw(i) .gt. 0.0) then
            sxx=sxx+clw(i)**2
            sxy=sxy+clw(i)*(t(i)-tbar)
         endif
      enddo
c
      slope=sxy/sxx  
c     write(6,*) 'tbar,npts,slope ',tbar,npts,slope
c
c over land temperature fix
      do 20 i=1,nxas
	 if (slope.lt.0 .and. clw(i) .ge. 0.0) then 
	    t(i) = t(i)-slope*clw(i) 
         elseif(clw(i).lt.-50.and.(t(i)-tbar).lt.-0.2)then
            t(i) = tbar+(-0.2+0.2*(t(i)-tbar))
         endif

   20 continue
c
      return
      end
      subroutine tcor(nx,ny,t,dt,dtred)
c     This routine reduces the temperature anomaly below the
c     threshold dt by a factor dtred. This version uses the analyzed
c     temperature data.
c
      dimension t(nx,ny)
c
c     Find mean temperature
      tbar = 0.0
      rpts = float(nx*ny)
      do 10 j=1,ny
      do 10 i=1,nx
	 tbar = tbar + t(i,j)
   10 continue
c
      tbar = tbar/rpts
c
      do 20 j=1,ny
      do 20 i=1,nx
	 dtt = t(i,j)-tbar
	 if (dtt .lt. dt) then
	    t(i,j) = tbar + dt + (dtt-dt)*dtred
         endif
   20 continue
c
      return
      end
      subroutine tcorclw(mxas,nxas,amkl,tas,clwas,k,lulog)
c     This routine corrects for CLW attenuation using temp and CLW
c     on the swath.  Regression slopes at each pressure level
c     were derived by comparing tdev vs CLW for 64 noland storms. 
c 
      dimension oldtas(mxas)
      dimension tas(mxas)
      dimension clwas(mxas)
c
c     Read in initial temp values
      do 69 i=1,nxas
        oldtas(i) = tas(i)
   69 continue
c
c     Values greater than clwthres are corrected
      clwthres = 0.3
      do 21 i=1,nxas
         if (clwas(i) .gt. clwthres) then
            tas(i) = oldtas(i) + (amkl*clwas(i))
         endif
   21 continue
      return
      end
      subroutine tcorice(k,nx,ny,t,clwxy,
     &                   told,tnew,diff,lulog,luice,
     &                   atcfid,imon,iday,itime)
c     This routine reduces the temperature anomaly via Ben Linstid's
c     algorithm.  It corrects for ice crystal attenuation where
c     temp < (tmean - thrvar); uses Poisson's (actually Laplace's b/c equ=0)
c     equation to replace bad temps with ave of nearest 2 to 4 neighbors.  
c
      dimension t(nx,ny)
      dimension clwxy(nx,ny)
      dimension told(nx,ny)
      dimension tnew(nx,ny)
      dimension diff(nx,ny)
c
c     Specify parameters
      clwaa=0.2 
      thrvar = 0.5
c      tol = 0.001
      tol = 0.005
      smoothmx = 1000.0
c
c     Find mean temperature of points with clwxy < clwaa to establish thresh
      tmean = 0.0
      count = 0.0
      do 22 j=1,ny
      do 22 i=1,nx
         told(i,j) = t(i,j)
         if (clwxy(i,j) .lt. clwaa) then
            tmean = tmean + t(i,j)
            count = count + 1.0
         endif
   22 continue
c
      tmean = tmean/count
      thresh = tmean - thrvar
c
c     Count number of points to be corrected
      iice = 0
      do j=1,ny
      do i=1,nx
         if (t(i,j) .lt. thresh) iice=iice+1
      enddo
      enddo
c
c     Correct where tprime is less than some threshold using
c     Poisson/Laplace's equation of averages.  Exit subroutine if 
c     temperature don't converge before mxsmooth number of iterations.
      smoothct = 0.0
   99 continue
      diffmax = 0.0
      smoothct = smoothct + 1.0
      if (smoothct .eq. smoothmx) then
	 write(luice,969) 'NO temp convergence ',k,atcfid,imon,iday,itime
  969    format(a20,i2,1x,a6,1x,3(i2.2))
         goto 101
      endif
c
      do 23 j=1,ny
      do 23 i=1,nx
	   if (t(i,j) .lt. thresh) then
	      if (i.eq.1 .and. j.eq.1) then
	         tnew(i,j) = (told(i,j+1) + told(i+1,j))/2.0
	      else if (i.eq.nx .and. j.eq.1) then
	         tnew(i,j) = (told(i-1,j) + told(i,j+1))/2.0
              else if (i.eq.1 .and. j.eq.ny) then
	         tnew(i,j) = (told(i,j-1) + told(i+1,j))/2.0
	      else if (i.eq.nx .and. j.eq.ny) then
	         tnew(i,j) = (told(i-1,j) + told(i,j-1))/2.0
	      else if (i.eq.1 .and. j.gt.1 .and. j.lt.ny) then
	         tnew(i,j) = (told(i,j+1) + told(i+1,j) + told(i,j-1))/3.0
	      else if(i.eq.nx .and. j.gt.1 .and. j.lt.ny) then
	         tnew(i,j) = (told(i,j+1) + told(i-1,j) + told(i,j-1))/3.0
	      else if(j.eq.1 .and. i.gt.1 .and. i.lt.nx) then
	         tnew(i,j) = (told(i+1,j) + told(i,j+1) + told(i-1,j))/3.0
	      else if(j.eq.ny .and. i.gt.1 .and. i.lt.nx) then
	         tnew(i,j) = (told(i+1,j) + told(i,j-1) + told(i-1,j))/3.0
	      else
	         tnew(i,j) = ((told(i+1,j) + told(i-1,j) + told(i,j+1) +
     &                  told(i,j-1))/4.0)
	      endif
	   else
	      tnew(i,j) = told(i,j)
	   endif
c
	   diff(i,j) = abs(tnew(i,j) - told(i,j))
	   if (diff(i,j) .gt. diffmax) then
	      diffmax = diff(i,j)
	   endif
   23 continue
c
	if (diffmax .gt. tol) then
	   do 28 j=1,ny
	   do 28 i=1,nx
	      told(i,j) = tnew(i,j)
   28      continue
           goto 99
        endif
c
c     Copy corrected temperature data into old array to return
      do 29 j=1,ny
      do 29 i=1,nx
	   t(i,j) = tnew(i,j)
   29 continue
  101 continue
c
      icount = ifix(count)
      write(lulog,400) iice,nx*ny
  400 format(/,' Ice correction completed, ',i5,' of ',i5,
     +       ' points adjusted')
c
c     Output number of smoothing iterations
  970 format('Ice correction used ',i6.1, ' smoothing iterations
     &       for level ',i2)
      return
      end
      subroutine fadj(f,fa)
c     This routine adjusts the Coriolis parameter f near the
c     equator to keep it from going to zero
c 
      fmin = 1.0e-6
      absf  = abs(f)
      sgnf  = f/absf
c
      if (absf .lt. fmin) then
         fa = fmin*sgnf
      else
         fa = f
      endif
c
      return
      end
      subroutine wloc(sslat,sslon,aslat,aslon,luloc,np,
     +                jyr,jmon,jday,jtimeh,jtimem,atcfid,sname)
c     This routine writes the storm center position (sslat,sslon)
c     and AMSU data locations to a file for later plotting.
c
      dimension aslat(np),aslon(np)
      character *6 atcfid
      character *9 sname
c
c     Write label
      write(luloc,210) jyr,jmon,jday,jtimeh,jtimem,atcfid,sname
  210 format('AMSU SWATH ',i4.4,1x,i2.2,i2.2,1x,i2.2,i2.2,' UTC ',
     +        a6,1x,a9)
c
      write(luloc,200) sslon,sslat
  200 format(f8.2,f7.2)
c
      do 10 i=1,np
	 write(luloc,200) aslon(i),aslat(i)
   10 continue
c
      return
      end
      subroutine barr(fk,flat,flon,nk,efld,exfac,rinf,
     +                 rlat,rlon,fr,rr,nr,ierr)
c     This routine performs a single pass Barnes analysis
c     of the unevenly values fk to give fr on an evenly
c     spaced radial grid.
c  
c     Input:
c        fk   - function values for the analysis
c        flat - latitudes  (deg N positive) of fk
c        flon - longitudes (deg E positive) of fk
c        nk   - number of fk points
c        efld - e-folding radius (km) for Barnes analysis
c        exfac- expansion factor to increasing efld as a function of radius
c        rinf - Influence radius (km) for Barnes analysis
c        rlat - latitude of center of radial grid (deg N)
c        rlon - longitude of center of radial grid (deg E)
c        rr   - radial grid points (m)
c        nr   - Number of radial grid points
c        ierr - error flat (0=normal completion)
c                          (1=error during analysis)
c
c     Output
c        fr  - Analysis of fk on analysis grid
c
      dimension fk(nk),flat(nk),flon(nk)
      dimension rr(nr),fr(nr)
c
c     Initialize fr to zero
      ierr=0
      do 10 i=1,nr
	 fr(i) = 0.0
   10 continue
c
      rmax = rr(nr)/1000.0
c
c     Perform analysis
      do 20 i=1,nr
	 wtsum = 0.0
	 rkm   = rr(i)/1000.0
	 efldt = efld*(1.0 + exfac*rkm/rmax)
	 do 30 k=1,nk
	    call distk(rlon,rlat,flon(k),flat(k),dx,dy,rad)
	    rad = abs(rad-rkm)
            if (rad .le. rinf) then
	       dnorm = rad/efldt
	       wt = exp(-dnorm*dnorm)
	       wtsum = wtsum + wt
	       fr(i) = fr(i) + wt*fk(k)
            endif
   30    continue
c
         if (wtsum .gt. 0.0) then
            fr(i) = fr(i)/wtsum
         else
            do 40 ii=1,nr
               fr(ii) = 0.0
   40       continue
	    ierr=1
	    return
         endif
   20 continue
c
      return
      end
      subroutine barxy(fk,flat,flon,nk,efld,rinf,ispf,
     +                 rlat,rlon,fxy,nx,ny,ierr)
c     This routine performs a Barnes analysis
c     of the unevenly values fk to give fxy on an evenly
c     spaced lat/lon grid. If ispf=1, a second pass is performed.
c  
c     Input:
c        fk   - function values for the analysis
c        flat - latitudes  (deg N positive) of fk
c        flon - longitudes (deg E positive) of fk
c        nk   - number of fk points
c        efld - e-folding radius (km) for Barnes analysis
c        rinf - Influence radius (km) for Barnes analysis
c        rlat - 1-D array of latitudes of analysis grid
c        rlon - 1-D array of longitudes of analysis grid
c        ispf - flag for second Barnes pass (ispf=1)
c        nx   - Number of longitudes on analysis grid
c        ny   - Number of latitudes on analysis grid
c        ierr - error flat (0=normal completion)
c                          (1=error during analysis)
c
c     Output
c        fxy  - Analysis of fk on analysis grid
c
      dimension fk(nk),flat(nk),flon(nk)
      dimension rlon(nx),rlat(ny),fxy(nx,ny)
c
c     Work arrays
      parameter (mxas=7000)
      dimension fki(mxas),iflag(mxas)
c
c     Initialize fxy to zero
      ierr=0
      do 10 j=1,ny
      do 10 i=1,nx
	 fxy(i,j) = 0.0
   10 continue
c
c     Perform analysis
      do 20 j=1,ny
      do 20 i=1,nx
	 wtsum = 0.0
	 do 30 k=1,nk
	    call distk(rlon(i),rlat(j),flon(k),flat(k),dx,dy,rad)
            if (rad .le. rinf) then
	       dnorm = rad/efld
	       wt = exp(-dnorm*dnorm)
	       wtsum = wtsum + wt
	       fxy(i,j) = fxy(i,j) + wt*fk(k)
            endif
   30    continue
c
         if (wtsum .gt. 0.0) then
            fxy(i,j) = fxy(i,j)/wtsum
         else
            do 40 jj=1,ny
            do 40 ii=1,nx
               fxy(ii,jj) = 0.0
   40       continue
	    ierr=1
	    return
         endif
   20 continue
c
      if (ispf .le. 0) return
c
c     Perform second pass of Barnes analysis
c
c     Interpolate analysis variable back to observation locations
c     and calcualte deviations from data input values
      slat1 = rlat(1)
      slon1 = rlon(1)
      dlat1 = rlat(2)-rlat(1)
      dlon1 = rlon(2)-rlon(1)
      izp   = 0
c
      do k=1,nk
	 call llintsp(fxy,slon1,slat1,dlon1,dlat1,nx,ny,nx,ny,
     +                fsp,flon(k),flat(k),izp,ierrt)
c
	 if (ierrt .eq. 0) then
	    fki(k)   = fk(k)-fsp
	    iflag(k) = 0
         else
	    fki(k)   = 0.0
	    iflag(k) = 1
         endif
      enddo
c
c     Perform analysis of deviations
      do 60 j=1,ny
      do 60 i=1,nx
         fxyt = 0.0
	 wtsum = 0.0
	 do 70 k=1,nk
	    call distk(rlon(i),rlat(j),flon(k),flat(k),dx,dy,rad)
            if (rad .le. rinf .and. iflag(k) .eq. 0) then
	       dnorm = rad/efld
	       wt = exp(-dnorm*dnorm)
	       wtsum = wtsum + wt
	       fxyt = fxyt + wt*fki(k)
            endif
   70    continue
c
         if (wtsum .gt. 0.0) then
            fxyt = fxyt/wtsum
         else
	    fxyt = 0.0
         endif
c
	 fxy(i,j) = fxy(i,j) + fxyt
   60 continue
c
      return
      end
      subroutine altonl(z,t,p,za,ta,pn,ts,ps,nx,ny,np,npn,lulog)
c     This routine extracts z,t at NCEP pressure levels 
c     from za,ta at AMSU pressure levels. Interpolation
c     is used if necessary.
c
      dimension z(nx,ny,np),t(nx,ny,np),p(np)
      dimension za(nx,ny,npn),ta(nx,ny,npn),pn(npn)
      dimension ts(nx,ny),ps(nx,ny)
c
c     Local variable
      dimension iamsuf(100)
c
c     Initialize flag variable to zero
      do 10 k=1,npn
	 iamsuf(k) = 0
   10 continue
c
c     Extract AMSU t,z at NCEP levels for output
      do 20 k=1,npn
	 do 30 kk=1,np
	    if (p(kk) .eq. pn(k)) then
	       iamsuf(k) = 1
c
	       do 40 j=1,ny
	       do 40 i=1,nx
		  za(i,j,k) = z(i,j,kk)
		  ta(i,j,k) = t(i,j,kk)
   40          continue
	       go to 20
	    endif
   30    continue
   20 continue
c 
c     Interpolate between amsu levels for t,z at ncep levels, if necessary
      do 41 k=1,npn
          if (iamsuf(k) .eq. 0) then
c            Find amsu level just above current ncep level
	     do 42 kk=1,np
	        if (p(kk) .lt. pn(k)) then
	 	   kup = kk
		   pup = p(kk)
                endif
   42        continue
c
             if (kup .eq. np) then
		plo = 1070.0e+2
             else
		plo = p(kup+1)
             endif
c
	     write(lulog,200) pn(k)/100.0,pup/100.0,plo/100.0
  200        format(/,' Interpolate T,Z at p=',f5.0,
     +                ' from available AMSU levels ',f5.0,1x,f5.0)
c 
             do 43 j=1,ny
	     do 43 i=1,nx
		pt = pn(k)
c
		p2 = p(kup)
		t2 = t(i,j,kup)
		z2 = z(i,j,kup)
c  
		if (kup .eq. np) then
		   p1 = ps(i,j)
		   t1 = ts(i,j)
		   z1 = 0.0
                else
		   p1 = p(kup+1)
		   t1 = t(i,j,kup+1)
		   z1 = z(i,j,kup+1)
                endif
c
		call ztint(p1,p2,z1,z2,t1,t2,pt,zt,tt)
                ta(i,j,k) = tt
		za(i,j,k) = zt
   43        continue
	 endif
   41 continue
c
      return
      end
      subroutine rzwrit(f,scale,nr,dr,nz,dz,slat,slon,atcfid,sname,
     +                  jyr,jmon,jday,jtimeh,jtimem,luout,lulog,label)
c     This routine writes f to a file for later plotting
c
      dimension f(nr,nz)
      character *6 atcfid
      character *10 sname
      character *20 label
c
c     Local array
      parameter (mxd=5000)
      dimension dum(mxd)
c
c     Make sure dummy array is large enough
      npts = nr*nz
      if (mxd .lt. npts) then
	 write(lulog,900) mxd
  900    format(/,'Dimension mxd too small in routine rzwrit: ',i4)
	 stop
      endif
c
c     Check scale factor
      if (scale .le. 0.0) scale = 1.0
c
c     Write header lines
      write(luout,200) label,jyr,jmon,jday,jtimeh,jtimem,atcfid,sname
  200 format(a20,1x,i4.4,1x,i2.2,i2.2,1x,i2.2,i2.2,1x,a6,1x,a10)
      write(luout,210) slat,slon,dr/1000.,nr,dz/1000.,nz
  210 format(f5.1,1x,f6.1,1x,f6.2,1x,i3,1x,f6.2,1x,i3)
c
c     Put f in 1-d array and scale for printing
      k = 0
      do 10 j=1,nz
      do 10 i=1,nr
	 k = k + 1
	 dum(k) = scale*f(i,j)
   10 continue
c
c     Write dum to file
      write(luout,220) (dum(k),k=1,npts)
  220 format(8(f9.2,1x))
c
      return
      end
      subroutine xywrit(f,scale,nx,rlonl,rlonr,ny,rlatb,rlatt,
     +                  slat,slon,atcfid,sname,
     +                  jyr,jmon,jday,jtimeh,jtimem,luout,lulog,label)
c     This routine writes f to a file for later plotting
c
      dimension f(nx,ny)
      character *6 atcfid
      character *10 sname
      character *20 label
c
c     Local array
      parameter (mxd=5000)
      dimension dum(mxd)
c
c     Make sure dummy array is large enough
      npts = nx*ny
      if (mxd .lt. npts) then
	 write(lulog,900) mxd
  900    format(/,'Dimension mxd too small in routine xywrit: ',i4)
	 stop
      endif
c
c     Check scale factor
      if (scale .le. 0.0) scale = 1.0
c
c     Write header lines
      write(luout,200) label,jyr,jmon,jday,jtimeh,jtimem,slat,slon,
     +                 atcfid,sname
  200 format(a20,1x,i4.4,1x,i2.2,i2.2,1x,i2.2,i2.2,1x,f5.1,1x,f6.1,
     +       1x,a6,1x,a10)
      write(luout,210) rlonl,rlonr,nx,rlatb,rlatt,ny
  210 format(f6.1,1x,f6.1,1x,i3,1x,f6.1,1x,f6.1,1x,i3)
c
c     Put f in 1-d array and scale for printing
      k = 0
      do 10 j=1,ny
      do 10 i=1,nx
	 k = k + 1
	 dum(k) = scale*f(i,j)
   10 continue
c
c     Write dum to file
      write(luout,220) (dum(k),k=1,npts)
  220 format(8(f9.2,1x))
c
      return
      end
      subroutine tcwrit(p,ps,ts,t,clw,nx,ny,np,lutcp,label,
     +                  x1,x2,y1,y2)
c
c     This routine writes the surface P,T, the temperature at the AMSU
c     pressure levels and cloud liquid water to a file
c
      dimension p(np)
      dimension ps(nx,ny),ts(nx,ny)
      dimension t(nx,ny,np),clw(nx,ny)
      character *20 label
c
c     Write file header
      write(lutcp,200) label,nx,x1,x2,ny,y1,y2
  200 format(' STORMID ',a13,1x,' x: ',i2,1x,f6.1,1x,f6.1,
     +                          ' y: ',i2,1x,f6.1,1x,f6.1)
c
c     Write the cloud liquid water
      write(lutcp,201)
  201 format(' CLOUD LIQUID WATER')
      write(lutcp,210) ((clw(i,j),i=1,nx),j=1,ny)
  210 format(10(f6.2,1x))
c
c     Write the surface pressure
      write(lutcp,202)
  202 format(' SURFACE PRESSURE (MB)')
      write(lutcp,211) ((ps(i,j)/100.,i=1,nx),j=1,ny)
  211 format(10(f6.1,1x))
c
c     Write the surface temperature
      write(lutcp,203)
  203 format(' SURFACE TEMPERATURE (K)')
      write(lutcp,210) ((ts(i,j),i=1,nx),j=1,ny)
c
c     Write the temperatures at each pressure level
      do 10 k=1,np
	 write(lutcp,204) p(k)/100.
  204    format(' TEMPERATURE (K) AT P= ',f5.0,' MB')
         write(lutcp,210) ((t(i,j,k),i=1,nx),j=1,ny)
   10 continue
c
      return
      end
      subroutine tcrwrit(prz,rhorz,trz,vrz,nr,nz,sslat,sslon,lutcrp,
     +                   label,coord,times,r1,r2,z1,z2)
c
c     This routine writes the p,rho,t and v as a function of r,z
c     to a file
c
      dimension prz(nr,nz),rhorz(nr,nz),trz(nr,nz),vrz(nr,nz)
      character *23 label
c
      character *90 coord,times
c
c     Write file header
      r1k = r1/1000.
      r2k = r2/1000.
      z1k = z1/1000.
      z2k = z2/1000.
c
      write(lutcrp,199) coord
      write(lutcrp,199) times
  199 format(a90)
c
      write(lutcrp,200) label,nr,r1k,r2k,nz,z1k,z2k,sslat,sslon
  200 format(a23,' r(km): ',i2,1x,f6.1,1x,f6.1,
     +           ' z(km): ',i2,1x,f6.1,1x,f6.1,
     +           ' lat=',f6.2,' lon=',f7.2)
c
c     Write the pressure
      write(lutcrp,201)
  201 format(' PRESSURE (MB)')
      write(lutcrp,211) ((prz(i,j)/100.,i=1,nr),j=1,nz)
  211 format(10(f6.1,1x))
c
c     Write the density
      write(lutcrp,202)
  202 format(' DENSITY (KG/M3)')
      write(lutcrp,212) ((rhorz(i,j),i=1,nr),j=1,nz)
  212 format(10(f6.4,1x))
c
c     Write the surface temperature
      write(lutcrp,203)
  203 format(' TEMPERATURE (K)')
      write(lutcrp,213) ((trz(i,j),i=1,nr),j=1,nz)
  213 format(10(f6.2,1x))
c
c     Write the gradient wind
      write(lutcrp,204)
  204 format(' GRADIENT WIND (M/S)')
      write(lutcrp,214) ((vrz(i,j),i=1,nr),j=1,nz)
  214 format(10(f6.2,1x))
c
      return
      end
      subroutine spata(f1,f2,p,nx,ny,np,ibrem,lulog,label)
c     This routine calculates area averages of f1 and f2.
c     If ibrem=1, a constant value to the first field (f1)
c     is added so that the average difference between the fields
c     is zero, each each pressure level.
c
      dimension f1(nx,ny,np),f2(nx,ny,np),p(np)
      character *20 label
c
      write(lulog,200) label
  200 format(/,'Domain average f1,f2: ',a20) 
c
      rpts = float(nx*ny)
      do 10 k=1,np
         f1m = 0.0
         f2m = 0.0
         do 20 j=1,ny
	 do 20 i=1,nx
	    f1m = f1m + f1(i,j,k)
	    f2m = f2m + f2(i,j,k)
   20    continue
	 f1m = f1m/rpts
	 f2m = f2m/rpts
c
         pmb = p(k)/100.0
c
	 write(lulog,210) f1m,f2m,pmb
  210    format(' f1=',f8.1,' f2=',f8.1,'  p=',f5.0)
c
	 if (ibrem .eq. 1) then
	    do 30 j=1,ny
	    do 30 i=1,nx
	       f1(i,j,k) = f1(i,j,k) + (f2m-f1m)
   30       continue
         endif
   10 continue
c
      if (ibrem .eq. 1) then
	 write(lulog,220)
  220    format(' f1 adjusted to make the mean equal to the f2 mean')
      endif
c
      return
      end
      subroutine spata1(f1,f2,nx,ny,ibrem,lulog,label)
c     This routine calculates area averages of f1 and f2.
c     If ibrem=1, a constant value to the first field (f1)
c     is added so that the average difference between the fields
c     is zero.
c
c     This routine is similar to spata, but for a f1 and f2 defined at
c     a single pressure level
c
      dimension f1(nx,ny),f2(nx,ny)
      character *20 label
c
      write(lulog,200) label
  200 format(/,'Domain average f1,f2: ',a20) 
c
      rpts = float(nx*ny)
      f1m = 0.0
      f2m = 0.0
      do 20 j=1,ny
      do 20 i=1,nx
         f1m = f1m + f1(i,j)
         f2m = f2m + f2(i,j)
   20 continue
      f1m = f1m/rpts
      f2m = f2m/rpts
c
      write(lulog,210) f1m,f2m
  210 format(' f1=',f8.1,' f2=',f8.1)
c
      if (ibrem .eq. 1) then
         do 30 j=1,ny
         do 30 i=1,nx
            f1(i,j) = f1(i,j) + (f2m-f1m)
   30    continue
      endif
c
      if (ibrem .eq. 1) then
	 write(lulog,220)
  220    format(' f1 adjusted so to make the mean equal to the f2 mean')
      endif
c
      return
      end
      subroutine swata(f1,p,mx,np,nx,lulog,label)
c     This routine calculates the average of f1 at the swath points
c
      dimension f1(mx,np),p(np)
      character *20 label
c
      write(lulog,200) label
  200 format(/,'Swath average f1: ',a20) 
c
      rpts = float(nx)
      do 10 k=1,np
         f1m = 0.0
	 do 20 i=1,nx
	    f1m = f1m + f1(i,k)
   20    continue
	 f1m = f1m/rpts
c
         pmb = p(k)
c
	 write(lulog,210) f1m,pmb
  210    format(' f1=',f6.1,'  p=',f6.1)
   10 continue
c
      return
      end
      subroutine qualcon(tas,clwas,tpwas,aslat,aslon,pamsu,dumas,
     +                   dumasp,idumas,mxas,mpas,nxas,np,npst,lulog)
c     This routine quality controls the AMSU temperatures by comparing
c     them with standard atmosphere temperatures. If the temperature
c     difference between the AMSU and standard atmosphere exceeds a
c     specified amount (devm) at any of the levels to be analyzed, 
c     the temperatures for the entire column, the clw and tpw are 
c     eliminated. The varible containing the number of AMSU swath points
c     (nxas) is reduced to account for any deleted data points.
c
      dimension tas(mxas,mpas),clwas(mxas),tpwas(mxas)
      dimension aslat(mxas),aslon(mxas),pamsu(mpas)
      dimension dumas(mxas),dumasp(mxas,mpas),idumas(mxas)
c
c     Local arrays 
      dimension p(200),st(200)
c
c     Specify maximum allowable temperature deviation (C)
      devm = 40.0
c
      write(lulog,200) devm
  200 format(/,' Quality control AMSU temperatures with devm= ',f5.1)
c
c     Calculate standard atmosphere temperatures
c     and extract amsu level pressures
      do 10 k=1,np
	 kk = npst+k-1
	 pmb = pamsu(kk)
	 p(k) = pmb
c
	 if (pmb .lt. 10.0) then
	    write(lulog,210) pmb
  210       format(' Quality control not designed for p<10 mb, p=',f6.1) 
	    stop
         endif
c
c        Find standard atmosphere temperature
	 call stndz(pmb,zstd,tstd,thetas)
	 st(k) = tstd
c
c         write(lulog,220) pmb,tstd,zstd/1000.0
c 220    format(1x,' p (mb)=',f6.1,' tstd (K)=',f6.1,1x,
c    +             ' zstd (km)=',f5.1)
   10 continue
c
c     Start loop over swath points
      do 20 i=1,nxas
c        Initialize error flag to zero
	 idumas(i) = 0
c
c        Check all pressure levels at current location
         do 30 k=1,np
	    kk = npst+k-1
	    dev = abs(tas(i,kk)-st(k))
	    if (dev .gt. devm) then
	       idumas(i) = 1
	       write(lulog,230) aslat(i),aslon(i),p(k),tas(i,kk)
  230          format(' Bad temp at lat/lon: ',f7.2,1x,f7.2,
     +                ' p=',f6.1,1x,' t=',f8.1)
	       go to 1000
           endif
   30    continue
c
 1000    continue
   20 continue
c
c     iland=1
c     distth=0.0
c
c     if (iland .eq. 0) then
c        Eliminate land points
c         do 35 i=1,nxas
c           write(6,*) aslon(i),aslat(i)
c           call aland(aslon(i),aslat(i),dist)
c           write(6,*) dist
c           if (dist .le. distth) idumas(i) = 1
c  35    continue
c     endif
c
c     Count the number of bad data points
      nbad = 0
      do 40 i=1,nxas
	 if (idumas(i) .eq. 1) then
	    nbad = nbad + 1
         endif
   40 continue
c
      if (nbad .le. 0) go to 2000
c
      nxasq = nxas - nbad
c
c     Eliminate bad data points
c     ** Temperatures
      ii = 0
      do 50 i=1,nxas
	 if (idumas(i) .eq. 0) then
	    ii = ii + 1
	    do 55 k=1,np
	       kk = npst+k-1
	       dumasp(ii,kk) = tas(i,kk)
   55       continue
         endif
   50 continue
c
      do 60 i=1,nxasq
      do 60 k=1,np
         kk = npst+k-1
	 tas(i,kk) = dumasp(i,kk)
   60 continue
c
c     ** clw,tpw,lat,lon
      ii = 0
      do 70 i=1,nxas
	 if (idumas(i) .eq. 0) then
	    ii = ii + 1
	    dumasp(ii,1) = clwas(i)
	    dumasp(ii,2) = tpwas(i)
	    dumasp(ii,3) = aslat(i)
	    dumasp(ii,4) = aslon(i)
         endif
   70 continue
c
      do 80 i=1,nxasq
	 clwas(i) = dumasp(i,1)
	 tpwas(i) = dumasp(i,2)
	 aslat(i) = dumasp(i,3)
	 aslon(i) = dumasp(i,4)
   80 continue
c
      nxas = nxasq
c
 2000 continue      
      write(lulog,240) nbad
  240 format(/,i4,' data points eliminated by quality control')
c
      return
      end
      subroutine delim(tas,clwas,tpwas,aslat,aslon,
     +                 rlatmn,rlatmx,rlatbf,rlonmn,rlonmx,rlonbf,
     +                 dumas,dumasp,idumas,
     +                 mxas,mpas,nxas,np,npst,lulog)
c
c     This routine eliminates data that is outside of the analysis
c     domain. Data is retained in a buffer region outside the domain
c     defined by rlonbf and rlatbf.
c
      dimension tas(mxas,mpas),clwas(mxas),tpwas(mxas)
      dimension aslat(mxas),aslon(mxas),pamsu(mpas)
      dimension dumas(mxas),dumasp(mxas,mpas),idumas(mxas)
c
c     Initialize elimination flag to zero
      do 10 i=1,nxas
	 idumas(i) = 0
   10 continue
c
c     Flag the points outside of the domain
      rltn = rlatmn-rlatbf
      rltx = rlatmx+rlatbf
      rlnn = rlonmn-rlonbf
      rlnx = rlonmx+rlonbf
c
      do 15 i=1,nxas
         if (aslat(i) .lt. rltn .or. aslat(i) .gt. rltx) idumas(i)=1 
         if (aslon(i) .lt. rlnn .or. aslon(i) .gt. rlnx) idumas(i)=1 
   15 continue
c
c     Count the number of excluded data points
      nex = 0
      do 20 i=1,nxas
	 if (idumas(i) .eq. 1) then
	    nex = nex + 1
         endif
   20 continue
c
      if (nex .le. 0) go to 2000
c
      nxasq = nxas - nex
c
c     Eliminate data points
c     ** Temperatures
      ii = 0
      do 30 i=1,nxas
	 if (idumas(i) .eq. 0) then
	    ii = ii + 1
	    do 35 k=1,np
	       kk = npst+k-1
	       dumasp(ii,kk) = tas(i,kk)
   35       continue
         endif
   30 continue
c
      do 40 i=1,nxasq
      do 40 k=1,np
         kk = npst+k-1
	 tas(i,kk) = dumasp(i,kk)
   40 continue
c
c     ** clw,tpw,lat,lon
      ii = 0
      do 50 i=1,nxas
	 if (idumas(i) .eq. 0) then
	    ii = ii + 1
	    dumasp(ii,1) = clwas(i)
	    dumasp(ii,2) = tpwas(i)
	    dumasp(ii,3) = aslat(i)
	    dumasp(ii,4) = aslon(i)
         endif
   50 continue
c
      do 60 i=1,nxasq
	 clwas(i) = dumasp(i,1)
	 tpwas(i) = dumasp(i,2)
	 aslat(i) = dumasp(i,3)
	 aslon(i) = dumasp(i,4)
   60 continue
c
 2000 continue      
      write(lulog,240) nex,nxas
  240 format(/,i4,' of ',i4,
     +       ' data points eliminated by routine delim')
c
      nxas = nxasq
c
      return
      end
      subroutine tdevtest(mxas,nxas,tas,clwas,lutdev,atcfid,
     &                    imon,iday,itime)
c     This routine outputs uncorrected temp deviations (from average at
c     each pressure level) vs CLW, where tdev(i)=tas(i)-tmean
      dimension tas(mxas)
      dimension clwas(mxas)
      dimension tdev(mxas)
c
c     Calculate average of temps at this pressure level
c     Initialize tdev array in this loop also
      tmean = 0.0
      count = 0.0
      do 24 i=1,nxas
        tmean = tmean + tas(i)
        count = count + 1.0
	tdev(i) = 0.0
   24 continue
      tmean=tmean/count
c
      do 26 i=1,nxas
        tdev(i) = tas(i)-tmean
        if (clwas(i) .lt. 0) then
          clwas(i) = 0
        endif
	write(lutdev,971)atcfid,imon,iday,itime,tdev(i),clwas(i)
  971   format(a6,3(i2.2),1x,f8.3,1x,f6.2)
   26 continue
      end
      subroutine uvmac(u,v,t,jyr,jlday,jtime,intv)
c     This routine prints u and v to an ASCII file that can be used
c     to plot data in McIDAS. Every intv point is printed.
c
      parameter (nx=61,ny=61)
      parameter (npn=12)
c
      dimension u(nx,ny,npn),v(nx,ny,npn),t(nx,ny,npn)
c
      dimension rlond(nx),rlatd(ny)
      dimension rlonr(nx),rlatr(ny)
      dimension sinlat(ny),coslat(ny),tanlat(ny)
      dimension pn(npn)
c
      common /ncepll/ rlond,rlonr,rlatd,rlatr,dlon,dlat,
     +                dlonr,dlatr,sinlat,coslat,tanlat 
      common /ncepp/ pn
c
      lumac = 66
c
c     Set imac = 1 for original format for J. Dostalek
c      or imac = 2 for modified format for J. Evans
      imac = 2
c
      open(file='uvamsu.dat',unit=lumac,form='formatted',
     +      status='unknown')
c
      if (jyr .lt. 2000) then
         jyr2 = jyr-1900
      else
	 jyr2 = jyr-2000
      endif
c
      if (imac .eq. 1) then
         do 10 j=1,ny,intv
         do 10 i=1,nx,intv
         do 10 k=1,npn
            pmb = pn(k)/100.0
            utem = u(i,j,k)
            vtem = v(i,j,k)
            call ctor(utem,vtem,spd,dir)
            dir = 270.-dir
            if (dir .lt. 0.0) dir = dir+360.0
c
            write(lumac,200) jyr2,jlday,jtime,rlatd(j),-rlond(i),
     +                     pmb,u(i,j,k),v(i,j,k),spd,dir
  200       format(1x,i2.2,i3,1x,i6,7(1x,f6.1))
   10    continue
      else
	 call jdayi(jlday,jyr,jmon,jday)
	 jtime4 = jtime/100
c
         do 20 j=1,ny,intv
         do 20 i=1,nx,intv
         do 20 k=1,npn
            pmb = pn(k)/100.0
c
            write(lumac,210) jyr,jmon,jday,jtime4,rlatd(j),rlond(i),
     +                       pmb,u(i,j,k),v(i,j,k),t(i,j,k)
  210       format(1x,i4,1x,i2.2,i2.2,1x,i4.4,6(1x,f6.1))
   20    continue
      endif
c
      close(lumac)
      return
      end
      subroutine AMSUfdeck(cbasin,istnum,cstype,
     &     iyr,mm,dd,hh,min,rlat,rlon,
     &     mslp,ivmx,ir34,ir50,ir64,irmw,isdist,lufix)
c
c     This subroutine creates an ATCF fdeck entry for an AMSU fix.
c
c
c     Written by 
c     John Knaff
c     CIRA
c     July 2005
c
c     Last Modified: August 1, 2005
c
c     Language: FORTRAN 90
c
c***********************************************************************
c     F-DECK structures 
c***********************************************************************
c	atcf fix formats as of 4/02/2002
c
c	The first 32 words are common followed by specific structure for
c	1)Subjective Dvorak (DVTS)
c	2)Objective Dvorak (DVTO)
c	3)Microwave (SSMI,ERS2,SEAW,TRMM,AMSU,QSCT,ALTI,ADOS)
c	4)Radar (RDRT,RDRD)
c	5)Aircraft (AIRC)
c	6)Dropsonde (DRPS)
c	7)Analysis (ANAL)
c
c     This subroutine used structures for (3) above.
c
c**********************************************************************
c
c     COMMON FEATURES TO ALL FIXES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c       All character fix record (first 32 words)
c
	type cfx_rec
	   SEQUENCE
	   character     basin*2
	   character     cynum*2
	   character     dtg*12
	   character     fform*3
	   character     ftype*4
	   character     cip*10
	   character     rflag*1
	   character     latns*5
	   character     lonew*6
	   character     hob*5
	   character     cconf*1
	   character     vmax*3
	   character     vconf*1
	   character     mslp*4
	   character     pconf*1
	   character     pder*4
	   character     rad*3
	   character     wcode*4
	   character     rad1*4
	   character     rad2*4
	   character     rad3*4
	   character     rad4*4
	   character     radm1*1
	   character     radm2*1
	   character     radm3*1
	   character     radm4*1
	   character     rconf*1
	   character     rmw*3
	   character     eyed*3
	   character     sreg*1
	   character     fsite*5
	   character     finit*3
	end type cfx_rec
c
c       Mixed structure... causes a warning message when compiled..
c
c       Common part of the fix record (32+2 words )
c
	type fx_rec
	   SEQUENCE
	   character     basin*2
	   integer       cynum
	   integer       dtg
	   integer       fform
	   character     ftype*4
	   character     cip*10
	   character     rflag*1
	   integer       lat
	   character     ns*1
	   integer       lon
	   character     ew*1
	   integer       hob
	   integer       cconf
	   integer       vmax
	   integer       vconf
	   integer       mslp
	   integer       pconf
	   character     pder*4
	   integer       rad
	   character     wcode*4
	   integer       rad1
	   integer       rad2
	   integer       rad3
	   integer       rad4
	   character     radm1*1
	   character     radm2*1
	   character     radm3*1
	   character     radm4*1
	   integer       rconf
	   integer       rmw
	   integer       eyed
	   character     sreg*1
	   character     fsite*5
	   character     finit*3
	end type fx_rec
c
c     MICROWAVE FIX SPECIFIC FIX STRUCTURE
c
c     Character only type
c
	type cmifix
	   sequence
	   character    rflag*1
	   character    rrate*3
	   character    process*6
	   character    waveh*2
	   character    temp*4
	   character    rawslp*4
	   character    retslp*4
	   character    maxsea*3
	   character    stype*6
	   character    rad*3
	   character    wcode*4
	   character    rad1*4
	   character    rad2*4
	   character    rad3*4
	   character    rad4*4
	   character    rad5*4
	   character    rad6*4
	   character    rad7*4
	   character    rad8*4
	   character    radm1*1
	   character    radm2*1
	   character    radm3*1
	   character    radm4*1
	   character    radm5*1
	   character    radm6*1
	   character    radm7*1
	   character    radm8*1
	   character    rconf*1
	   character    comments*52
	end type cmifix
c
c     Mixed structure type ... causes warnings when compiled
c
	type mifix
	   sequence
	   character    rflag*1
	   integer      rrate
	   character    process*6
	   integer      waveh
	   integer      temp
	   integer      rawslp
	   integer      retslp
	   integer      maxsea
	   character    stype*6
	   integer      rad
	   character    wcode*4
	   integer      rad1
	   integer      rad2
	   integer      rad3
	   integer      rad4
	   integer      rad5
	   integer      rad6
	   integer      rad7
	   integer      rad8
	   character    radm1*1
	   character    radm2*1
	   character    radm3*1
	   character    radm4*1
	   character    radm5*1
	   character    radm6*1
	   character    radm7*1
	   character    radm8*1
	   integer      rconf
	   character    comments*52   
	end type mifix
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c     End F90 Structures for the Fixes
c
c************************************************************************
c     Program defined variables
c************************************************************************
      integer iyr,mm,dd,hh,min,sdist,istnum,mslp,ivmx
      integer ir34(4),ir50(4),ir64(4)
      integer irmw,isdist,lufix
      real rlat,rlon
      character*4 clat
      character*5 clon
      character*31 fixname
      character*2 cbasin
      character*6 cstype
      logical lr34,lr50,lr64
      type (cfx_rec)fix         ! all characters
      type (fx_rec) cfx         ! mix of types
      type (mifix) mfx          ! mix of types
      type (cmifix) mfix        ! all characters
c
c     Mark...here's your data statement...preceeding the executables
c
c**********************************************************************
c     Data Statements
c**********************************************************************
c     Note: The data statements below were converted to assignment statements
c
c     data lr34,lr50,lr64/.false.,.false.,.false./
c
c**********************************************************************
c     End Variables
c**********************************************************************
c
      lr34=.false.
      lr50=.false.
      lr64=.false.
c
c     Set some constants for AMSU retrievals
c
      fix%fform=' 30'
      fix%ftype='AMSU'          ! HARDWIRED TO AMSU
      fix%cip='        IP'      ! HARDWIRED Intensity and Pressure estimate
      fix%rflag=' '
      fix%hob='    '
      fix%pder='MEAS'
      fix%rad='   '
      fix%wcode='    '
      fix%rad1='    '
      fix%rad2='    '
      fix%rad3='    '
      fix%rad4='    '
      fix%radm1=' '
      fix%radm2=' '
      fix%radm3=' '
      fix%radm4=' '
      fix%eyed='   '
      fix%fsite=' CIRA'
      fix%finit='JAK'
      fix%cconf='1'
c
      mfix%rflag=' '
      mfix%rrate='   '
      mfix%process='      '
      mfix%waveh='  '
      mfix%temp='    '
      mfix%rawslp='    '
      mfix%maxsea='   '
      mfix%stype=cstype
      mfix%rad='   '
      mfix%wcode='    '
      mfix%rad1='    '
      mfix%rad2='    '
      mfix%rad3='    '
      mfix%rad4='    '
      mfix%rad5='    '
      mfix%rad6='    '
      mfix%rad7='    '
      mfix%rad8='    '
      mfix%radm1=' '
      mfix%radm2=' '
      mfix%radm3=' '
      mfix%radm4=' '
      mfix%radm5=' '
      mfix%radm6=' '
      mfix%radm7=' '
      mfix%radm8=' '
      mfix%comments='storm center extrapolated from t=-12 and t=0 '
     .     //'adeck'
c
      fix%basin=cbasin
      write(fix%cynum,15)istnum
 15   format(i2.2)
c      read(*,15,end=999)fix%basin,fix%cynum
c 15   format(/,19x,a2,a2,/,/,/)
c      read(*,30,end=999) iyr,mm,dd,hh,min
c 30   format(23x,i4,1x,i2,i2,1x,i2,i2,/)
      write(fix%dtg,30)iyr,mm,dd,hh,min
 30   format(i4.4,i2.2,i2.2,i2.2,i2.2)
      write(fix%mslp,45)mslp
 45   format(i4)
      mfix%retslp=fix%mslp
      write(fix%vmax,60)ivmx
 60   format(i3)
      do i=1,4
         if(ir34(i).gt.0)lr34=.true.
         if(ir50(i).gt.0)lr50=.true.
         if(ir64(i).gt.0)lr64=.true.
      enddo
      write(fix%rmw,75)irmw
 75   format(i3)
c
c     determine confidence given the distance from the satellite swath center
c     (i.e., isdist) passed to the subroutine.
c
      cfx%cconf=2
      if(isdist.le.300)then
         fix%vconf='1'
         fix%pconf='1'
         fix%rconf='1'
         mfix%rconf='1'
      else
         fix%vconf='2'
         fix%pconf='2'
         fix%rconf='2'
         mfix%rconf='2'
      endif
c
c     Latitude and Longitude
      cfx%lat=nint(rlat*100)
      cfx%lon=nint(rlon*100)
      if(cfx%lat.ge.0)cfx%ns='N'
      if(cfx%lat.lt.0)cfx%ns='S'
      if(cfx%lon.ge.0)cfx%ew='E'
      if(cfx%lon.lt.0)cfx%ew='W'
      if(cfx%lon.gt.18000) then
         cfx%lon=cfx%lon-36000.
         cfx%ew='W'
      endif
      write(fix%latns,151)abs(cfx%lat),cfx%ns
 151  format(i4,a1)
      write(fix%lonew,152)abs(cfx%lon),cfx%ew
 152  format(i5,a1)
c
c     determine sub-basin
c
      if (fix%basin.eq.'WP')then
         fix%sreg=fix%basin(1:1)
      elseif (fix%basin.eq.'AL')then
         fix%sreg='L'
      elseif (fix%basin.eq.'CP')then
         fix%sreg=fix%basin(1:1)
      elseif (fix%basin.eq.'EP')then
         fix%sreg=fix%basin(1:1)
      elseif (fix%basin.eq.'IO')then
         if (rlon .lt. 80.)fix%sreg='A'
         if (rlon .ge. 80.)fix%sreg='B'
      elseif (fix%basin.eq.'SH')then
         if (rlon .lt. 135.0 .and. rlon .ge. 0.)fix%sreg='S'
         if (rlon .ge. 135.0 .or. rlon .lt. 0.)fix%sreg='P'
      endif
c
c
c     write(fixname,200)fix%dtg,fix%cynum,fix%sreg
c200  format('CIRA_',a12,'_',a2,a1,'_FIX')
c     open(unit=lufix,file=fixname,status='replace')
c
c     write the center/intensity/pressure fix
      if(.not.lr34)then
         write(lufix,165)fix,mfix
      endif
c
c     if 34 knot winds exist print the intensity/pressure/Radii fix
c     for 34 knot winds
c
      if(lr34)then
         fix%cip='       IPR'
         fix%rad=' 34'
         fix%wcode=' NEQ'
         mfix%rad=' 34'
         mfix%wcode=' NEQ'
         write(fix%rad1,175)ir34(1)
         write(fix%rad2,175)ir34(2)
         write(fix%rad3,175)ir34(3)
         write(fix%rad4,175)ir34(4)
 175     format(i4)
         write(mfix%rad1,175)ir34(1)
         write(mfix%rad2,175)ir34(2)
         write(mfix%rad3,175)ir34(3)
         write(mfix%rad4,175)ir34(4)
         write(lufix,165)fix,mfix
       endif
c
c     if 50 knot winds exist print the Radii fix
c     for 50 knot winds
c
      if(lr50)then
         fix%cip='         R'
         fix%rad=' 50'
         fix%wcode=' NEQ'
         mfix%rad=' 50'
         mfix%wcode=' NEQ'
         write(fix%rad1,175)ir50(1)
         write(fix%rad2,175)ir50(2)
         write(fix%rad3,175)ir50(3)
         write(fix%rad4,175)ir50(4)
         write(mfix%rad1,175)ir50(1)
         write(mfix%rad2,175)ir50(2)
         write(mfix%rad3,175)ir50(3)
         write(mfix%rad4,175)ir50(4)
         write(lufix,165)fix,mfix
       endif
c
c     if 64 knot winds exist print the Radii fix
c     for 64 knot winds
c
      if(lr64)then
         fix%cip='         R'
         fix%rad=' 64'
         fix%wcode=' NEQ'
         mfix%rad=' 64'
         mfix%wcode=' NEQ'
         write(fix%rad1,175)ir64(1)
         write(fix%rad2,175)ir64(2)
         write(fix%rad3,175)ir64(3)
         write(fix%rad4,175)ir64(4)
         write(mfix%rad1,175)ir64(1)
         write(mfix%rad2,175)ir64(2)
         write(mfix%rad3,175)ir64(3)
         write(mfix%rad4,175)ir64(4)
         write(lufix,165)fix,mfix
       endif
 165  format(32(a,', '),28(a,', '),a)
 999  close(lufix)
      stop
      end
