c     This code contains several subroutines for
c     manipulating arrays in spherical coordinates. Most of the
c     routines require the variables in common blocks /cons/
c     and /ncepll/.
c
c     Included routines:
c          ** ddx
c          ** ddy
c          ** del2
c          ** pson
c
      subroutine ddx(h,scale,dhdx)
c     This routine calculates the x-derivative of h, multiplied by
c     scale to give dhdx, where x = a*cos(lat)*lon
c
      parameter (nx=61,ny=61)
      dimension h(nx,ny), dhdx(nx,ny)
c
      dimension rlond(nx),rlatd(ny)
      dimension rlonr(nx),rlatr(ny)
      dimension sinlat(ny),coslat(ny),tanlat(ny)
c
      common /cons/ pi,g,rd,dtr,erad,erot
      common /ncepll/ rlond,rlonr,rlatd,rlatr,dlon,dlat,
     +                dlonr,dlatr,sinlat,coslat,tanlat
c
c     Interior points
      cf = scale/(erad*2.0*dlonr)
c
      do 10 j=1,ny
      do 10 i=2,nx-1
         dhdx(i,j) = cf*(h(i+1,j)-h(i-1,j))/coslat(j)
   10 continue
c
c     x-boundary points
      cf = 2.0*cf
      do 20 j=1,ny
         dhdx( 1,j) = cf*(h( 2,j) - h(   1,j))/coslat(j)
         dhdx(nx,j) = cf*(h(nx,j) - h(nx-1,j))/coslat(j)
   20 continue
c
      return
      end
      subroutine ddy(h,scale,dhdy)
c     This routine calculates the y-derivative of h, multiplied by
c     scale to give dhdy, where y = a*lat
c
      parameter (nx=61,ny=61)
      dimension h(nx,ny), dhdy(nx,ny)
c
      dimension rlond(nx),rlatd(ny)
      dimension rlonr(nx),rlatr(ny)
      dimension sinlat(ny),coslat(ny),tanlat(ny)
c
      common /cons/ pi,g,rd,dtr,erad,erot
      common /ncepll/ rlond,rlonr,rlatd,rlatr,dlon,dlat,
     +                dlonr,dlatr,sinlat,coslat,tanlat
c
c     Interior points
      cf = scale/(erad*2.0*dlatr)
c
      do 10 i=1,nx
      do 10 j=2,ny-1
         dhdy(i,j) = cf*(h(i,j+1)-h(i,j-1))
   10 continue
c
c     y-boundary points
      cf = 2.0*cf
      do 20 i=1,nx
         dhdy(i, 1) = cf*(h(i, 2) - h(i,   1))
         dhdy(i,ny) = cf*(h(i,ny) - h(i,ny-1))
   20 continue
c
      return
      end
      subroutine del2(h,scale,del2h)
c     This routine calculates the Laplacian of h, multiplied by
c     scale, to give del2h
c
      parameter (nx=61,ny=61)
      dimension h(nx,ny), del2h(nx,ny)
c
      dimension rlond(nx),rlatd(ny)
      dimension rlonr(nx),rlatr(ny)
      dimension sinlat(ny),coslat(ny),tanlat(ny)
c
      common /cons/ pi,g,rd,dtr,erad,erot
      common /ncepll/ rlond,rlonr,rlatd,rlatr,dlon,dlat,
     +                dlonr,dlatr,sinlat,coslat,tanlat
c
      cfx2 =  scale/(erad*erad*dlonr*dlonr)
      cfy2 =  scale/(erad*erad*dlatr*dlatr)
      cfy1 = -scale/(erad*erad*2.0*dlatr)
c
c     Interior points
      do 10 j=2,ny-1
         cli2 = 1.0/(coslat(j)**2)
         do 10 i=2,nx-1
            del2h(i,j) = cfx2*(h(i+1,j)+h(i-1,j) - 2.0*h(i,j))/cli2 +
     +                   cfy2*(h(i,j+1)+h(i,j-1) - 2.0*h(i,j)) +
     +                   cfy1*(h(i,j+1)-h(i,j-1))*tanlat(j)
   10 continue
c
c     
c     Bottom and top boundary points
      do 15 i=2,nx-1
         j = 1
         cli2 = 1.0/(coslat(j)**2)
         del2h(i,j) = cfx2*(h(i+1,j)+h(i-1,j) - 2.0*h(i,j  ))/cli2 +
     +                cfy2*(h(i,j+2)+h(i  ,j) - 2.0*h(i,j+1)) +
     +                cfy1*(h(i,j+1)-h(i,j))*2.0*tanlat(j)
c
         j = ny
         cli2 = 1.0/(coslat(j)**2)
         del2h(i,j) = cfx2*(h(i+1,j)+h(i-1,j  ) - 2.0*h(i,j  ))/cli2 +
     +                cfy2*(h(i  ,j)+h(i  ,j-2) - 2.0*h(i,j-1)) +
     +                cfy1*(h(i,j)-h(i,j-1))*2.0*tanlat(j)
   15 continue                               
c
c     left and right boundary points
      do 20 j=2,ny-1
         i = 1
         cli2 = 1.0/(coslat(j)**2)
         del2h(i,j) = cfx2*(h(i+2,j  )+h(i,j  ) - 2.0*h(i+1,j))/cli2 +
     +                cfy2*(h(i  ,j+1)+h(i,j-1) - 2.0*h(i,j)) +
     +                cfy1*(h(i  ,j+1)-h(i,j-1))*tanlat(j)
c
         i = nx
         cli2 = 1.0/(coslat(j)**2)
         del2h(i,j) = cfx2*(h(i,j  )+h(i-2,j  ) - 2.0*h(i-1,j))/cli2 +
     +                cfy2*(h(i,j+1)+h(i  ,j-1) - 2.0*h(i  ,j)) +
     +                cfy1*(h(i,j+1)-h(i  ,j-1))*tanlat(j)
c
   20 continue
c
c     Upper-left corner
      i = 1
      j = ny
      cli2 = 1.0/(coslat(j)**2)
      del2h(i,j) = cfx2*(h(i+2,j)+h(i,j  ) - 2.0*h(i+1,j  ))/cli2 +
     +             cfy2*(h(i  ,j)+h(i,j-2) - 2.0*h(i  ,j-1)) +
     +             cfy1*(h(i  ,j)-h(i,j-1))*tanlat(j)
c
c     Lower-left corner
      i = 1
      j = 1
 1     cli2 = 1.0/(coslat(j)**2)
      del2h(i,j) = cfx2*(h(i+2,j  )+h(i,j) - 2.0*h(i+1,j  ))/cli2 +
     +             cfy2*(h(i  ,j+2)+h(i,j) - 2.0*h(i  ,j+1)) +
     +             cfy1*(h(i  ,j+1)-h(i,j))*tanlat(j)
c
c     Upper-right corner
      i = nx
      j = ny
      cli2 = 1.0/(coslat(j)**2)
      del2h(i,j) = cfx2*(h(i-2,j  )+h(i,j) - 2.0*h(i-1,j  ))/cli2 +
     +             cfy2*(h(i  ,j)+h(i,j-2) - 2.0*h(i  ,j-1)) +
     +             cfy1*(h(i  ,j)-h(i,j-1))*tanlat(j)
c
c     Lower-right corner
      i = nx
      j = 1
      cli2 = 1.0/(coslat(j)**2)
      del2h(i,j) = cfx2*(h(i-2,j  )+h(i,j) - 2.0*h(i-1,j  ))/cli2 +
     +             cfy2*(h(i  ,j+2)+h(i,j) - 2.0*h(i  ,j+1)) +
     +             cfy1*(h(i  ,j+1)-h(i,j))*tanlat(j)
c
      return
      end
      subroutine pson(f,hfg,h,ierr)
c     This routine solves (del**2)h = f in spherical geometry using
c     over-relaxation. The boundary values for h and the first guess
c     for h at the interior points are assumed to be in the array hfg.
c     If the relaxation converges, ierr=0, otherwise, ierr=1.
c
      parameter (nx=61,ny=61)
      dimension f(nx,ny),hfg(nx,ny),h(nx,ny)
c
      dimension rlond(nx),rlatd(ny)
      dimension rlonr(nx),rlatr(ny)
      dimension sinlat(ny),coslat(ny),tanlat(ny)
c
      common /cons/ pi,g,rd,dtr,erad,erot
      common /log/ lulog
      common /ncepll/ rlond,rlonr,rlatd,rlatr,dlon,dlat,
     +                dlonr,dlatr,sinlat,coslat,tanlat
c
c     Local variables for storing coefficients 
      dimension c1(ny),c2(ny),c3(ny),c4(ny)
c
c     Specify max number of iterations and error check increment
      nit = 200
      iecal = 10
      emax = 2.0e-5
c
c     Calculate constants for over-relaxation
      def    = 1.0 - 2.0*((sin(pi/(2.0*float(ny-1))))**2)
      omega  = 2.0/(1.0 + sqrt(1.0-def*def))
      oomega = 1.0-omega
c
c     Move first guess h to h array
      do 10 j=1,ny
      do 10 i=1,nx
	 h(i,j) = hfg(i,j)
   10 continue
c
c     Find the maximum magnitude of f for normalizing the error
      enorm = 0.0
      do 15 j=2,ny-1
      do 15 i=2,nx-1
	 if (abs(f(i,j)) .gt. enorm) enorm = abs(f(i,j))
   15 continue
c
      if (enorm .le. 0.0) then
c        The forcing term is zero. Scale error from boundary values.
	 bsum = 0.0
	 count = 0.0
	 do 16 j=1,ny
	 do 16 i=1,nx
	    if (j .ne. 1 .and. j .ne. ny .and.
     +          i .ne. 1 .and. i .ne. nx     ) go to 16
	    count = count + 1.0
	    bsum = bsum + abs(h(i,j))
   16    continue
         bsum = bsum/count
c
         enorm = bsum/(dlatr*dlatr*erad*erad)
      endif
c
c     Calculate common factors for iteration
      do 20 j=1,ny
	 sdlonr = coslat(j)*dlonr
	 t1 = ( (dlatr*sdlonr)**2 )/
     +        ( dlatr**2 + sdlonr**2 )
c
	 c1(j) = 0.5*t1/(sdlonr*sdlonr) 
	 c2(j) = 0.5*t1/(dlatr*dlatr)
	 c3(j) = -0.25*t1/(dlatr)
	 c4(j) = -0.5*t1*erad*erad
   20 continue
c
c     Perform iteration
      errtp = 1.0e+10
c 
      do 25 k=1,nit
	 do 30 j=2,ny-1
	 do 30 i=2,nx-1
            h(i,j) = oomega*h(i,j) + omega*(
     +               c1(j)*(h(i+1,j)+h(i-1,j)) +
     +               c2(j)*(h(i,j+1)+h(i,j-1)) +
     +               c3(j)*(h(i,j+1)-h(i,j-1)) +
     +               c4(j)*(f(i,j)) )
   30    continue
c
c        Check for convergence
	 if (mod(k,iecal) .eq. 0) then
	    call rchk(f,h,enorm,errt)
c
            write(lulog,888) k,errt,emax,enrom
  888       format(1x,'k=',i3,' errt,emax,enorm= ',3(e11.4))
c
	    if (errt .le. emax .or. errt .gt. errtp) go to 1000
	    errtp = errt
         endif
c
   25 continue
c
      ierr = 1
      return
c
 1000 continue
      ierr = 0
c
      return
      end
      subroutine rchk(f,h,enorm,errt)
c     This routine calculates the relative error between (del**2)h and f for 
c     determining if the relaxation in routine pson has converged. 
c
      parameter (nx=61,ny=61)
      dimension f(nx,ny),h(nx,ny)
c
c     Local array
      dimension d2h(nx,ny)
c
      rpts = float( (nx-1)*(ny-1) )
      errt = 0.0
      scale = 1.0
c
      call del2(h,scale,d2h)
      do 10 j=2,ny
      do 10 i=2,nx
	 errt = errt + (f(i,j)-d2h(i,j))**2
   10 continue
c
      errt = ( sqrt(errt/rpts) )/enorm
c
      return
      end
