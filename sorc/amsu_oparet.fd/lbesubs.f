c     ** This group of subroutines is for solving the linear balance equation
c     ** on a beta plane
c
      subroutine lbe(phi,u,v,nxt,nyt)
c     This routine solves the linear balance equation for u and v
c     given phi and boundary values for u and v.
c
      parameter (nx=61,ny=61)
c
c     Passed arrays
      dimension phi(nxt,nyt),u(nxt,nyt),v(nxt,nyt)
c
c     Common arrays
      dimension x(nx),y(ny),fy(ny)
c
c     Local arrays
      dimension psi(nx,ny),psifg(nx,ny),d2phi(nx,ny),g(ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
      common /log/ lulog
c
c     Find Laplacian of phi
      call del2xy(phi,nx,ny,dx,dy,d2phi)
c
c     Divide d2phi by Coriolis parameter
      do 10 j=1,ny
      do 10 i=1,nx
         d2phi(i,j) = d2phi(i,j)/fy(j)
   10 continue
c
c     Calculate coefficient for d/dx term of lbe
      do 15 j=1,ny
         g(j) = beta/fy(j)
   15 continue
c
c     Make first guess for psi
      do 20 j=1,ny
      do 20 i=1,nx
         psifg(i,j) = 0.0
   20 continue
c
c     Calculate psi boundary conditions
      call psibd(psifg,nx,ny,dx,dy,u,v)
c
c     Calculate stream function
      call psonxy(d2phi,g,psi,psifg,ierr)
c
      call dxcal(psi,nx,ny,dx,dy,v)
      call dycal(psi,nx,ny,dx,dy,u)
c
      do 30 j=1,ny
      do 30 i=1,nx
         u(i,j) = -1.0*u(i,j)
   30 continue
c
c     do 50 j=ny,1,-1
c        write(lulog,400) (u(i,j),i=1,nx)
c 400    format(1x,22(f5.1))
c  50 continue
c
c     write(lulog,409)
c 409 format(/,'psi')
c
c     do 51 j=ny,1,-1
c        write(lulog,410) (psi(i,j),i=1,10)
c 410    format(1x,22(e11.4))
c  51 continue
c
      return
      end
      subroutine psibd(psi,nx,ny,dx,dy,u,v)
c     This routine calculates psi on the domain boundaries
c     by integrating u,v
c
      dimension psi(nx,ny),u(nx,ny),v(nx,ny)
c
c     Start in the lower left corner
      psi(1,1) = 0.0
c
c     Integrate along left boundary
      i = 1
      do 10 j=2,ny
         psi(i,j) = psi(i,j-1) - dy*0.5*(u(i,j)+u(i,j-1))
   10 continue
c
c     Integrate along top boundary
      j=ny
      do 20 i=2,nx
         psi(i,j) = psi(i-1,j) + dx*0.5*(v(i,j)+v(i-1,j))
   20 continue
c
c     Integrate along right boundary
      i=nx
      do 30 j=ny-1,1,-1
         psi(i,j) = psi(i,j+1) + dy*0.5*(u(i,j)+u(i,j+1))
   30 continue
c
c     Integrate along bottom boundary
      j=1
      do 40 i=nx-1,2,-1
         psi(i,j) = psi(i+1,j) - dx*0.5*(v(i,j)+v(i+1,j))
   40 continue
c
c     Spread the difference between psi(1,1) and psi(2,1) over the entire boundary
      rptt = 2.0*float(nx+ny)
      diff = (psi(1,1)-psi(2,1))
c
      rpt = 0.0
      i = 1
      do 50 j=2,ny
	 rpt = rpt+1.0
	 psi(i,j) = psi(i,j) + diff*rpt/rptt
   50 continue
c
      j=ny
      do 51 i=2,nx
	 rpt = rpt+1.0
	 psi(i,j) = psi(i,j) + diff*rpt/rptt
   51 continue
c
      i = nx
      do 52 j=ny-1,1,-1
	 rpt = rpt+1.0
	 psi(i,j) = psi(i,j) + diff*rpt/rptt
   52 continue
c
      j=1
      do 53 i=nx-1,2,-1
	 rpt = rpt+1.0
	 psi(i,j) = psi(i,j) + diff*rpt/rptt
   53 continue
c
      return
      end
      subroutine dxcal(f,nx,ny,dx,dy,dfdx)
c     This routine calculates the x-derivative of f
c
      dimension f(nx,ny),dfdx(nx,ny)
c
      dxi  = 1.0/(    dx)
      tdxi = 1.0/(2.0*dx)
c
c     Points not affected by boundaries
      do 10 j=1,ny
      do 10 i=2,nx-1
         dfdx(i,j) = tdxi*(f(i+1,j) - f(i-1,j))
   10 continue
c
c     Side boundaries
      do 20 j=1,ny
c        Left boundary
         i=1
         dfdx(i,j) = dxi*(f(i+1,j) - f(i,j))
c
c        Right boundary
         i=nx
         dfdx(i,j) = dxi*(f(i,j) - f(i-1,j))
   20 continue
c
      return
      end
      subroutine dycal(f,nx,ny,dx,dy,dfdy)
c     This routine calculates the y-derivative of f
c
      dimension f(nx,ny),dfdy(nx,ny)
c
      dyi  = 1.0/(    dy)
      tdyi = 1.0/(2.0*dy)
c
c     Points not affected by boundaries
      do 10 j=2,ny-1
      do 10 i=1,nx
         dfdy(i,j) = tdyi*(f(i,j+1) - f(i,j-1))
   10 continue
c
c     Top/Bottom boundaries
      do 20 i=1,nx
c        bottom boundary
         j=1
         dfdy(i,j) = dyi*(f(i,j+1) - f(i,j))
c
c        top boundary
         j=ny
         dfdy(i,j) = dyi*(f(i,j) - f(i,j-1))
   20 continue
c
      return
      end
      subroutine del2xy(f,nx,ny,dx,dy,d2f)
c     This routine calculates the Laplacian of f in Cartesian coordinates
c
      dimension f(nx,ny),d2f(nx,ny)
c
      dx2i = 1.0/(dx*dx)
      dy2i = 1.0/(dy*dy)
c
c     Interior points
      do 10 j=2,ny-1
      do 10 i=2,nx-1
         d2f(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  )) +
     +              dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
   10 continue
c
c
      do 20 j=2,ny-1
c        Left edge
         i = 1
         d2f(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  )) +
     +              dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
c
c        Right edge
         i = nx
         d2f(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  )) +
     +              dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
   20 continue
c
      do 30 i=2,nx-1
c        Bottom edge
         j = 1
         d2f(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  )) +
     +              dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))
c
c        Top edge
         j = ny
         d2f(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  )) +
     +              dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))
   30 continue
c
c     Lower left corner
      i=1
      j=1
      d2f(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  )) +
     +           dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))
c
c     Upper left corner
      i=1
      j=ny
      d2f(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  )) +
     +           dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))
c
c     Upper right corner
      i=nx
      j=ny
      d2f(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  )) +
     +           dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))
c
c     Lower right corner
      i=nx
      j=1
      d2f(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  )) +
     +           dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))
c
      return
      end
      subroutine psonxy(f,g,h,hfg,ierr)
c     This routine solves (del**2 + g(y)d/dy)h = f in Cartesian
c     geometry using over-relaxation. The boundary values for h
c     and the first guess for h at the interior points
c     are assumed to be in the array hfg.
c
c     If the relaxation converges, ierr=0, otherwise, ierr=1.
c
      parameter (nx=61,ny=61)
      dimension f(nx,ny),hfg(nx,ny),h(nx,ny),g(ny)
      dimension x(nx),y(ny),fy(ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
c
c     Specify max number of iterations and error check increment
      nit = 200
      iecal = 10
      emax = 1.0e-5
      pi   = 3.14159
c
c     Calculate constants for over-relaxation
      def    = 1.0 - 2.0*((sin(pi/(2.0*float(ny-1))))**2)
      omega  = 2.0/(1.0 + sqrt(1.0-def*def))
      oomega = 1.0-omega
c
      write(6,410) omega,oomega
  410 format(' omega, oomega: ',e11.4,1x,e11.4)
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
         enorm = bsum/(dx*dy)
      endif
c
      write(6,400) enorm
  400 format(' enorm=',e11.4)
c
c     Calculate common factors for iteration
      c4 = 1.0/( 2./(dx*dx) + 2./(dy*dy) )
      c3 = c4/(2.0*dy)
      c2 = c4/(dy*dy)
      c1 = c4/(dx*dx)
c
c     Perform iteration
      errtp = 1.0e+10
c 
      do 25 k=1,nit
	 do 30 j=2,ny-1
	 do 30 i=2,nx-1
            h(i,j) = oomega*h(i,j) + omega*(
     +               c1*(h(i+1,j)+h(i-1,j)) +
     +               c2*(h(i,j+1)+h(i,j-1)) +
     +               c3*(h(i,j+1)-h(i,j-1))*g(j) -
     +               c4*(f(i,j)))
   30    continue
c
c        Check for convergence
	 if (mod(k,iecal) .eq. 0) then
            call rchkxy(f,g,h,dx,dy,enorm,errt)
c
c           write(6,888) k,errt,emax,enorm,h(11,11)
c 888       format(1x,'k=',i3,' errt,emax,enorm,h= ',4(e11.4))
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
      subroutine rchkxy(f,g,h,dx,dy,enorm,errt)
c     This routine calculates the relative error between
c     (del**2 + g(y)d/dx)h and f for determining if the
c     relaxation in routine psonxy has converged.
c
      parameter (nx=61,ny=61)
      dimension f(nx,ny),h(nx,ny),g(ny)
c
c     Local arrays
      dimension d2h(nx,ny),dhdy(nx,ny)
c
      rpts = float( (nx-1)*(ny-1) )
      errt = 0.0
      scale = 1.0
c
      call del2xy(h,nx,ny,dx,dy,d2h)
      call dycal(h,nx,ny,dx,dy,dhdy)
c
      do 10 j=2,ny-1
      do 10 i=2,nx-1
         d2h(i,j) = d2h(i,j) + g(j)*dhdy(i,j)
   10 continue
c
      do 20 j=2,ny-1
      do 20 i=2,nx-1
	 errt = errt + (f(i,j)-d2h(i,j))**2
   20 continue
c
      errt = ( sqrt(errt/rpts) )/enorm
c
      return
      end
      subroutine uvmac(u,v,jyr,jlday,jtime,intv)
c     This routine prints u and v to an ASCII file that can be used
c     to plot data in McIDAS. Every intv point is printed.
c
      parameter (nx=61,ny=61)
      parameter (npn=12)
c
      dimension u(nx,ny,npn),v(nx,ny,npn)
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
      open(file='uvamsu.dat',unit=lumac,form='formatted',
     +      status='replace')
c
      if (jyr .lt. 2000) then
         jyr2 = jyr-1900
      else
	 jyr2 = jyr-2000
      endif
c
c
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
         write(lumac,200) jyr2,jlday,jtime,rlatd(j),-rlond(i),pmb,
     +                 u(i,j,k),v(i,j,k),spd,dir
  200    format(1x,i2,i3,1x,i6,7(1x,f6.1))
   10 continue
c
      close(lumac)
      return
      end
