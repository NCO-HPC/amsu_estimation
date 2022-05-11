c     ** This group of subroutines is for solving the balance equation
c     ** on a beta plane
c
      subroutine nbevs(phi,u,v,inon,nxt,nyt)
c     This routine solves the balance equation for u and v
c     given phi and boundary values for u and v. A variational method
c     is used for solving the be. If inon=1, then the nonlinear version
c     is used, otherwise, the linear version is used.
c
c     This routine solves the balance equation in streamfunction form
c     and then calculates u,v from psi
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
      dimension d2phi(nx,ny)
      dimension psi(nx,ny),zeta(nx,ny),dvdx(nx,ny),dudy(nx,ny)
      dimension psifg(nx,ny),gy(ny)
      dimension d2pdxx(nx,ny),d2pdyy(nx,ny),d2pdxy(nx,ny)
      dimension d2psi(nx,ny),dpdy(nx,ny)
      dimension res(nx,ny),dedpsi(nx,ny)
      dimension cpta(nx,ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
      common /log/ lulog
c
c     Specify maximum number of iterations
      nit=1500
c
c     Specify maximum number of corrections before updating gradient
      ncg=20
c
c     Specify coefficient of the smoothness penalty term
      cpt = 1.0e-10
c
c     Specify reduction factor for adjusting alpha before
c     new gradient calculation
      redfac = 0.1
c
c     Specify first guess for step length alpha
      alpha = 1.0e+25
c
c     Initialize penalty term coefficient array
      call zinter(cpta,cpt,nx,ny)
      call zbound(cpta,0.0,nx,ny)
c
c     Set gy=0 for poisson solver
      do 3 j=1,ny
	 gy(j) = 0.0
    3 continue
c
c     Find Laplacian of phi
      call del2xy(phi,nx,ny,dx,dy,d2phi)
c
c     Calculate vorticity from u,v
      call dxcal(v,nx,ny,dx,dy,dvdx)
      call dycal(u,nx,ny,dx,dy,dudy)
c
      do 5 j=1,ny
      do 5 i=1,nx
	 zeta(i,j) = dvdx(i,j)-dudy(i,j)
	 psifg(i,j) = 0.0
    5 continue
c
c     Calcuate boundary values of psi from u,v
      call psibd(psifg,nx,ny,dx,dy,u,v)
c
c     Calculate first guess for psi 
      call psonxy(zeta,gy,psi,psifg,ierr)
c
c     Calculate x,y derviatives of psi
      call d2xxcal(psi,nx,ny,dx,dy,d2pdxx)
      call d2yycal(psi,nx,ny,dx,dy,d2pdyy)
      call d2xycal(psi,nx,ny,dx,dy,d2pdxy)
      call dycal(psi,nx,ny,dx,dy,dpdy)
c
c     Calculate Laplacian of psi
      call del2xy(psi,nx,ny,dx,dy,d2psi)
c
c     Calculate initial value of residual
      call rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy,d2pdxy,dpdy,
     +             inon,nx,ny,res)
c
c     Calculate initial value of cost function
      call cfcals(res,d2psi,cpta,nx,nx,cfunr,cfunp,cfunt)
c
      write(lulog,600) cfunr,cfunp,cfunt
  600 format(//,' Initial Cost function (r,p,t): ',3(e11.4,1x),/)
c
      if (nit .le. 0) then
         call uvcal(psi,nx,ny,dx,dy,u,v)
	 return
      endif
c
      ngdate = 1
 1000 continue
c        Calculate x,y derviatives of psi
         call d2xxcal(psi,nx,ny,dx,dy,d2pdxx)
         call d2yycal(psi,nx,ny,dx,dy,d2pdyy)
         call d2xycal(psi,nx,ny,dx,dy,d2pdxy)
         call dycal(psi,nx,ny,dx,dy,dpdy)
c
c        Calculate Laplacian of psi
         call del2xy(psi,nx,ny,dx,dy,d2psi)
c
c        Calculate residual
         call rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy,d2pdxy,dpdy,
     +                inon,nx,ny,res)
c
c        Calculate cost function gradient
         call cfgcals(res,d2pdxx,d2pdyy,d2pdxy,d2psi,cpta,
     +               inon,nx,ny,dedpsi)
c
c       write(lulog,776) ngdate
c 776    format(/,' ngdate=',i4,' res')
c        do 76 j=17,15,-1
c           write(lulog,777) (res(i,j),i=15,17)
c 777       format(1x,3(e11.4,1x))
c  76    continue
c
c        write(lulog,*) ' dedpsi'
c        do 77 j=17,15,-1
c           write(lulog,777) (dedpsi(i,j),i=15,17)
c  77    continue
c
c        write(lulog,*) ' psi'
c        do 78 j=17,15,-1
c           write(lulog,777) (psi(i,j),i=15,17)
c  78    continue
c
c        Calculate cost function
         call cfcals(res,d2psi,cpta,nx,nx,cfunr,cfunp,cfunt)
c
         nudate = 0
 1100    continue
            cfunpr = cfunt
c
c           Update psi
            do 10 j=2,ny-1
            do 10 i=2,nx-1
               psi(i,j) = psi(i,j) - alpha*dedpsi(i,j)
   10       continue
c
            nudate = nudate + 1
c
c           Calculate x,y derviatives of psi
            call d2xxcal(psi,nx,ny,dx,dy,d2pdxx)
            call d2yycal(psi,nx,ny,dx,dy,d2pdyy)
            call d2xycal(psi,nx,ny,dx,dy,d2pdxy)
            call dycal(psi,nx,ny,dx,dy,dpdy)
c
c           Calculate Laplacian of psi
            call del2xy(psi,nx,ny,dx,dy,d2psi)
c
c           Calcualte residual
            call rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy,d2pdxy,dpdy,
     +                   inon,nx,ny,res)
c
c           Calculate cost function
            call cfcals(res,d2psi,cpta,nx,nx,cfunr,cfunp,cfunt)
c
c           write(6,701) cfunr,cfunp,cfunt
c 701       format(1x,'cfunr=',e11.4,1x,'cfunp=',e11.4,' cfunt=',e11.4)
c
            if (cfunt .lt. cfunpr .and. nudate .lt. ncg) then
c              Cost function is decreasing, so continue
c              down the gradient
               nupdate = nupdate + 1
               go to 1100
            else
c              Cost function is not decreasing, or maximum gradient
c              steps has been exceeded
               if (cfunt .ge. cfunpr) then
c                 Undo the last step down the gradient
                  do 20 j=2,ny-1
                  do 20 i=2,nx-1
                     psi(i,j) = psi(i,j) + alpha*dedpsi(i,j)
   20             continue
              endif
c
              if (mod(ngdate,10) .eq. 0) then
                 write(lulog,402) cfunr,cfunp,cfunt,
     +                            ngdate,nudate,alpha
  402            format(' CF:',e9.3,e9.3,e11.4,
     +                  ' ng=',i5,' nu=',i5,' al=',e10.3)
	      endif
c
c             Calculate optimal step and adjust it for next
c             gradient calculation
              rnu = float(nudate)
              if (rnu .lt. 1.0) rnu=1.0
              alpha = rnu*alpha*redfac
c
              if (ngdate .ge. nit) go to 1200
              if (alpha .le. 0.0) go to 1200
              ngdate = ngdate + 1
              go to 1000
           endif
 1200 continue
c
      call uvcal(psi,nx,ny,dx,dy,u,v)
c
      return
      end
      subroutine nbev(phi,u,v,inon,nxt,nyt)
c     This routine solves the balance equation for u and v
c     given phi and boundary values for u and v. A variational method
c     is used for solving the be. If inon=1, then the nonlinear version
c     is used, if inon=0, the linear version is used. If inon=-1, only the
c     cost function penalty term is included in the minimization procedure
c
c     This routine solves the balance equation in u,v form
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
      dimension d2phi(nx,ny),d2u(nx,ny),d2v(nx,ny)
      dimension dudx(nx,ny),dvdx(nx,ny),dudy(nx,ny),dvdy(nx,ny)
      dimension res(nx,ny),dedu(nx,ny),dedv(nx,ny)
      dimension cpta(nx,ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
      common /log/ lulog
c
c     Specify maximum number of iterations
      nit=3000
c
c     Specify maximum number of corrections before updating gradient
      ncg=20
c
c     Specify coefficient of the smoothness penalty term
      cpt =20.0  
c
c     Specify reduction factor for adjusting alpha before
c     new gradient calculation
      redfac = 0.1
c
c     Specify first guess for step length alpha
      alpha = 1.0e+17
c
c     Initialize penalty term coefficient array
      call zinter(cpta,cpt,nx,ny)
      call zbound(cpta,0.0,nx,ny)
c
c     Find Laplacian of phi
      call del2xy(phi,nx,ny,dx,dy,d2phi)
c
c     Calculate x,y derviatives of u,v
      call dxcal(u,nx,ny,dx,dy,dudx)
      call dxcal(v,nx,ny,dx,dy,dvdx)
      call dycal(u,nx,ny,dx,dy,dudy)
      call dycal(v,nx,ny,dx,dy,dvdy)
c
c     Calculate Laplacian of u,v
      call del2xy(u,nx,ny,dx,dy,d2u)
      call del2xy(v,nx,ny,dx,dy,d2v)
c
c     Calculate initial value of residual
      call rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nx,ny,res)
c
c     Calculate initial value of cost function
      call cfcal(res,d2u,d2v,cpta,nx,nx,cfunr,cfunp,cfunt)
c
      write(lulog,600) cfunr,cfunp,cfunt
  600 format(//,' Initial Cost function (r,p,t): ',3(e11.4,1x),/)
c
      ngdate = 1
 1000 continue
c        Calculate x,y derivaties of u,v
         call dxcal(u,nx,ny,dx,dy,dudx)
         call dxcal(v,nx,ny,dx,dy,dvdx)
         call dycal(u,nx,ny,dx,dy,dudy)
         call dycal(v,nx,ny,dx,dy,dvdy)
c
c        Calculate Laplacian of u,v
         call del2xy(u,nx,ny,dx,dy,d2u)
         call del2xy(v,nx,ny,dx,dy,d2v)
c
c        Calcualte residual
         call rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nx,ny,res)
c
c        Calculate cost function gradient
         call cfgcal(res,u,v,dudx,dvdx,dudy,dvdy,d2u,d2v,cpta,
     +               inon,nx,ny,dedu,dedv)
c
c        Calculate cost function
         call cfcal(res,d2u,d2v,cpta,nx,nx,cfunr,cfunp,cfunt)
c
         nudate = 0
 1100    continue
            cfunpr = cfunt
c
c           Update u,v
            do 10 j=2,ny-1
            do 10 i=2,nx-1
               u(i,j) = u(i,j) - alpha*dedu(i,j)
               v(i,j) = v(i,j) - alpha*dedv(i,j)
   10       continue
c
            nudate = nudate + 1
c           Calculate x,y derivaties of u,v
            call dxcal(u,nx,ny,dx,dy,dudx)
            call dxcal(v,nx,ny,dx,dy,dvdx)
            call dycal(u,nx,ny,dx,dy,dudy)
            call dycal(v,nx,ny,dx,dy,dvdy)
c
c           Calculate Laplacian of u,v
            call del2xy(u,nx,ny,dx,dy,d2u)
            call del2xy(v,nx,ny,dx,dy,d2v)
c
c           Calcualte residual
            call rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nx,ny,res)
c
c           Calculate cost function
            call cfcal(res,d2u,d2v,cpta,nx,nx,cfunr,cfunp,cfunt)
c
c            write(6,701) cfunpr,cfunt
c  701       format(1x,'cfunpr=',e11.4,1x,'cfunt=',e11.4)
c
            if (cfunt .lt. cfunpr .and. nudate .lt. ncg) then
c              Cost function is decreasing, so continue
c              down the gradient
               nupdate = nupdate + 1
               go to 1100
            else
c              Cost function is not decreasing, or maximum gradient
c              steps has been exceeded
               if (cfunt .ge. cfunpr) then
c                 Undo the last step down the gradient
                  do 20 j=2,ny-1
                  do 20 i=2,nx-1
                     u(i,j) = u(i,j) + alpha*dedu(i,j)
                     v(i,j) = v(i,j) + alpha*dedv(i,j)
   20             continue
              endif
c
              if (mod(ngdate,50) .eq. 0) then
                 write(lulog,402) cfunr,cfunp,cfunt,
     +                            ngdate,nudate,alpha
  402            format(' CF:',e9.3,e9.3,e11.4,
     +                  ' ng=',i5,' nu=',i5,' al=',e10.3)
	      endif
c
c             Calculate optimal step and adjust it for next
c             gradient calculation
              rnu = float(nudate)
              if (rnu .lt. 1.0) rnu=1.0
              alpha = rnu*alpha*redfac
c
              if (ngdate .ge. nit) go to 1200
              if (alpha .le. 0.0) go to 1200
              ngdate = ngdate + 1
              go to 1000
           endif
 1200 continue
c
      return
      end
      subroutine cfgcals(res,d2pdxx,d2pdyy,d2pdxy,d2psi,cpta,
     +                  inon,nxt,nyt,dedpsi)
c     This routine calculates the gradient of the cost function
c     with respect to u and v (dedu and dedv) required for the variational
c     solution to the balance equation. 
c
      parameter (nx=61,ny=61)
c
c     Passed arrays
      dimension res(nxt,nyt)
      dimension d2pdxx(nxt,nyt),d2pdyy(nxt,nyt),d2pdxy(nxt,nyt)
      dimension d2psi(nxt,nyt),cpta(nxt,nyt)
      dimension dedpsi(nxt,nyt)
c
c     Common arrays
      dimension x(nx),y(ny),fy(ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
c
c     Set cost function gradient to zero at the domain boundaries
      call zbound(dedpsi,0.0,nxt,nyt)
c
      dxi = 1.0/dx
      dyi = 1.0/dy
      dxi2  = dxi*dxi
      dyi2  = dyi*dyi
      dxyi  = dxi*dyi
c
c     Interior points
c        Linear terms:
      do 10 j=2,ny-1
      do 10 i=2,nx-1
         dedpsi(i,j) = 
     +   dxi2*(res(i-1,j)*fy(j  )+res(i+1,j)*fy(j  )-2.*res(i,j)*fy(j))
     +  +dyi2*(res(i,j-1)*fy(j-1)+res(i,j+1)*fy(j+1)-2.*res(i,j)*fy(j))
     +  +beta*0.5*dyi*(res(i,j-1)-res(i,j+1))
   10 continue
c
c       Penalty terms:
      do 20 j=2,ny-1
      do 20 i=2,nx-1
	 dedpsi(i,j) = dedpsi(i,j) + dxi2*cpta(i+1,j)*d2psi(i+1,j)
     +                             + dxi2*cpta(i-1,j)*d2psi(i-1,j)
     +                          -2.0*dxi2*cpta(i  ,j)*d2psi(i  ,j)
     +                           +   dyi2*cpta(i,j+1)*d2psi(i,j+1)
     +                           +   dyi2*cpta(i,j-1)*d2psi(i,j-1)
     +                          -2.0*dyi2*cpta(i,j  )*d2psi(i,j  )
   20 continue
c
      if (inon .ne. 1) return
c
c       Nonlinear terms
      do 30 j=2,ny-1
      do 30 i=2,nx-1
	 dedpsi(i,j) = dedpsi(i,j) + 
     +                 dxyi*( d2pdxy(i-1,j-1)*res(i-1,j-1) +
     +                        d2pdxy(i+1,j+1)*res(i+1,j+1) -
     +                        d2pdxy(i-1,j+1)*res(i-1,j+1) -
     +                        d2pdxy(i+1,j-1)*res(i+1,j-1) )
     +            -2.0*dyi2*( d2pdxx(i,j-1)*res(i,j-1) +
     +                        d2pdxx(i,j+1)*res(i,j+1) -
     +                    2.0*d2pdxx(i,j  )*res(i,j  )     )
     +            -2.0*dxi2*( d2pdyy(i-1,j)*res(i-1,j) +
     +                        d2pdyy(i+1,j)*res(i+1,j) -
     +                    2.0*d2pdyy(i  ,j)*res(i  ,j)     )
   30 continue
c
      return
      end
      subroutine cfgcal(res,u,v,dudx,dvdx,dudy,dvdy,d2u,d2v,cpta,
     +                  inon,nxt,nyt,dedu,dedv)
c     This routine calculates the gradient of the cost function
c     with respect to u and v (dedu and dedv) required for the variational
c     solution to the balance equation. 
c
      parameter (nx=61,ny=61)
c
c     Passed arrays
      dimension res(nxt,nyt),u(nxt,nyt),v(nxt,nyt)
      dimension dudx(nxt,nyt),dvdx(nxt,nyt),dudy(nxt,nyt),dvdy(nxt,nyt)
      dimension d2u(nxt,nyt),d2v(nxt,nyt),cpta(nxt,nyt)
      dimension dedu(nxt,nyt),dedv(nxt,nyt)
c
c     Common arrays
      dimension x(nx),y(ny),fy(ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
c
c     Set cost function gradient to zero at the domain boundaries
      call zbound(dedu,0.0,nxt,nyt)
      call zbound(dedv,0.0,nxt,nyt)
c
      dxi = 1.0/dx
      dyi = 1.0/dy
      tdxi = 0.5*dxi
      tdyi = 0.5*dyi
      dxi2 = dxi*dxi
      dyi2 = dyi*dyi
c
c     Interior points
c
c       Penalty terms:
      do 10 j=2,ny-1
      do 10 i=2,nx-1
	 dedu(i,j) =      dxi2*cpta(i+1,j)*d2u(i+1,j)
     +                  + dxi2*cpta(i-1,j)*d2u(i-1,j)
     +               -2.0*dxi2*cpta(i  ,j)*d2u(i  ,j)
     +                  + dyi2*cpta(i,j+1)*d2u(i,j+1)
     +                  + dyi2*cpta(i,j-1)*d2u(i,j-1)
     +               -2.0*dyi2*cpta(i,j  )*d2u(i,j  )
c
	 dedv(i,j) =      dxi2*cpta(i+1,j)*d2v(i+1,j)
     +                  + dxi2*cpta(i-1,j)*d2v(i-1,j)
     +               -2.0*dxi2*cpta(i  ,j)*d2v(i  ,j)
     +                  + dyi2*cpta(i,j+1)*d2v(i,j+1)
     +                  + dyi2*cpta(i,j-1)*d2v(i,j-1)
     +               -2.0*dyi2*cpta(i,j  )*d2v(i,j  )
   10 continue
c
      if (inon .eq. -1) return
c
c        Linear terms:
      do 20 j=2,ny-1
      do 20 i=2,nx-1
         dedu(i,j) = dedu(i,j) + tdyi*(res(i,j-1)*fy(j-1)-
     +                                res(i,j+1)*fy(j+1))  
     +                         + beta*res(i,j)
c
         dedv(i,j) = dedv(i,j) - tdxi*(res(i-1,j)*fy(j)-
     +                                 res(i+1,j)*fy(j))
   20 continue
c
      if (inon .ne. 1) return
c
c       Nonlinear terms
      do 30 j=2,ny-1
      do 30 i=2,nx-1
	 dedu(i,j) = dedu(i,j) + dxi*(res(i-1,j)*dudx(i-1,j)-
     +                                res(i+1,j)*dudx(i+1,j))
     +                         + dyi*(res(i,j-1)*dvdx(i,j-1)-
     +                                res(i,j+1)*dvdx(i,j+1))
c
	 dedv(i,j) = dedv(i,j) + dxi*(res(i-1,j)*dudy(i-1,j)-
     +                                res(i+1,j)*dudy(i+1,j))
     +                         + dyi*(res(i,j-1)*dvdy(i,j-1)-
     +                                res(i,j+1)*dvdy(i,j+1))
   30 continue
c
      return
      end
      subroutine cfcals(res,d2psi,cpta,nx,ny,cfunr,cfunp,cfunt)
c     This routine calculates the cost function. The contribution
c     from the residual of the nonlinear balance equation and from
c     the smoothness penalty term are also calculated.
c
c     This version is for the balance equation in streamfunction form
c
      dimension res(nx,ny)
      dimension d2psi(nx,ny),cpta(nx,ny)
c
c     Calculate the contribution from the residual
      cfunr = 0.0
      do 10 j=2,ny-1
      do 10 i=2,nx-1
         cfunr = cfunr + res(i,j)*res(i,j)
   10 continue
      cfunr = 0.5*cfunr
c
c     Calculate the contribution from the penalty term
      cfunp = 0.0
      do 20 j=2,ny-1
      do 20 i=2,nx-1
	 cfunp = cfunp + cpta(i,j)*(d2psi(i,j)**2)
   20 continue
      cfunp = 0.5*cfunp
c
      cfunt = cfunr+cfunp
c
      return
      end
      subroutine cfcal(res,d2u,d2v,cpta,nx,ny,cfunr,cfunp,cfunt)
c     This routine calculates the cost function. The contribution
c     from the residual of the nonlinear balance equation and from
c     the smoothness penalty term are also calculated.
c
c     This version is for the balance equation in u,v form
c
      dimension res(nx,ny)
      dimension d2u(nx,ny),d2v(nx,ny),cpta(nx,ny)
c
c     Calculate the contribution from the residual
      cfunr = 0.0
      do 10 j=2,ny-1
      do 10 i=2,nx-1
         cfunr = cfunr + res(i,j)*res(i,j)
   10 continue
      cfunr = 0.5*cfunr
c
c     Calculate the contribution from the penalty term
      cfunp = 0.0
      do 20 j=2,ny-1
      do 20 i=2,nx-1
	 cfunp = cfunp + cpta(i,j)*(d2u(i,j)**2 + d2v(i,j)**2)
   20 continue
      cfunp = 0.5*cfunp
c
      cfunt = cfunr+cfunp
c
      return
      end
      subroutine rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy,d2pdxy,dpdy,
     +                   inon,nxt,nyt,res)
c     The routine calculates the residual of the balance equation
c     in Cartesian coordinates on a beta plane. This version 
c     is for the balance equation in terms of streamfunction.
c
      parameter(nx=61,ny=61)
c
      dimension psi(nxt,nyt)
      dimension d2phi(nxt,nyt),d2psi(nxt,nyt)
      dimension d2pdxx(nxt,nyt),d2pdyy(nxt,nyt),d2pdxy(nxt,nyt)
      dimension dpdy(nxt,nyt)
      dimension res(nxt,nyt)
c
      dimension x(nx),y(ny),fy(ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
c
c     Set residual to zero at boundary points because they
c     are not included in the cost function
      call zbound(res,0.0,nxt,nyt)
c
c     Calculate residual at interior points
      do 10 j=2,ny-1
      do 10 i=2,nx-1
         res(i,j) =  fy(j)*d2psi(i,j) +
     +               beta*dpdy(i,j) - d2phi(i,j)
   10 continue
c
      if (inon .ne. 1) return
c
c     Add nonlinear terms
      do 20 j=2,ny-1
      do 20 i=2,nx-1
	 res(i,j) = res(i,j) + 2.0*d2pdxy(i,j)*d2pdxy(i,j) 
     +                       - 2.0*d2pdxx(i,j)*d2pdyy(i,j)
   20 continue
c
      return
      end
      subroutine rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nxt,nyt,res)
c     The routine calculates the residual of the balance equation
c     in Cartesian coordinates on a beta plane. This version is
c     for the balance equation in terms of u,v.
c
      parameter(nx=61,ny=61)
c
      dimension u(nxt,nyt),v(nxt,nyt)
      dimension d2phi(nxt,nyt)
      dimension dudx(nxt,nyt),dvdx(nxt,nyt),dudy(nxt,nyt),dvdy(nxt,nyt)
      dimension res(nxt,nyt)
c
      dimension x(nx),y(ny),fy(ny)
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
c
c     Set residual to zero at boundary points because they
c     are not included in the cost function
      call zbound(res,0.0,nxt,nyt)
c
      if (inon .eq. -1) then
c        Minimization only includes penalty term, so set res=0 
	 call zinter(res,0.0,nxt,nyt)
	 return
      endif
c
c     Calculate residual at interior points
      do 10 j=2,ny-1
      do 10 i=2,nx-1
         res(i,j) = -fy(j)*(dvdx(i,j)-dudy(i,j)) +
     +               beta*u(i,j) + d2phi(i,j)
   10 continue
c
      if (inon .ne. 1) return
c
c     Add nonlinear terms
      do 20 j=2,ny-1
      do 20 i=2,nx-1
         res(i,j) = res(i,j) + dudx(i,j)*dudx(i,j) + 
     +                         dvdy(i,j)*dvdy(i,j) +
     +                     2.0*dvdx(i,j)*dudy(i,j)
   20 continue
c
      return
      end
      subroutine nbei(phi,u,v,nxt,nyt)
c     This routine solves the nonlinear balance equation for u and v
c     given phi and boundary values for u and v. An iterative
c     procedure is used to solve the balance equation.
c
      parameter (nx=61,ny=61)
c
c     Passed arrays
      dimension phi(nxt,nyt),u(nxt,nyt),v(nxt,nyt)
c
c     Common arrays
      dimension x(nx),y(ny),fy(ny),gy(ny)
c
c     Local arrays
      dimension psi(nx,ny),psifg(nx,ny),d2phi(nx,ny)
      dimension zeta(nx,ny),zetat(nx,ny),phib(nx,ny)
      dimension dudx(nx,ny),dvdx(nx,ny),dudy(nx,ny),dvdy(nx,ny)
      dimension cterm(nx,ny),disc(nx,ny),div(nx,ny)
c
      character *30 label
c
      common /cgrid/ x,y,fy,dx,dy,f0,beta
      common /log/ lulog
c
c     Specify maximum number of iterations
      nit=10
c
c     Specify under-relaxation coefficient
c       (  zeta(new) = (1-urc)*zeta(update) + urc*zeta(old)  )
      urc  = 0.2
      urcc = 1.0-urc
c
c     Find Laplacian of phi
      call del2xy(phi,nx,ny,dx,dy,d2phi)
c
c     Set gy=0 for poisson routine
      do 10 j=1,ny
         gy(j) = 0.0
   10 continue
c
c     Make first guess for psi
      do 15 j=1,ny
      do 15 i=1,nx
         psifg(i,j) = 0.0
   15 continue
c
c     Calculate psi boundary conditions
      call psibd(psifg,nx,ny,dx,dy,u,v)
c
c     Calculate vorticity
      call dxcal(v,nx,ny,dx,dy,dvdx)
      call dycal(u,nx,ny,dx,dy,dudy)
      do 20 j=2,ny-1
      do 20 i=2,nx-1
	 zeta(i,j) = dvdx(i,j)-dudy(i,j)
   20 continue
c
c     Calculate stream function
      call psonxy(zeta,gy,psi,psifg,ierr)
c
c     Calculate u,v from psi
      call uvcal(psi,nx,ny,dx,dy,u,v)
c
c     Calculate x,y derivatives of u,v
      call dxcal(u,nx,ny,dx,dy,dudx)
      call dxcal(v,nx,ny,dx,dy,dvdx)
      call dycal(u,nx,ny,dx,dy,dudy)
      call dycal(v,nx,ny,dx,dy,dvdy)
c
c     Start iteration
      do 99 k=1,nit
c
c        Calculate c term of quadratic form of vorticity equation
	 do 25 j=2,ny-1
	 do 25 i=2,nx-1
	    cterm(i,j) = ( dvdx(i,j)+dudy(i,j))**2 +
     +                   (-dudx(i,j)+dvdy(i,j))**2 +
     +                   2.0*(d2phi(i,j) + beta*u(i,j))
   25    continue
c
c        Calculate the discriminant of the quadratic (in zeta) form of the BE
         do 30 j=2,ny-1
	 do 30 i=2,nx-1
	    disc(i,j) = fy(j)*fy(j) + cterm(i,j)
   30    continue
c
c        Calculate updated vorticity 
	 do 35 j=2,ny-1
	 do 35 i=2,nx-1
	    if (disc(i,j) .gt. 0.0) then
	       div(i,j) = 0.0
	       zetat(i,j) = -fy(j) + sqrt(disc(i,j))
            else
	       div(i,j) = abs(disc(i,j))
	       zetat(i,j) = -fy(j)
            endif
   35    continue
c
c        Apply under-relaxation
	 do 36 j=2,ny-1
	 do 36 i=2,nx-1
	    zeta(i,j) = urcc*zetat(i,j) + urc*zeta(i,j)
   36    continue
c
c        Calculate stream function
         call psonxy(zeta,gy,psi,psifg,ierr)
c
c        Calculate u,v from psi
         call uvcal(psi,nx,ny,dx,dy,u,v)
c
c        Calculate x,y derivatives of u,v
         call dxcal(u,nx,ny,dx,dy,dudx)
         call dxcal(v,nx,ny,dx,dy,dvdx)
         call dycal(u,nx,ny,dx,dy,dudy)
         call dycal(v,nx,ny,dx,dy,dvdy)
c
c        Calculate the forcing in the "reverse" balance equation 
c        to test for convergence
	 do 40 j=2,ny-1
	 do 40 i=2,nx-1
            cterm(i,j) = fy(j)*(dvdx(i,j)-dudy(i,j)) - beta*u(i,j) 
     +                   -dudx(i,j)**2 - dvdy(i,j)**2 
     +                   -2.0*dvdx(i,j)*dudy(i,j)
   40    continue
c
c        Solve reverse balance equation for phi
         call psonxy(cterm,gy,phib,phi,ierr)
c
c        Calculate average and max difference between phi and phib (scaled by g)
         dzmax = 0.0
	 dzavg = 0.0
	 do 45 j=2,ny-1
	 do 45 i=2,nx-1
	    dz = abs(phib(i,j)-phi(i,j))
	    dzavg = dzavg + dz
	    if (dz .gt. dzmax) dzmax = dz
   45    continue
c  
	 dzavg = dzavg/(9.81*float(nx-2)*float(ny-2))
	 dzmax = dzmax/9.81
c
         itest=1
         if (itest .eq. 1) then
            write(lulog,200) k,dzavg,dzmax
  200       format(' Iteration ',i4,' dzavg=',e11.4,'  dzmax=',e11.4)
	 endif
c
   99 continue
c
      itest=0
      if (itest .eq. 1) then
         do 60 j=1,ny
         do 60 i=1,nx
            phib(i,j) = (phib(i,j)-phi(i,j))/9.81
c           phib(i,j) = (phib(i,j))/9.81 - 2000.0
   60    continue
c
         label=' z error from nbei'
         call fprint(phib,nx,ny,lulog,label)
      endif
c
      return
      end
      subroutine uvcal(psi,nx,ny,dx,dy,u,v)
c     This routine calculates u,v from the streamfunction psi
c
      dimension psi(nx,ny),u(nx,ny),v(nx,ny)
c
      call dxcal(psi,nx,ny,dx,dy,v)
      call dycal(psi,nx,ny,dx,dy,u)
c
      do 10 j=1,ny
      do 10 i=1,nx
         u(i,j) = -1.0*u(i,j)
   10 continue
c
      return
      end
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
      subroutine d2xxcal(f,nx,ny,dx,dy,d2fdx2)
c     This routine calculates d2f/dx2 in Cartesian coordinates
c
      dimension f(nx,ny),d2fdx2(nx,ny)
c
      dx2i = 1.0/(dx*dx)
c
c     Interior points
      do 10 j=1,ny
      do 10 i=2,nx-1
         d2fdx2(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  )) 
   10 continue
c
      do 20 j=1,ny
c        Left edge
         i = 1
         d2fdx2(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  )) 
c
c        Right edge
         i = nx
         d2fdx2(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  )) 
   20 continue
c
      return
      end
      subroutine d2yycal(f,nx,ny,dx,dy,d2fdy2)
c     This routine calculates d2f/dy2 in Cartesian coordinates
c
      dimension f(nx,ny),d2fdy2(nx,ny)
c
      dy2i = 1.0/(dy*dy)
c
c     Interior points
      do 10 j=2,ny-1
      do 10 i=1,nx
         d2fdy2(i,j) = dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
   10 continue
c
      do 30 i=1,nx
c        Bottom edge
         j = 1
         d2fdy2(i,j) = dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))
c
c        Top edge
         j = ny
         d2fdy2(i,j) = dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))
   30 continue
c
      return
      end
      subroutine d2xycal(f,nx,ny,dx,dy,d2fdxy)
c     This routine calculates d2f/dxdy in Cartesian coordinates,
c     using centered differences. The mixed derivative is only
c     calculated at the domain interior points, and is set to zero 
c     on the domain boundaries.
c
      dimension f(nx,ny),d2fdxy(nx,ny)
c
      dxdyi = 0.25/(dx*dy)
c
c     Interior points
      do 10 j=1,ny
      do 10 i=2,nx-1
         d2fdxy(i,j) = dxdyi*( f(i+1,j+1)+f(i-1,j-1)
     +                        -f(i+1,j-1)-f(i-1,j+1) ) 
   10 continue
c
c     Set boundary values to zero
      call zbound(d2fdxy,0.0,nx,ny)
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
c      write(6,410) omega,oomega
c  410 format(' omega, oomega: ',e11.4,1x,e11.4)
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
c      write(6,400) enorm
c  400 format(' enorm=',e11.4)
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
      subroutine zinter(f,c,nx,ny)
c     This routine sets the interior points in the array f to a constant
c
      dimension f(nx,ny)
c
      do 10 j=2,ny-1
      do 10 i=2,nx-1
	 f(i,j) = c
   10 continue
c
      return
      end
      subroutine zbound(f,c,nx,ny)
c     This routine sets the boundary points in the array f to a constant
c
      dimension f(nx,ny)
c
      do 10 j=1,ny
	 f( 1,j) = c
	 f(nx,j) = c
   10 continue
c
      do 20 i=2,nx-1
	 f(i, 1) = c
	 f(i,ny) = c
   20 continue
c
      return
      end
      subroutine fprint(f,nx,ny,lulog,label)
c     This routine prints f(x,y)
c     
      dimension f(nx,ny)
      character *30 label
c
      write(lulog,200) label
  200 format(/,1x,a30)
c
      do 10 j=ny,1,-1
         write(lulog,220) j,( ifix(f(i,j)+0.5*sign(1.0,f(i,j)) ),i=1,nx)
c 220    format(1x,i2,1x,31(f4.0))
  220    format(1x,i2,1x,31(i4))
   10 continue
      write(lulog,230) (i,i=1,nx)
  230 format(3x,31(i4))
c
      return 
      end
