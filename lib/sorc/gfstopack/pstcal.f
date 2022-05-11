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
