c     This is a group of subroutines for evaluating the hydrostatic
c     equation. The physical constants in common block /cons/ must
c     be provided. Included routines:
c
c        ** tint
c        ** ztint
c        ** tkness
c        ** p2cal
c
      subroutine tint(z1,z2,zt,t1,t2,tt)
c     This routine linearly interpolates the temperature to the level
c     zt to give tt. The level zt must be between z1 and z2, with
c     temperatures t1 and t2. 
c
c     Check for zero thickness
      if (z1 .eq. z2) then
         tt = 0.0
         return
      endif
c
c     Check for isothermal case
      if (t1 .eq. t2) then
         tt = t1
         return
      endif
c
      slope = (t2-t1)/(z2-z1)
      yint  = (t1*z2 - t2*z1)/(z2-z1)
      tt = slope*zt + yint
c
      return
      end
      subroutine ztint(p1,p2,z1,z2,t1,t2,pt,zt,tt)
c     This routine calculates the height zt and temperature tt at a specified 
c     pressure level pt which lies between p1 and p2. The heights and temperatures
c     at p1,p2 are z1,z2 and t1,t2, respecitively. A constant lapse rate atmosphere is assumed
c     between p1 and p2, and it is also assumed that p1>p2 and z1<z2. 
c
      common /cons/ pi,g,rd,dtr,erad,erot
c
c     Check for zero pressure
      if (p1 .le. 0.0 .or. p2 .le. 0.0) then
         pt = 0.0
         zt = 0.0
         tt = 0.0
         return
      endif
c
c     Check for zero absolute temperatures
      if (t1 .le. 0.0 .or. t2 .le. 0.0) then
         pt = 0.0
         zt = 0.0
         tt = 0.0
         return
      endif
c
c     Check for zero thickness
      if (p1 .eq. p2 .or. z1 .eq. z2) then
         pt = 0.0
         zt = 0.0
         tt = 0.0
         return
      endif
c
c     Check for nearly isothermal atmosphere
      dt = abs(t1-t2)
      if (dt .lt. 0.1) then
         tbar = 0.5*(t1+t2)
	 tt  = tbar
         zt  = z1 + (rd/g)*tbar*alog(p1/pt)
         return
      endif
c     
c     General case
      gamma = (t2-t1)/(z2-z1)
      a     = g/(rd*gamma)
      ai    = 1.0/a
      zt    = z1 + (t1/gamma)*((p1/pt)**ai - 1.0)
      tt    = t1 + gamma*(zt-z1)
c
      return
      end
      subroutine tkness(p1,p2,t1,t2,dz)
c     This routine calculates the thickness dz (m) between pressure levels
c     p1,p2 (Pa) given the temperatures t1,t2 (K). A constant lapse rate as a
c     function of z is assumed between the levels. dz is always positive,
c     unless non-physical input values were provided.
c
      common /cons/ pi,g,rd,dtr,erad,erot
c
c     Check for zero pressure
      if (p1 .le. 0.0 .or. p2 .le. 0.0) then
         dz = 0.0
         return
      endif
c
c     Check for zero absolute temperatures
      if (t1 .le. 0.0 .or. t2 .le. 0.0) then
         dz = 0.0
         return
      endif
c
c     Check for zero thickness
      if (p1 .eq. p2) then
         dz = 0.0
         return
      endif
c
c     Check for nearly isothermal atmosphere
      dt = abs(t1-t2)
      if (dt .lt. 0.1) then
         tbar = 0.5*(t1+t2)
         dzt = (rd/g)*tbar*alog(p1/p2)
         dz  = abs(dzt)
         return
      endif
c     
c     General case
      dzt = (rd/g)*(t1-t2)*alog(p1/p2)/(alog(t1/t2))
      dz  = abs(dzt)
c
      return
      end
      subroutine p2cal(z1,z2,t1,t2,p1,p2)
c     This routine calculates the pressure p2 (Pa) at height z2 (m)
c     given the temperatures t1,t2 (K), the height z1 (m) and pressure
c     p1 (Pa). A constant lapse rate as a function of z is assumed
c     between the levels.
c
      common /cons/ pi,g,rd,dtr,erad,erot

c     Check for negative heights
      if (z1 .lt. 0.0 .or. z2 .lt. 0.0) then
         p2 = 0.0
         return
      endif
c
c     Check for zero absolute temperatures
      if (t1 .le. 0.0 .or. t2 .le. 0.0) then
         p2 = 0.0
         return
      endif
c
c     Check for zero thickness
      if (z1 .eq. z2) then
         p2 = p1
         return
      endif
c
c     Check for nearly isothermal atmosphere
      dt = abs(t1-t2)
      if (dt .lt. 0.1) then
         tbar = 0.5*(t1+t2)
         p2 = p1*exp( -g*(z2-z1)/(rd*tbar) )
         return
      endif
c     
c     General case
      gm = -(t2-t1)/(z2-z1)
      a= g/(rd*gm)
      p2 = p1*( (t2/t1)**a )
c
      return
      end
