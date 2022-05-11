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
