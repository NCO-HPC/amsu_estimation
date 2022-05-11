      subroutine amsures(dist,res)
c     This routine estimates the resolution of the AMSU data 
c     as a function of the distance from the satellite sub point
c 
c     Input:  dist = distance in km from satellite sub-point
c     Output: res  = Approximate distance (km) between scan points
c
      a = 5.47e-4
      b = 8.57e-1
      c = 48.0
c
c     Check for invalid range of dist
      if (dist .lt. 0.0 .or. dist .gt. 1500.0) then
	 res = -99.9
	 return
      endif
c
      x = dist/100.0
      res = a*(x**4) + b*(x**2) + c
c
      return
      end
