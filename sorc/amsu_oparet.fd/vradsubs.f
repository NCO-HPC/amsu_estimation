      subroutine vradp(preds,slat,slon,spd,head,vmaxop,ias,
     +                 pvmx,ppmin,rm,x,pr34,pr50,pr64)
c***********************************************************
c     This subroutine was modified April 2005, by CIRA
c     with the new predictors and coefficients, 
c     developed using global data from 1999-2004,
c     for estimating the MSW (pvmx), MSLP (ppmin), and wind radii.  
c     Changes are denoted by *** prior to and after changes.
c***********************************************************
c     This routine estimates the storm maximum wind
c     and the radii of 34,50 and 64 kt winds from AMSU
c     analysis parameters.
c
c     Input:
c       preds - Array of length 18 containing predictors from the 
c               AMSU analysis (see below)
c       slat - Storm latitude (deg N)
c       slon - Storm longitude (deg W positive)
c       spd  - storm translational speed
c       head - storm heading (deg clockwise from north)
c       vmaxop - Operational estimate of max winds (kt)
c       ias  - Set ias=1 to skip asymmetric wind radii estimation
c              Set ias=0 to include asymmetric winds 
c
c     Output 
c       pvmx - predicted maximum surface winds (kt)
c       ppmin- predicted minimum sea-level pressure (hPa) 
c       pr34 - radius (nm) of 34 kt winds
c       pr50 - radius (nm) of 50 kt winds
c       pr64 - radius (nm) of 64 kt winds
c       rm   - radius of max winds (nm) from vortex model fit
c       x    - Decay exponent from vortex model fit
c
c       Notes: 
c          The 18 input AMSU predictors are as follows: 
c          1. analyzed pressure (hPa) at r,z=0
c          2. r=600 to r=0 pressure drop (hPa) at z=0 km  
c          3. r=600 to r=0 pressure drop (hPa) at z=3 km
c          4. r=0 Max temperature anomaly (C)
c          5. height (km) of r=0 max temp. anomaly
c          6. swath spacing (km)
c          7. max wind (kt) at z=0 
c          8. radius (km) of z=0 max wind
c          9. max wind (kt) at z=3 km
c          10. radius (km) of z=3 max wind
c          11. 0-250 km avg. wind (kt) at z=0 km
c          12. 0-250 km avg. wind (kt) at z=3 km
c          13. 0-250 km avg. wind (kt) at z=5 km
c          14. 250-500 km avg. wind (kt) at z=0 km
c          15. 250-500 km avg. wind (kt) at z=3 km
c          16. 250-500 km avg. wind (kt) at z=5 km
c          17. r=0 to r=100 km avg. CLW
c          18. Percent CLW r=0 to r=300 exceeding 0.5
c
c          vmaxop and lat are included as possible predictors
c          for wind radii. These are predictors 19. and 20. 
c
c          (vmaxop should not be used to predict pvmx or ppmin)
c***
c          Four additional predictors are added to the pool. 
c          Predictor 21 = tmax^2 (i.e., predictor 4 from above squared).
c          Predictor 22 = tmax*clwave (i.e., predictors 4*17).
c          Predictor 23 = clwave^2 (i.e., predictor 17 squared).
c          Predictor 24 = the pressure at r=600km (i.e., predictors 1+2
c          from above), which is not AMSU-derived, rather it is acquired
c          from the NCEP GFS model and is used to derive pmin and dp0.
c***
c
c          pr34,50,64 are 1-D arrays of dimension 6, where
c          pr(1) - NE radius 
c          pr(2) - SE radius
c          pr(3) - SW radius
c          pr(4) - SW radius
c          pr(5) - azimuthal mean radius
c          pr(6) - azimuthal mean radius from vortex fit
c
      dimension pr34(6),pr50(6),pr64(6), dummy(4)
c
      parameter  (nco=18)
c***   nct is modified to accomodate the 4 additional predictors, 21-24
c      parameter  (nct=nco+2)
      parameter  (nct=nco+6)
c***      
c
      dimension preds(nco),predt(nct)
      dimension cvmx(0:nct),cpmn(0:nct)
      dimension cr34(0:nct),cr50(0:nct),cr64(0:nct)
c
c***  Modified predictor layout to include predictors 21-24 
c     (i.e., tmax^2, tmax*clwave, clwave^2, and p600, respectively) 
c     Coefficients also were modified to be based on 1999-2004 data.
c                 0        1        2        3        4        5   
c                          6        7        8        9       10
c                         11       12       13       14       15  
c                         16       17       18       19       20
c                         21       22       23       24
c
c     Coefficients based upon 1999-2004 global data
      data cvmx /13.12557, 0.00000,-4.33459, 6.48789, 6.28701, 0.00000, 
     +                     0.13380, 0.00000, 0.00000, 0.49635,-0.02713,
     +                     0.00000, 0.00000, 1.72608, 1.85672,-2.48450,
     +                     0.00000,19.84888,-0.26614, 0.00000, 0.00000,
     +                    -0.51428, 0.00000, 0.00000, 0.00000/
c
      data cpmn /15.17819, 0.00000,-0.04260, 0.06316, 0.07395, 0.00000, 
     +                     0.00153, 0.00000, 0.00000, 0.00537,-0.00029,
     +                     0.00000, 0.00000, 0.01681, 0.01753,-0.02156,
     +                     0.00000, 0.20820,-0.00209, 0.00000, 0.00000,
     +                    -0.00655, 0.00000, 0.00000, -0.01145/
c
      data cr34 /1734.869,-1.67423, 3.46238, 0.00000,10.41016, 0.00000,
     +                     0.38938,-0.67877, 0.09078, 0.00000, 0.00000,
     +                     0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
     +                     0.00000,-39.6419, 0.30049, 0.00000, 0.00000,
     +                     0.00000,-8.81319,30.62560, 0.00000/
c
      data cr50 /-5.12785, 0.00000, 2.56301, 0.00000, 6.85088, 0.00000,
     +                     0.00000, 0.89570, 0.06395, 0.00000, 0.00000,
     +                    -5.54525,13.59224,-13.0449, 0.00000, 0.00000,
     +                     0.00000,-18.6103, 0.27442, 0.49101, 0.00000,
     +                     0.00000, 0.00000, 0.00000, 0.00000/
c
      data cr64 /-18.4571, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,
     +                     0.37589, 2.10560, 0.00000,-2.52543, 0.00000,
     +                    -4.11899, 6.62236, 1.75455, 0.00000, 0.00000,
     +                    -1.94906, 0.00000, 0.15221, 0.00000, 0.00000,
     +                     0.45719, 0.00000, 0.00000, 0.00000/
c***
c     Coefficients based upon 1999-2001 data
c      data cvmx /14.036, 0.000,-2.171, 0.000, 2.917, 0.000, 
c     +                   0.190, 0.000, 0.000, 0.000,-0.028,
c     +                   0.000, 0.000, 4.250, 0.581, 0.000,
c     +                   0.000,20.050,-0.206, 0.000, 0.000/
c
c      data cpmn /450.28, 0.564, 1.808, 0.000,-2.737, 0.000,
c     +                  -0.145, 0.000, 0.000, 0.000, 0.020,
c     +                   0.000, 0.000,-2.588,-0.371, 0.000,
c     +                   0.000,-12.77, 0.103, 0.000, 0.000/
c
c      data cr34 /2406.1,-2.409, 4.482,-10.39, 0.000, 0.000,
c     +                   0.000, 0.000, 0.000, 0.000, 0.000,
c     +                   0.000, 0.000, 0.000, 0.000, 0.000,
c     +                   2.493, 0.000, 0.590, 1.676, 0.891/
c
c      data cr50 /-63.59, 0.000, 0.000, 0.000,22.037, 0.000,
c     +                   0.000, 0.000, 0.000, 0.000, 0.122,
c     +                   0.000, 8.043,-13.62, 0.000, 0.000,
c     +                   0.000, 0.000, 0.000, 0.000, 0.990/
c
c      data cr64 /-48.57, 0.000, 0.000, 0.000, 6.286, 0.000,
c     +                   0.427, 0.000, 0.000, 0.000, 0.000,
c     +                   0.000, 0.000, 0.000, 0.000, 0.000,
c     +                   0.000, 0.000, 0.270, 0.000, 0.217/
c
c     Coefficients based upon 1999-2000 data
c     data cvmx /26.781, 0.000, 0.000,-2.315, 0.000, 0.000, 
c    +                   0.000, 0.000, 0.000, 0.000,-0.038,
c    +                   0.000,-5.158,10.261, 1.169, 0.000,
c    +                  -1.477,11.319, 0.000, 0.000, 0.000/
c
c     data cpmn /1005.5, 0.000, 0.000, 0.000, 0.000, 0.000,
c    +                   0.000, 0.000, 0.000, 0.000, 0.024,
c    +                   0.000, 3.453,-6.190,-0.909, 0.000,
c    +                   1.236,-8.436, 0.000, 0.298, 0.000/
c
c     data cr34 /-85.57, 0.000, 0.000,-0.658, 0.000, 0.000,
c    +                   0.000, 0.000, 0.000, 0.000, 0.000,
c    +                   0.000, 1.597, 0.000, 0.000, 0.000,
c    +                   0.000,32.184, 0.000, 3.616, 0.873/
c
c     data cr50 /-72.25, 0.000, 0.000, 3.269, 0.000, 0.000,
c    +                   0.000, 0.000, 0.000, 0.000, 0.000,
c    +                   0.000,-0.890, 0.000, 0.000, 0.000,
c    +                   0.000,19.278, 0.000, 1.609, 0.842/
c
c     data cr64 /-22.97, 0.000, 0.000, 4.087, 0.000, 0.000,
c    +                   0.000, 0.000, 0.000, 0.000, 0.000,
c    +                   0.000,-1.647, 0.000, 0.000, 0.000,
c    +                   0.000, 8.009, 0.000, 0.231, 0.418/
c
c     Initialize wind radii to zero
      do 10 i=1,6
	 pr34(i) = 0.0
	 pr50(i) = 0.0
	 pr64(i) = 0.0
   10 continue
c
c     Copy preds array and append it with latitude and vmaxop
      do 12 i=1,nco
	 predt(i) = preds(i) 
  12  continue
      predt(nco+1) = abs(slat)
      predt(nco+2) = vmaxop
c
c***  Create predictors 21 (tmax^2), 22 (tmax*clwave), 
c     23 (clwave^2), and 24 (p600=pmin+dp0)
      predt(nco+3) = preds(4) * preds(4)
      predt(nco+4) = preds(4) * preds(17)
      predt(nco+5) = preds(17) * preds(17)
      predt(nco+6) = preds(1) + preds(2)
c***
c
c     Predict  max wind, minimum pressure, and azimuthally averaged
c     34, 50 and 64 kt wind radii
      pvmx   = cvmx(0)
      ppmin  = cpmn(0)
      rb34   = cr34(0)
      rb50   = cr50(0)
      rb64   = cr64(0)
c
      do 15 k=1,nct
	 pvmx  = pvmx  + predt(k)*cvmx(k)
	 ppmin = ppmin + predt(k)*cpmn(k)
	 rb34  = rb34  + predt(k)*cr34(k)
	 rb50  = rb50  + predt(k)*cr50(k)
	 rb64  = rb64  + predt(k)*cr64(k)
   15 continue
c
c***  Modified prediction of MSLP (ppmin) is of ln(1050-MSLP),
c     so need to calculate MSLP (ppmin) from that
      ppmin = 1050. - exp(ppmin)
c***
c
      if (rb34 .lt. 0.0) rb34=0.0
      if (rb50 .lt. 0.0) rb50=0.0
      if (rb64 .lt. 0.0) rb64=0.0
c
      pr34(5) = rb34
      pr50(5) = rb50
      pr64(5) = rb64
c
      if (ias .eq. 1) return
c
c     Calcuate asymmetric wind radii
      call wrasym(rb34,rb50,rb64,vmaxop,spd,head,rm,x,pr34,pr50,pr64,
     .     slat)
c
c     Switch assymetry around for southern hemisphere
c     J. Knaff - January 10, 2003
c      if (slat.lt.0.0) then
c         do i=1,4
c            dummy(i)=pr34(i)
c         enddo
c         pr34(1)=dummy(4)
c         pr34(4)=dummy(1)
c         pr34(2)=dummy(3)
c         pr34(3)=dummy(2)
c         do i=1,4
c            dummy(i)=pr50(i)
c         enddo
c         pr50(1)=dummy(4)
c         pr50(4)=dummy(1)
c         pr50(2)=dummy(3)
c         pr50(3)=dummy(2)
c         do i=1,4
c            dummy(i)=pr64(i)
c         enddo
c         pr64(1)=dummy(4)
c         pr64(4)=dummy(1)
c         pr64(2)=dummy(3)
c         pr64(3)=dummy(2)
c      endif       
      return
      end
      subroutine wrasym(rb34,rb50,rb64,vmx,spd,head,rm,x,pr34,pr50,pr64,
     .     slat)
c
c     The routine calculates the wind radii in 4 quadrants relative
c     to the storm center (NE,SE,SW,NW) given the azimuthally
c     average wind radii (rb34,rb50,rb64) the max winds (vmx),
c     and the speed and heading of the storm motion (spd,head).
c
c     An idealized Rankine vortex with a wave number one asymmetry
c     is fitted to the mean wind radii to give the wind radii in
c     each qaudrant. The parameters of the Rankine vortex
c     (maximum wind radius rm and decay exponent x) are also returned. 
c
c     Input:  rb34 - azimuthally averaged 34 kt wind radius
c             rb50 - azimuthally averaged 50 kt wind radius
c             rb64 - azimuthally averaged 64 kt wind radius
c             vmx  - maximum wind (kt)
c             spd  - storm speed of motion (kt)
c             head - storm heading (deg clockwise from N)
c             slat - storm latitude (degrees)
c
c     Output: pr34(4) - array with 34 kt wind radii (nm)  NE,SE,SW,NW of center
c             pr50(4) - array with 50 kt wind radii (nm)  NE,SE,SW,NW of center
c             pr64(4) - array with 64 kt wind radii (nm)  NE,SE,SW,NW of center
c             rm     - radius of max winds (nm) from vortex model fit
c             x      - Decay exponent from vortex model fit
c       
      dimension pr34(6),pr50(6),pr64(6)
c
c     Internal work array
      dimension cf(0:125,0:125)
c
c     Set weights for climatology penalty terms for x, rm
      al1 = 0.1
      al2 = 0.1
c
c     Specify angle for adjusting max winds relative to the direction
c     90 deg to the right of the direction of motion
      theta0 = 0.0
c
c     Specify weight for adjusting the asymmetry factor
      rasf = 0.6
c
c     Specify weight for case when wind threshold is too close
c     to vmx, or set wttc=-1.0 to calculate wttc based upon azimuthal
c     distance covered by each wind radii. 
      wttc = -1.0
c
c     Specify search interval for x,rm
      x0 = 0.01
      dx = 0.01 
      nx = 125
c
      rm0 = 5.0
      dr  = 1.0
      nr  = 125
c
c     Initialize output variables to zero
      do 10 k=1,4
         pr34(k) = 0.0
         pr50(k) = 0.0
         pr64(k) = 0.0
   10 continue
c
      x  = 0.0
      rm = 0.0
c
c     Calculate asymmetry factor from spd
      if (spd .le. 0.0) then
         a = 0.0
      else
         a = rasf*1.5*(spd**0.63)
      endif
c
      vmxa  = vmx - a
      vmx2a = vmx - 2.0*a
c
c     Find azimuth covered by each wind radius
      if (a .le. 0.0) then 
         ac34 = 360.0
         ac50 = 360.0
         ac64 = 360.0
      else
         pi  = 3.14159
         rtd = 180.0/pi
c
         if (vmx2a .ge. 34.0) then
            ac34 = 360.0
         else
            if (vmx .le. 34.0) then
               ac34 = 0.0
            else
               ac34 = 2.0*rtd*acos(1.0 - (vmx-34.0)/a)
            endif
         endif
c
         if (vmx2a .ge. 50.0) then
            ac50 = 360.0
         else
            if (vmx .le. 50.0) then
               ac50 = 0.0
            else
               ac50 = 2.0*rtd*acos(1.0 - (vmx-50.0)/a)
            endif
         endif
c

         if (vmx2a .ge. 64.0) then
            ac64 = 360.0
         else
            if (vmx .le. 64.0) then
               ac64 = 0.0
            else
               ac64 = 2.0*rtd*acos(1.0 - (vmx-64.0)/a)
            endif
         endif
      endif
c
c     Set wttc variables
      aclow = 180.0
      wtval = 0.1
c
      if (wttc .lt. 0.0) then
         if (ac34 .lt. aclow) then
            wttc34 = wtval
         elseif (ac34 .ge. 360.0) then
            wttc34 = 1.0
         else
            wttc34 = wtval + (1.0-wtval)*(ac34-aclow)/(360.0-aclow)
         endif
c
         if (ac50 .lt. aclow) then
            wttc50 = wtval
         elseif (ac50 .ge. 360.0) then
            wttc50 = 1.0
         else
            wttc50 = wtval + (1.0-wtval)*(ac50-aclow)/(360.0-aclow)
         endif
c
         if (ac64 .lt. aclow) then
            wttc64 = wtval
         elseif (ac64 .ge. 360.0) then
            wttc64 = 1.0
         else
            wttc64 = wtval + (1.0-wtval)*(ac64-aclow)/(360.0-aclow)
         endif
      else
         wttc34 = wttc
         wttc50 = wttc
         wttc64 = wttc
      endif
c
c     Check maximum wind and set values accordingly.
      if (vmx .lt. 34.0) then
         return
      elseif (vmx .ge. 34.0 .and. vmx .lt. 50.0) then
         w34 = wttc34
         w50 = 0.0
         w64 = 0.0
      elseif (vmx .gt. 50.0 .and. vmx .lt. 64.0) then
         w34 = wttc34
         w50 = wttc50
         w64 = 0.0
      else
         w34 = wttc34
         w50 = wttc50
         w64 = wttc64
      endif
c
c     Calculate climatological rm,x and their standard deviations 
c     from empirical formulas
      rmc = 54.0 - .27*vmx
      if (rmc .lt. 18.0) rmc = 18.0
c
      srm = 33.0 - .21*vmx
      if (srm .lt.  6.0) srm = 6.0
c
      xc = .42 + .0025*vmx
      sx = .10
c
c     Specify wind radii standard deviations (indep. of vmax)
      s34 = 43.0
      s50 = 30.0
      s64 = 22.0
c
c     write(6,810) rmc,srm,xc,sx,ac34,ac50,ac64,w34,w50,w64,a
c 810 format(' rm mean,std: ',f5.1,1x,f5.1,/,
c    +       '  x mean,std: ',f5.3,1x,f5.3,/,
c    +       ' ac34,50,64:  ',f5.1,1x,f5.1,1x,f5.1,/,
c    +       ' wt34,50,64:  ',f5.3,1x,f5.3,1x,f5.3,/,
c    +       ' asym factor: ',f5.1)
c
c     Start search loop for x,rm
      do 20 i=0,nx
      do 20 j=0,nr
         rmt = rm0 + float(j)*dr
         xt  = x0  + float(i)*dx
c
c        Calculate mean radii for current values of x,rm
         call rbar(vmx,34.0,a,rmt,xt,tb34)
         call rbar(vmx,50.0,a,rmt,xt,tb50)
         call rbar(vmx,64.0,a,rmt,xt,tb64)
c
c        Calculate cost function
         cf(i,j) = w34*((tb34-rb34)/s34)**2 +
     +             w50*((tb50-rb50)/s50)**2 +
     +             w64*((tb64-rb64)/s64)**2 +
     +             al1*((xt  -xc  )/sx )**2 +
     +             al2*((rmt -rmc )/srm)**2
   20 continue
c
      iprt = 0
      if (iprt .eq. 1) then
c        Print cost function
         write(6,300)
  300    format(/,' COST FUNCTION')
c
         do 30 j=nr,0,-1
            write(6,310) j,(cf(i,j),i=0,nx)
  310       format(1x,i2,1x,11(f4.1,1x))
   30    continue
         write(6,320) (i,i=0,nx)
  320    format(4x,11(1x,i2,2x))
      endif
c
c     Find cost function minimum
      cmin = 1.0e+10
      do 40 j=0,nr
      do 40 i=0,nx
         if (cf(i,j) .lt. cmin) then
            imin = i
            jmin = j
            cmin = cf(i,j)
         endif
   40 continue
c
      x = x0 + dx*float(imin)
      rm= rm0+ dr*float(jmin)
c
c     Calculate best fit mean radii
      call rbar(vmx,34.0,a,rm,x,fb34)
      call rbar(vmx,50.0,a,rm,x,fb50)
      call rbar(vmx,64.0,a,rm,x,fb64)
c
c     write(6,200) fb34,fb50,fb64
c 200 format('  Fit 34,50,64 kt wind radii:  ',3(f5.0,1x))
c    
c     Put fit to mean radii in element 6 of pr arrays
      pr34(6) =fb34
      pr50(6) =fb50
      pr64(6) =fb64
c
c     Calculate wind radii in each quadrant
      pi = 3.14159
      dtr= pi/180.0
      xi = 1.0/x
c
      hemfac=1.0
      if (slat.lt.0.0)then  ! a knaff change for SH.
         hemfac=-1.0
      endif
      do 50 k=1,4
         q = 45.0 + 90.0*(float(k-1))
         theta = dtr*(head + hemfac*90.0 - q -theta0)
c
         pr34(k) = rm*( (vmx-a)/(34.0-a*cos(theta)) )**xi
         if (pr34(k) .lt. rm) pr34(k) = 0.0
c
         pr50(k) = rm*( (vmx-a)/(50.0-a*cos(theta)) )**xi
         if (pr50(k) .lt. rm) pr50(k) = 0.0
c
         pr64(k) = rm*( (vmx-a)/(64.0-a*cos(theta)) )**xi
         if (pr64(k) .lt. rm) pr64(k) = 0.0
   50 continue
c
      return
      end
      subroutine rbar(vm,v,a,rm,x,rb)
c     This routine calculates the azimuthally averaged wind radii 
c     (rb) for wind speed v, max wind vm, asymmetry factor a, radius
c     of max wind rm, and Rankine vortex factor x.  
c
c     Check for illegal values of x,rm
      if (x .le. 0.0 .or. rm .le. 0.0) then
         write(6,100) 

  100    format(/,' Illegal values of x or rm input to routine rbar')
         stop
      endif
c
c     Check for wind threshold greater than max wind
      if (v .gt. vm) then
         rb = 0.0
         return
      endif
c
      pi = 3.14159
      dtr= pi/180.0
      xi = 1.0/x
c
      nt = 72
      dt = 360.0/float(nt)
c
      rb = 0.0
c***   Set up new counter to only average azimuths with wind radii > 0
      ncount = 0
c***
      do 10 i=1,nt
         theta = dtr*dt*float(i)
         fac = (vm-a)/(v-a*cos(theta))
         if (fac .lt. 1.0) then
            fac = 0.0
         else
            fac = fac**xi
c***  Increment new counter for azimuths with wind radii > 0	    
	    ncount = ncount + 1
c***
         endif
c
         rb = rb + fac
   10 continue
c
c***  Check that ncount isn't 0 and modify calculation of rb
c     so that it's only averaged over azimuths with wind radii > 0      
      if (ncount .le. 0) then
         rb = rm*(1.1)
      else 
        rb = rm*rb/float(ncount)
      endif
c      rb = rm*rb/float(nt)      
c***
c
      return
      end
