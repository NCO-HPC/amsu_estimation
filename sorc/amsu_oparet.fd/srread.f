      subroutine srread(ludat,jyr,jlday,jtime,swlat,swlon,
     +            iyr,imon,iday,itime,slat00,slon00,slat12,slon12,
     +            idir,ispeed,ivmax,atcfid,sname,nsat,
     +            npoints,xlat,xlon,temps,clw,tpw,
     +            coord,times,istat)
c
c     Subroutine to read the file containing storm parameters and AMSU-A
c     temperature retrieval information. 
c
c     Input: 
c       ludat     - unit number of input file
c
c     Output:
c    
c     Temperatures are retrieved at 40 levels and are listed from top to
c     bottom at each scan point.  The levels (mb) of the retrievals are
c     0.1, 0.2, 0.5, 1.0, 1.5, 2, 3, 4, 5, 7, 10, 15, 20, 25, 30, 50, 60,
c     70, 85, 100, 115, 135, 150, 200, 250, 300, 350, 400, 430, 475, 500,
c     570, 620, 670, 700, 780, 850, 920, 950, 1000.
c
c     Cloud liquid water and total precipitable water are also calculated by
c     the retrieval code and are returned in clw and tpw 
c
c     Points in data array can be accumulated in multiple calls to 
c     swread. The starting index for storing data at each call
c     is ns. 
c
*****************************************************************************
c
c     INPUT:
c     ludat  - unit number of the ascii temperature retrieval to be read from
c
c     OUTPUT:
c     jyr                 - Year of AMSU data
c     jlday               - Julian Day of AMSU data
c     jtime               - Time at center of AMSU data (hhmmss in UTC)
c     swlat               - Lat (deg N) of center of AMSU data
c     swlon               - Lon (deg E) of center of AMSU data
c     iyr                 - Year of storm
c     imon                - month of storm
c     iday                - day of storm
c     itime               - Time (hr in UTC) of storm
c     slat00              - Lat of storm center 
c     slon00              - Lon of storm center
c     slat12              - Lat of storm center at t-12hr
c     slon12              - Lon of storm center at t-12 hr
c     idir                - Storm heading (deg)
c     ispeed              - Storm translational speed (kt)
c     ivmax               - Storm max winds (kt)
c     atcfid              - ATCF ID (a6)
c     sname               - Storm name (a9)
c     nsat                - number of the satellite (15=NOAA15, 16=NOAA16, etc)
c     npoints             - number of points added to the output arrays 
c     xlat(npoints)       - array of latitudes matching the points in data
c     xlon(npoints)       - array of longitudes matching the points in data
c     temps(npoints,mpas) - array of temperature retrieval data at all 40 levels
c     clw(npoints)        - array of cloud liquid water
c     tpw(npoints)        - array of total precipitable water
c     coord               - The "COORDINATES" line as an 80 character variable
c     times               - The "TIMES" line as an 80 character variable
c     istat               - status of the read (ok if = 0)
c                           istat=1 - end of file reached
c                           istat=2 - error encountered
c                           istat=3 - mxas too small
c                           istat=4 - No temperature data found
c
      character *6 atcfid
      character *9 sname
      character *90 coord,times
c
c     Note: These parameters must match those in the main oparet program
      parameter(mxas=7000,mpas=40)
c
      dimension xlat(mxas),xlon(mxas),clw(mxas),tpw(mxas)
      dimension temps(mxas,mpas)
c
c     Internal variable (placeholder for AMSU brightness temperatures)
      parameter (nchan=15)
      dimension btemps(nchan)
c
c     Default satellite number
      nsat = 99
c
      read(ludat,101,err=920,end=910) coord
      read(ludat,101,err=920,end=910) times
  101 format(a90)
      read(coord,100,err=920,end=910) iyr,imon,iday,itime,
     +                                slat00,slon00,slat12,slon12,
     +                                idir,ispeed,ivmax,atcfid,sname
  100 format(3x,i4,i2,i2,8x,i2,
     +       4(f7.1),
     +       1x,i3,1x,i3,5x,i3,5x,a6,2x,a9)
c
      read(times,110,err=920,end=910) jyr,jlday,jtime,swlat,swlon
  110 format(3x,i4,i3,1x,i6,4(1x,f8.2))
c
      read(ludat,120,err=920,end=910) npoints
  120 format(i6)
c
      if (npoints .gt. mxas) go to 930
      if (npoints .le.    0) go to 940
c
      do i=1,npoints
         read(ludat,130,err=920,end=910) xlat(i),xlon(i),nsat,
     +                                   (btemps(k),k=1,nchan),
     +                                   (temps(i,k),k=1,mpas),
     +                                   tpw(i),clw(i)
  130    format(15x,f9.3,f9.3,1x,i3,
     +          45x,15(f8.2),
     +          40(f8.2),
     +           2(f8.2))
      enddo
c
      istat=0
      return
c
 910  continue
      istat=1
      close(ludat)
      return
c
 920  continue
      istat=2
      close(ludat)
      return
c
 930  continue
      istat=3
      close(ludat)
      return
c
 940  continue
      istat=4
      close(ludat)
      return
c
      end
