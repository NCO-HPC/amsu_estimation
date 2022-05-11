      program oparet
c     This program performs retrievals of temperature, heights,
c     surface pressure and winds using the AMSU temperatures at
c     the satellite footprints. 
c
c     This is the updated version where all satellite and tropical cyclone
c     information is in a single file. 
c
c     File Updates:
c        New version created        5/15/2003     M. DeMaria
c        ATCF fix file option added 9/26/2005     M. DeMaria and J. Knaff
c        DATELINE ISSUE Resolved    2/20/2006     J. Knaff
c
c
c     Input files: 
c           temp_ret.dat        - File with storm input parameters 
c                                 and AMSU-A radiances and temp/CLW retrievals
c                                 (combination of temp, times and coordinate 
c                                  files, in the old version of oparet)
c                                     
c           AVN.DAT             - Packed ASCII NCEP analysis file
c
c     Output files:
c           oparet.log          -      ASCII file with basic information about the 
c                                      retrieval calcuations
c           Lbnnyy_mmddtt.DATss - loc: ASCII file containing lat/lons of AMSU input data 
c
c           Rbnnyy_mmddtt.DATss - rza: ASCII file containing V,T,P,rho as function of 
c                                      (r,z) from AMSU retrieval
c           Abnnyy_mmddtt.DATss - xya: ASCII file containing U,V,T,Z,Ps,rho,CLW  
c                                      as function of (x,y,P) from AMSU retrieval
c
c           Sbnnyy_mmddtt.DATss - sta: ASCII file containing statistical parameters for 
c                                      the TC estimation algorithm
c           Fbnnyy_mmddtt.DATss - fix: ASCII file containing estimated TC intensity/size 
c
c           Gbnnyy_mmddtt.DATss - afx: ATCF formatted fix file
c
c           Abnnyy_XmmDD_PACK.DAT - apk: Packed ASCII file containing U,V,T,Z,Ps
c                                        as function of (x,y,P) from the AMSU retrieval
c           Nbnnyy_XmmDD_PACK.DAT - npk: Packed ASCII file containing U,V,T,Z,Ps 
c                                        as function of (x,y,P) from NCEP fields 
c                                        interpolated to the AMSU analysis grid
c    
c           All of the output file names can be replaced by generic file
c           names by setting the parameter igenfn=1. The files will be of the 
c           for NOAAss.olg, .loc, .rza, etc according the to above list.
c
c           Note: b  = basin ID (A=Atlantic, E=East Pacific, W=West Pacific)
c                 nn = ATCF storm number
c                 yy = year
c                 mm = month
c                 dd = day
c                 tt = time in UTC of ATCF record used for storm center
c                 ss = satellite number (15=NOAA15, etc)
c
c                 X  = X if 00 or 12 UTC analysis
c                    = Y if 06 or 18 UTC analysis
c                 DD = dd    if 00 or 12 UTC analysis
c                    = dd+50 if 06 or 18 UTC analysis
c
c     Specify the lon/lat dimensions of the AMSU analysis domain
      parameter (nx=61,ny=61)
c
c     Specify number of AMSU pressure levels for calculations
c     and first pressure (out of 40, see array pamsu) to use
      parameter (np=23,npst=16)
c
c     Specify number of NCEP pressure levels
      parameter (npn=12)
c
c     Specify maximum dimensions of NCEP grid arrays
      parameter(ixmax=181,iymax=91)
c
c     Specify max number of AMSU swath points and pressure levels
      parameter (mxas=7000,mpas=40)
c
c     Specify number of radial and height points for gradient wind
c     calculations
      parameter (nr=31,nz=21)
c
c     Arrays for AMSU variables on analysis grid
      dimension p(np)
      dimension z(nx,ny,np),t(nx,ny,np)
      dimension us(nx,ny),vs(nx,ny),ps(nx,ny),ts(nx,ny),rhos(nx,ny)
      dimension clwxy(nx,ny)
c 
c     Array for AMSU swath variables
      dimension tas(mxas,mpas),aslat(mxas),aslon(mxas)
      dimension dumasp(mxas,mpas),dumas(mxas),idumas(mxas)
      dimension clwas(mxas),tpwas(mxas),pamsu(mpas)
c
c     Arrays for AMSU output (at NCEP pressure levels)
      dimension ua(nx,ny,npn),va(nx,ny,npn),za(nx,ny,npn),ta(nx,ny,npn)
c
c     Arrays for NCEP analysis variables     
      dimension pn(npn)
      dimension un(nx,ny,npn),vn(nx,ny,npn),zn(nx,ny,npn),tn(nx,ny,npn)
      dimension psn(nx,ny),tsn(nx,ny)
c
c     Array for NCEP 1000 mb T for clw temperature adjustment routine
      dimension tn1000(ixmax,iymax)
c
c     Arrays for gradient wind calculations
      dimension rr(nr),trp(nr,np),psr(nr),tsr(nr)
      dimension zrp(nr,np),vrp(nr,np),zz(nz)
      dimension trz(nr,nz),prz(nr,nz),rhorz(nr,nz),vrz(nr,nz)
      dimension tarz(nr,nz)
c
c     Lat/Lon arrays
      dimension rlond(nx),rlatd(ny)
      dimension rlonr(nx),rlatr(ny)
      dimension sinlat(ny),coslat(ny),tanlat(ny)
c
c     Temporary arrays for radial Barnes analysis
      parameter (mxb=nx*ny)
      dimension temlat(mxb),temlon(mxb),ftem(mxb)
c
c     Array for predictors for the stats file
      parameter (npred=18)
      dimension preds(npred)
c
c     Arrays for statistical wind radii prediction
      dimension pr34(6),pr50(6),pr64(6)
      dimension ir34(4),ir50(4),ir64(4)
c
c     Work arrays
      dimension work1(nx,ny),work2(nx,ny),work3(nx,ny)
      dimension work4(nx,ny),work5(nx,ny),work6(nx,ny)
      dimension worky(ny)
      dimension works(mxas)
c
c     Arrays for Cartesian grid calculations
      dimension x(nx),y(ny),fy(ny),ibwin(npn)
c
      character *1  rawdid
      character *2  cnsat,cbasin
      character *3  lab3
      character *6  atcfid,noaass,cstype
      character *9  sname
      character *14 ctimdat
      character *20 label
      character *23 labelr
c
c     Variables for input file names
      character *25 fntinp,fnninp
c
c     Variables for output file names
      character *25 fnlog,fnloc
      character *25 fnnpk,fnapk
      character *25 fnrza,fnxya
      character *25 fnsta,fnfix,fnafx
c 
c     Variables for output file headers
      character *90 coord,times
c
c     Variable for temporary file names
      character *25 fntemp
c
      common /cons/ pi,g,rd,dtr,erad,erot
      common /cgrid/ x,y,fy,dx,dy,f0,beta
      common /log/  lulog
      common /ncepfg/ rlonln,rlatbn,dlonn,dlatn,nlonn,nlatn
      common /ncepll/ rlond,rlonr,rlatd,rlatr,dlon,dlat,
     +                dlonr,dlatr,sinlat,coslat,tanlat
      common /ncepp/ pn
      common /sinfo1/ atcfid,sname
      common /sinfo2/ iyr,imon,iday,itime4,
     +                jyr,jmon,jday,jtime4,
     +                ivmax,spd,head,
     +                slat00,slat12,sslat,
     +                slon00,slon12,sslon,
     +                rmp,xp,pr34,pr50,pr64
c
c     Unit numbers for input files
      data lutinp,luninp
     +     /21,22/
c
c     Unit numbers for output files
      data luloc,lulog,lunpk,luapk,lurza,lusta,lufix,luafx,luxya
     +     /31,32,33,34,35,36,37,38,39/
c
c     Constants
      data  pi,      dtr,       g,    rd,    erad,      erot 
     +     /3.14159, 0.0174533, 9.81, 287.0, 6371.0e+3, 7.292e-5/
c
c     Specify input file with storm parameters and AMSU temperatures
      data fntinp /'temp_ret.dat'/
c
c     NCEP input data file name
      data fnninp /'AVN.DAT'/
c
c     Output log file name
      data fnlog /'oparet.log'/
c
c     Total AMSU pressure levels 
      data pamsu / 0.1, 0.2, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 7.0,
     +             10., 15., 20., 25., 30., 50., 60., 70., 85.,100.,
     +            115.,135.,150.,200.,250.,300.,350.,400.,430.,475.,
     +            500.,570.,620.,670.,700.,780.,850.,920.,950.,1000./
c
c     NCEP pressure levels
      data pn / 100., 150., 200., 250., 300., 400.,
     +          500., 600., 700., 850., 925.,1000./
c
c     Flag array for calculating balanced winds at each NCEP pressure level.
c     The calculation is performed (skipped) if ibwin=1 (0). 
      data ibwin / 1, 1, 1, 1, 1, 1,
     +             1, 1, 1, 1, 1, 1/
c
c     Open log file
      open(unit=lulog,file=fnlog,form='formatted',
     +     status='unknown')
c
c     **** Begin Parameter Specification ****
c
c     ++ 3-D analysis grid parameters
c
c     Specify lat/lon spacing of AMSU analysis grid
      dlat = 0.20
      dlon = 0.20
c
c     Set idex=1 to exclude data outside of analysis domain. Also
c     set lat/lon buffers for exclusion
      idex = 1
      rlonbf =3.0
      rlatbf =3.0
c
c     Specify maximum distance (km) the storm can be from the swath center
c     to perform an analysis
      threshr =700.0
c
c     ++ Gradient wind parameters
c
c     Set iaxsym =1 for axisymmetric analysis
      iaxsym=1
c
c     Specify the radial and height spacing (m) for gradient wind calculations
      dr = 20.0e+3
      dz = 1.0e+3
c
c     ++ 3-D wind parameters
c     Set iwinda = 0 to use NCEP x,y winds  
c                = 1 to use linear balance equation for x,y winds
c                = 2 to use nonlinear balance equation for x,y winds
c                           (iterative solution)
c                = 3 to use nonlinear balance equation for x,y winds
c                           (variational solution, u,v form)
c                = 4 to use nonlinear balance equation for x,y winds
c                           (variational solution, psi form)
      iwinda=3
c
c     If iwinda = 2, 3 or 4 , select method for first guess u,v:
c        Set ifgnbe = 0 to use NCEP winds as first guess 
c        Set ifgnbe = 1 to use linear balance winds at first guess
c        Set ifgnbe = 2 to use zero curvature winds as first guess
      ifgnbe=0
c
c     ++ Set flags for output files (0=don't write file, 1=write file)
c     
c     Set igenfn=1 to use generic output file names, otherwise igenfn=0
      igenfn=1
c
c     fix file (requires iaxsym=1, ifixall=1 writes fix file even if TC
c                                  analysis is not performed)
      ipfix = 1
      ifixall = 1
c
c     loc file
      iploc = 1 
c
c     rza file (requires iaxsym=1)
      iprza=1
c
c     xya file 
      ipxya = 1 
c
c     sta file (requires iaxsym=1 and ivrapd=1)
      ipsta = 1  
c
c     apk file 
      ipapk = 0
c
c     npk file
      ipnpk = 0
c
c     ++ Barnes analysis parameters
c
c     Specify e-folding and influence radii (km) for 2-D Barnes analysis
c     of AMSU temperatures.
c     Rule of thumb: rinfxy = efldxy*5, but rinfxy must be at least 400
c      efldxy=40.0
c      rinfxy=200.0
c     efldxy=100.0
      efldxy=100.0
      rinfxy=6.0*efldxy
c
c     Set ispf=1 for a second pass of the x,y Barnes analyses or
c         ispf=0 for single pass
      ispf=1
c
c     Specify e-folding and influence radii (km) for 1-D Barnes analysis
c     of AMSU temperatures. Also specify expansion factor (exfac) for
c     increasing e-folding radius as a function of radius. 
      efldr=20.0
      rinfr=100.0
c     efldr=120.0
c     rinfr=480.0
      exfac=0.0
c
c     Set iradxy=1 to perform radial Barnes temperature analysis using
c     gridded temperature from x,y analysis instead of swath data
      iradxy=1 
c
c     Set iefa = 1 to adjust e-folding radii based upon horizontal 
c     resolution, else set iefa=0. Also, specify reduction
c     factor for 2dx wave, to choose e-folding radius.
      iefa = 0
c     tdxfxy = 0.05
c     tdxfr  = 0.20 
      tdxfxy = 0.04
      tdxfr  = 0.16
c
c     ++ CLW attenuation and ice scattering corretion parameters
c
c     Set itcor   = 0-10 to correct cold anomalies for water 
c                   vapor attenuation. 
c                   If itcor=0 no correction is applied 
c                   If itcor=1  temp analysis is corrected, using a
c                               cold anomaly threshold
c                   If itcor=2  original swath data is corrected using a
c                               cold anomaly threshold
c                   If itcor=3  original swath data is corrected using a
c                               linear regression technique
c                   If itcor=4  original swath data is corrected using the
c                               linear regression method (3), followed
c                               by the cold anomaly threshold method (2). 
c                   If itcor=5  orginal swath data is adjusted in regions
c                               with high liquid water content to a 
c                               constant lapse rate sounding
c                   If itcor=6  method 3 is applied, followed by method 5
c                   If itcor=7  method 3, 5 and 2 are applied
c                   If itcor=8  temp analysis corrected; quadratically 
c                               smoothed m(k) from Tdev vs CLW regression
c                               slopes; m(k)=ap^2+bp+c where p=pressure in Pa
c                               and b=c=0 according to initial conditions.
c                   If itcor=9  temp analysis corrected, for ice crystals
c                               via B. Linstid's algorithm
c                   If itcor=10 temp analysis correction via method (8) and (9)
c
c         ncemx,ncsmx = first,last AMSU level (out of analysis levels) 
c                       to apply correction (x=2 applies to method 1 or 2,
c                                            x=3 applies to method 3,
c                                            x=5 applies to method 5,
c                                            x=8 applies to method 8 or 10,
c                                            x=9 applied to method 9 or 10)
c         dt          = anomaly threshold for applying correction
c                       (for itcor=1 or 2)
c         dtred       = Factor to reduce fraction of cold anomaly below dt
c                       (for itcor=1 or 2)
c         tcrmax      = Max radius (km) from storm for applying method 3
c
      itcor=10
c
c     Specify variables for method 1 and 2
      ncsm2 = np-5
      ncem2 = np
      dt    = -0.5
      dtred = 0.10
      tcrmax = 900.0
c
c     Specify variables for method 3
      ncsm3 = 11
      ncem3 = np
c
      ncs2 = ncsm2 + npst - 1
      nce2 = ncem2 + npst - 1
      ncs3 = ncsm3 + npst - 1
      nce3 = ncem3 + npst - 1
c
c     Specify variables for method 5
      ncsm5 = 13
      ncem5 = np
      clwth1 = 1.00
      clwth2 = 2.00
c
      ncs5 = ncsm5 + npst - 1
      nce5 = ncem5 + npst - 1
c
c     Specify variables for method 8
c     Variables for Ben Linstid's original correction
c      ncsm8 = 10
c      ncem8 = np
c      amkx = 5.78
c      xlambda = 1.55
c
c     Variables for Tdev vs CLW regression correction on grid
c     The first amkx value is with no core removed from the grid data,
c     and the second is with a 2 x 2 degree core removed (comment one out).
c      ncsm8 = 12
c      ncem8 = np
c      amkx = 4.27435
c      amkx = 4.04594
c      xlambda = 1.0
c
c     Variables for Tdev vs CLW regression correction on swath
      ncsm8 = 12 + npst - 1
      ncem8 = np + npst - 1
      amkx = 4.57166
      xlambda = 1.0
c
c     Specify variables for method 9
      ncsm9 = 12 
      ncem9 = np 
c
c     ++ NCEP analysis parameters
c
c     Set ismooth to the number of times to smooth the NCEP 
c     fields after interpolating them to the AMSU analysis grid
      ismooth=3
c
c     Set intop=1 to use NCEP height field at top of AMSU domain
c     as boundary condition. This is only possible if AMSU domain
c     top is at 100 mb. 
      intop=0
c
c     Set ibrem=1 to correct amsu T,Z and surface P to make the mean
c     equal to that in the NCEP analysis
      ibrem = 0
c
c     **** End Parameter Specification ****
c
c     **** Begin Storm/AMSU Data Input File Read and Related Processing ****
c
c     Open the storm parameter and temperature retrieval input file
      fntemp=fntinp
      open(unit=lutinp,file=fntinp,form='formatted',
     +     status='old',err=900)
c 
c     Read the storm/retrieval file
      call srread(lutinp,jyr,jlday,jtime,swlat,swlon,
     +            iyr,imon,iday,itime,slat00,slon00,slat12,slon12,
     +            idir,ispeed,ivmax,atcfid,sname,nsat,
     +            nxas,aslat,aslon,tas,clwas,tpwas,
     +            coord,times,istat)
      close(lutinp)
c
c     J. Knaff -- DATELINE ISSUE resolved 2/20/2006
c     This fix allows for the use of data across the dateline when storm 
c     is within 10 degrees of the dateline by using a 0 to 360 longitude
c     convention in that region.
c     Fix 
      if (abs(slon00).gt.170.0 .or. abs(slon12).gt.170.0)then
         if(slon00 .lt. 0.0 ) slon00 = slon00 + 360.0
         if(slon12 .lt. 0.0 ) slon12 = slon12 + 360.0
         if(swlon  .lt. 0.0 ) swlon  = swlon  + 360.0
         do ikn=1,nxas
            if(aslon(ikn) .lt. 0.0) aslon(ikn) = aslon(ikn) + 360.0
         enddo
      endif
c
c
      if (istat .ne. 0) go to 900
c
      call ucase(atcfid,6)
      cbasin=atcfid(1:2)
      iyr2 = iyr - 100*(iyr/100)
c 
      write(cnsat,309) nsat
  309 format(i2.2)
      noaass(1:4) = 'NOAA'
      noaass(5:6) = cnsat
c
      call jdayi(jlday,jyr,jmon,jday)
c
c     Extract AMSU pressures for calculations and convert to Pa
      do 5 k=1,np
         p(k) = 100.0*pamsu(npst+k-1)
    5 continue
c
c     Estimate storm center at AMSU swath center time
      jtimeh = jtime/10000
      jtimem = (jtime - 10000*jtimeh)/100
      jtime4 = 100*jtimeh + jtimem
c
      call tdiff(jyr,jmon,jday,jtimeh,iyr,imon,iday,itime,idelt)
      adelt = float(idelt) + float(jtimem)/60.0
      itime4 = 100*itime
c
      if (abs(slat12) .gt. 0.0 .and. 
     +    abs(slon12) .gt. 0.0       ) then
         w0 = 1.0 + adelt/12
         wm = -adelt/12
      else
	 w0 = 1.0
	 wm = 0.0
      endif
c
      sslat = w0*slat00 + wm*slat12
      sslon = w0*slon00 + wm*slon12
c
c     Calculate distance from storm to swath center
      call distk(swlon,swlat,sslon,sslat,dxk,dyk,rad)
      radsts=rad
c
c     Estimate AMSU resolution
      call amsures(rad,ares)
c
      if (iefa .eq. 1 .and. ares .gt. 0.0) then
c        Adjust e-folding radii based upon horizontal resolution of the data
         cofr  = 2.0*sqrt(alog(1./tdxfr))/pi
         cofxy = 2.0*sqrt(alog(1./tdxfxy))/pi
	 efldxy = float(ifix(cofxy*ares))
	 efldr  = float(ifix(cofr *ares))
c
	 if (efldxy .lt.  35.0) efldxy = 35.0
	 if (efldxy .gt. 150.0) efldxy = 150.0
c
	 if (efldr  .lt.  35.0) efldr  = 35.0
	 if (efldr  .gt. 150.0) efldr  = 150.0
c
         rinfxy = 5.0*efldxy
         rinfr  = 5.0*efldr
	 if (rinfxy .lt. 400.0) rinfxy = 400.0
	 if (rinfr  .lt. 400.0) rinfr  = 400.0 
      endif
c
c     Write storm parameter info to log file
      write(lulog,200) atcfid,sname,iyr,imon,iday,itime,
     +                 slat00,slon00,ivmax,slat12,slon12
  200 format(/,' Start program oparet. ',
     +       /,' ATCF INPUT: ',a6,1x,a9,1x,i4.4,1x,i2.2,i2.2,1x,
     +                                              i2.2,' UTC',
     +       /,' Storm lat/lon (t=  0 hr): ',f5.1,1x,f6.1,1x,
     +         ' Vmax: ',i3,
     +       /,' Storm lat/lon (t=-12 hr): ',f5.1,1x,f6.1)
c
      write(lulog,202) np,p(1)/100.,p(np)/100.
  202 format(/,i2,' AMSU levels from ',f5.0,' to ',f5.0,' included')
c
      write(lulog,204) efldxy,rinfxy
  204 format(/,' Cartesian Barnes analysis with e-fold, inf. radii: ',
     +         f5.0,1x,f5.0)
c 
      if (iaxsym .eq. 1) then
         write(lulog,206) efldr,rinfr,exfac
  206    format(' Radial    Barnes analysis with e-fold, inf. radii: ',
     +            f5.0,1x,f5.0,' exfac=',f4.1)
      endif
c 
      if (itcor .gt. 0) then
         write(lulog,207) itcor,p(ncsm2)/100.,p(ncem2)/100.,
     +                          p(ncsm3)/100.,p(ncem3)/100.,
     +                          p(ncsm5)/100.,p(ncem5)/100.,
     +                          clwth1,clwth2
  207    format(/,' Cold anomalies adjusted using method ',i2,
     +          /,' pm2= ',f5.0,' to ',f5.0,
     +          /,' pm3= ',f5.0,' to ',f5.0,
     +          /,' pm5= ',f5.0,' to ',f5.0,
     +          /,' clwth1=',f5.2,' clwth2=',f5.2)
      else
        write(lulog,*) ' itcor=0, no correction applied'
      endif
c
      write(lulog,210) swlat,swlon,jyr,jmon,jday,jtime
  210 format(/,'AMSU data swath center: ',f6.2,1x,f7.2,' at ',
     +       i4,1x,i2.2,i2.2,1x,i6)
c
      write(lulog,212) adelt,sslat,sslon,rad,ares,nxas,nsat
  212 format('Swath data is ',f5.1,' hr from analysis time',/,
     +       'Storm center at swath time:',f6.2,1x,f7.2,/,
     +       'Distance (km) from storm to swath center: ',f6.0,/,
     +       'Approximate data spacing at storm center: ',f6.0,/,
     +       'AMSU data points read from input,     n=: ',i5,/,
     +       'Data from NOAA',i2.2)
c
c     Check to make sure storm is not too close to the swath edge
      inhce = 0
      if (radsts .gt. threshr) then
	 write(lulog,905) radsts,threshr
  905    format(/,' Storm too far from swath center, r (km)=',f6.0,
     +            ' max allowable r (km) = ',f5.0)
	 inhce = 1
c
	 if (ifixall .eq. 1) go to 5000
	 stop
      endif
c
c     **** End Storm/AMSU Data Input File Read and Related Processing ****
c
c     **** Begin Calculation of Analysis Grid Parameters ****
c     Center the domain on the estimated storm position
c     at the swath time
      islat = ifix(sslat+0.5)
      islon = ifix(sslon-0.5)
c
      rlonl = float(islon) - dlon*float(nx/2)
      rlatb = float(islat) - dlat*float(ny/2)
c
c     Calculate lat and lon in deg and radians, and related variables
      dlonr = dlon*dtr
      dlatr = dlat*dtr
c
      do 10 i=1,nx
         rlond(i) = rlonl + dlon*float(i-1)
         rlonr(i) = dtr*rlond(i)
   10 continue
c
      do 12 j=1,ny
         rlatd(j) = rlatb + dlat*float(j-1)
         rlatr(j) = dtr*rlatd(j)
c
         sinlat(j) = sin(rlatr(j))
         coslat(j) = cos(rlatr(j))
         tanlat(j) = tan(rlatr(j))
   12 continue
c
c     Calcuate Cartesian grid variables
      reflat = 0.5*(rlatd(1)+rlatd(ny))
      reflon = 0.5*(rlond(1)+rlond(nx))
c
      do j=1,ny
	 y(j) = erad*dtr*(rlatd(j)-reflat)
      enddo
c
      crl = cos(dtr*reflat)
      do i=1,nx
	 x(i) = erad*dtr*(rlond(i)-reflon)*crl
      enddo
c
      dx = x(2)-x(1)
      dy = y(2)-y(1)
      f0   = 2.0*erot*sin(dtr*reflat)
      beta = 2.0*erot*cos(dtr*reflat)/erad
c
      do j=1,ny
	 fy(j) = f0 + beta*y(j)
      enddo
c
c     Calculate radial and height grids
      do 13 i=1,nr
	 rr(i) = dr*float(i-1)
   13 continue
c
      do 14 m=1,nz
	 zz(m) = dz*float(m-1)
   14 continue
c
c     Write grid info to the log file
      write(lulog,230) rlond(1),rlond(nx),dlon,
     +                 rlatd(1),rlatd(ny),dlat
  230 format(/,' Longitude domain: ',f6.1,1x,f6.1,'  dlon=',f5.2,/,
     +         ' Latitude domain:  ',f6.1,1x,f6.1,'  dlat=',f5.2)
c
      if (iaxsym .eq. 1) then
         write(lulog,232) nr,dr/1000.0,rr(1)/1000.0,rr(nr)/1000.0
  232    format(i3,' radial grid points, dr=',f5.1,1x,
     +            '  rmin=',f5.1,' rmax=',f6.1)
      endif
c
      write(lulog,234) nz,dz/1000.0,zz(1)/1000.0,zz(nz)/1000.0
  234 format(i3,' z grid points, dz=',f5.1,1x,
     +         '  zmin=',f5.1,' zmax=',f6.1)
c
      write(lulog,239) x(1),x(nx),dx,y(1),y(ny),dy,f0,beta
  239 format(/,' Cartesian grid created for x,y balanced winds: ',
     +       /,' x domain: ',e11.4,' to ',e11.4,'  dx=',e11.4,
     +       /,' y domain: ',e11.4,' to ',e11.4,'  dy=',e11.4,
     +       /,'   f0=',e11.4,' beta=',e11.4)
c
      if (iwinda .eq. 4) then
	 write(lulog,244) ifgnbe
  244    format(/,' AMSU x,y winds from nonlinear balance equation',
     +          /,' Variational solution (psi form) with ifgnbe= ',i1)
      elseif (iwinda .eq. 3) then
	 write(lulog,243) ifgnbe
  243    format(/,' AMSU x,y winds from nonlinear balance equation',
     +          /,' Variational solution (u,v form) with ifgnbe= ',i1)
      elseif (iwinda .eq. 2) then
	 write(lulog,242) ifgnbe
  242    format(/,' AMSU x,y winds from nonlinear balance equation',
     +          /,' Iterative solution with ifgnbe= ',i1)
      elseif (iwinda .eq. 1) then
	 write(lulog,241)
  241    format(/,' AMSU x,y winds from linear balance equation')
      else
	 write(lulog,240)
  240    format(/,' AMSU x,y winds from NCEP analysis')
      endif
c
c     **** End Calculation of Analysis Grid Parameters ****
c
c     **** Begin NCEP Input File Read and Related Processing ****
c
c     Open NCEP data input file
      fntemp=fnninp
      open(unit=luninp,file=fnninp,form='formatted',status='old',
     +     err=900)
c
c     Get NCEP analysis variables
      call ncepget(pn,un,vn,zn,tn,tn1000,rlond,rlatd,nx,ny,npn,
     +             dlon,dlat,dayx,utcx,luninp,lulog,ierr)
      close(luninp)
c
      if (ierr .ne. 0) then
         write(lulog,910) ierr
  910    format(/,' Halting due to error in routine ncepget, ierr=',
     +                                                             i2)
      endif
c
      if (ismooth .gt. 0) then
	 do n=1,ismooth
	    do k=1,npn
	       call smooth(un(1,1,k),work1,nx,ny,nx,ny,0)
	       call smooth(vn(1,1,k),work1,nx,ny,nx,ny,0)
	       call smooth(tn(1,1,k),work1,nx,ny,nx,ny,0)
	       call smooth(zn(1,1,k),work1,nx,ny,nx,ny,0)
            enddo
	 enddo
      endif
c
c     Convert NCEP date/time varibles to integers
      idayx = ifix(dayx)
      iutcx = ifix(utcx)
c
      nyrt  = idayx/10000
      nmon  = (idayx - 10000*nyrt)/100
      nday  = (idayx - 10000*nyrt - 100*nmon)
      ntime = iutcx/100
c
      if (nyrt .lt. 50) then
	 nyr = nyrt + 2000
      else
	 nyr = nyrt + 1900
      endif
c
c     Calculate time difference between NCEP analysis to AMSU swath data
      call tdiff(jyr,jmon,jday,jtimeh,nyr,nmon,nday,ntime,idelt)
      adelt = float(idelt) + float(jtimem)/60.0
c
      write(lulog,216) fnninp
  216 format(/,'NCEP data input file:    ',a25)
c
      write(lulog,236) nyr,nmon,nday,ntime,adelt
  236 format(/,'NCEP analysis date/time: ',i4,1x,2(i2.2),1x,i2,/,
     +         'NCEP analysis is ',f5.1,' hrs from AMSU swath')
c
      if (abs(adelt) .gt. 36.0) then
	 write(lulog,*) ' NCEP analysis is too old'
	 stop
      endif
c
c     Convert NCEP pressures to Pa
      do 15 k=1,npn
	 pn(k) = 100.0*pn(k)
   15 continue
c
c     Use data at lowest two NCEP levels to get surface temperature
c     and pressure
      do 17 j=1,ny
      do 17 i=1,nx
	 z1 = zn(i,j,npn)
	 z2 = zn(i,j,npn-1)
         t1 = tn(i,j,npn)
	 t2 = tn(i,j,npn-1)
	 p1 = pn(npn)
	 call pstcal(z1,z2,t1,t2,p1,pstem,tstem)
c
	 psn(i,j) = pstem
	 tsn(i,j) = tstem
   17 continue
c
c     **** End NCEP Input File Read and Related Processing ****
c
c     ***** Begin Output File Naming/Opening  ****
c
c     ++ Name for loc file (AMSU footprint locations)
      fnloc        = 'LX0000_000000.DAT'
      fnloc( 2: 2) = atcfid(1:1)
      fnloc( 3: 6) = atcfid(3:6)
      fnloc(18:19) = cnsat
      write(fnloc(8:13),226) imon,iday,itime
  226 format(3(i2.2))
c
      if (igenfn .eq. 1) then
	 fnloc='  '
	 fnloc(1:6) = noaass
	 fnloc(7:10) = '.LOC'
      endif
c
c     ++ Name for rza file (2-D AMSU retrieval output file)
      fnrza        = 'RX0000_000000.DAT'
      fnrza( 2: 2) = atcfid(1:1)
      fnrza( 3: 6) = atcfid(3:6)
      fnrza(18:19) = cnsat
      write(fnrza(8:13),226) imon,iday,itime
c
      if (igenfn .eq. 1) then
	 fnrza='  '
	 fnrza(1:6) = noaass
	 fnrza(7:10) = '.RZA'
      endif
c
c     ++ Name for xya file (3-D AMSU retrieval output file)
      fnxya        = 'AX0000_000000.DAT'
      fnxya( 2: 2) = atcfid(1:1)
      fnxya( 3: 6) = atcfid(3:6)
      fnxya(18:19) = cnsat
      write(fnxya(8:13),226) imon,iday,itime
c
      if (igenfn .eq. 1) then
	 fnxya='  '
	 fnxya(1:6) = noaass
	 fnxya(7:10) = '.XYA'
      endif
c
c     ++ Name for sta file (TC statistics file)
      fnsta        = 'SX0000_000000.DAT'
      fnsta( 2: 2) = atcfid(1:1)
      fnsta( 3: 6) = atcfid(3:6)
      fnsta(18:19) = cnsat
      write(fnsta(8:13),226) imon,iday,itime
c
      if (igenfn .eq. 1) then
	 fnsta='  '
	 fnsta(1:6) = noaass
	 fnsta(7:10) = '.STA'
      endif
c
c     ++ Create file name for fix file
      fnfix        = 'FX0000_000000.DAT'
      fnfix( 2: 2) = atcfid(1:1)
      fnfix( 3: 6) = atcfid(3:6)
      fnfix(18:19) = cnsat
      write(fnfix(8:13),226) imon,iday,itime
c
      if (igenfn .eq. 1) then
	 fnfix='  '
	 fnfix(1:6) = noaass
	 fnfix(7:10) = '.FIX'
      endif
c
c     ++ Create file name for afx file
      fnafx        = 'GX0000_000000.DAT'
      fnafx( 2: 2) = atcfid(1:1)
      fnafx( 3: 6) = atcfid(3:6)
      fnafx(18:19) = cnsat
      write(fnafx(8:13),226) imon,iday,itime
c
      if (igenfn .eq. 1) then
	 fnafx='  '
	 fnafx(1:6) = noaass
	 fnafx(7:10) = '.AFX'
      endif
c
c     ++ Name for npk file (packed NCEP output file)
      if (itime .eq. 6 .or. itime .eq. 18) then
	 rawdid = 'Y'
      else
	 rawdid = 'X'
      endif
c
      if (itime .ge. 12) then
	 idayr = iday+50
      else
	 idayr = iday
      endif
c
      fnnpk       = ' '
      fnnpk(1:1)  = 'N'
      fnnpk(2:2)  = atcfid(1:1)
      fnnpk(3:6)  = atcfid(3:6)
      fnnpk(7:21) = '_X0000_PACK.DAT'
      fnnpk(22:23)= cnsat 
      write(fnnpk(8:12),300) rawdid,imon,idayr
  300 format(a1,i2.2,i2.2)
c
      if (igenfn .eq. 1) then
	 fnnpk='  '
	 fnnpk(1:6) = noaass
	 fnnpk(7:10) = '.NPK'
      endif
c
c     ++ Name for apk file (packed 3-D AMSU retrieval output file)
c     on lat,lon,P grid
      fnapk=fnnpk
      fnapk(1:1) = 'A'
c
      if (igenfn .eq. 1) then
	 fnapk='  '
	 fnapk(1:6) = noaass
	 fnapk(7:10) = '.APK'
      endif
c
c     ++ Write file names to log file
      lab3   =  'loc'
      fntemp = fnloc
      iprt   = iploc
      write(lulog,218) lab3,fntemp,iprt
c
      lab3   =  'rza'
      fntemp = fnrza
      iprt   = iprza
      write(lulog,218) lab3,fntemp,iprt
c
      lab3   =  'xya'
      fntemp = fnxya
      iprt   = ipxya
      write(lulog,218) lab3,fntemp,iprt
c
      lab3   =  'sta'
      fntemp = fnsta
      iprt   = ipsta
      write(lulog,218) lab3,fntemp,iprt
c
      lab3   =  'fix'
      fntemp = fnfix
      iprt   = ipfix
      write(lulog,218) lab3,fntemp,iprt
c
      lab3   =  'afx'
      fntemp = fnafx
      iprt   = ipfix
      write(lulog,218) lab3,fntemp,iprt
c
      lab3   =  'apk'
      fntemp = fnapk
      iprt   = ipapk
      write(lulog,218) lab3,fntemp,iprt
c
      lab3   =  'npk'
      fntemp = fnnpk
      iprt   = ipnpk
      write(lulog,218) lab3,fntemp,iprt
  218 format(a3,' data output file: ',a25,' write flag=',i1)
c
c     ++ Open the output files, if necessary 
c        Note: Fix file is opened later if needed, depending
c              on the outcome of the TC retrieval
c
      if (iploc .eq. 1) then
          fntemp=fnloc
          open(unit=luloc,file=fnloc,form='formatted',status='unknown',
     +        err=900)
      endif
c
      if (iprza .eq. 1 .and. iaxsym .eq. 1) then
	 fntemp = fnrza
	 open(file=fnrza,unit=lurza,form='formatted',status='unknown',
     +        err=900)
      endif
c
      if (ipxya .eq. 1) then
         fntemp=fnxya
         open(unit=luxya,file=fnxya,form='formatted',status='unknown',
     +        err=900)
      endif
c
      if (ipsta .eq. 1 .and. iaxsym .eq. 1) then
         fntemp = fnsta
         open(file=fnsta,unit=lusta,form='formatted',status='unknown',
     +        err=900)
      endif
c
      if (ipapk .eq. 1) then
         fntemp=fnapk
         open(unit=luapk,file=fnapk,form='formatted',status='unknown',
     +        err=900)
      endif
c
      if (ipnpk .eq. 1) then
         fntemp=fnnpk
         open(unit=lunpk,file=fnnpk,form='formatted',status='unknown',
     +        err=900)
      endif
c
c     ++ Write NCEP variables at AMSU analysis grid points to packed output file
      if (ipnpk .eq. 1) then
         call wpof(un,vn,tn,zn,psn,iyr2,imon,iday,itime,lunpk)
         close(lunpk)
      endif
c
c     ++ Write amsu data locations to a file
      if (iploc .eq. 1) then
         call wloc(sslat,sslon,aslat,aslon,luloc,nxas,
     +             jyr,jmon,jday,jtimeh,jtimem,atcfid,sname)
         close(luloc)
      endif
c 
c     **** End Output File Naming/Opening  ****
c
c     **** Begin Processing of AMSU temperature/CLW data ****
      if (idex .eq. 1) then
c        Eliminate data outside analysis domain
	 call delim(tas,clwas,tpwas,aslat,aslon,
     +              rlatd(1),rlatd(ny),rlatbf,
     +              rlond(1),rlond(nx),rlonbf,
     +              dumas,dumasp,idumas,
     +              mxas,mpas,nxas,np,npst,lulog)
      endif
c
c     ++ Quality control the AMSU temperatures
      call qualcon(tas,clwas,tpwas,aslat,aslon,pamsu,dumas,
     +             dumasp,idumas,mxas,mpas,nxas,np,npst,lulog)
c
c     Write AMSU swath info to the log file
      write(lulog,238) nxas,aslat(   1),aslon(   1),
     +                            aslat(nxas),aslon(nxas)
  238 format(/,' Final AMSU swath data includes ',i4,' points',
     +       /,' First lat/lon: ',f5.1,1x,f6.1,/,
     +         ' Last  lat/lon: ',f5.1,1x,f6.1,/)
c
c     ++ Correct swath temperatures for cold anomalies if necessary
      if (itcor .eq. 3 .or. itcor .eq. 4 .or. itcor .eq. 6 .or.
     &    itcor .eq. 7) then
	 write(lulog,*) ' itcor=3 correction applied'
	 do 23 k=ncs3,nce3
	    call tcorsr(mxas,nxas,tas(1,k),clwas)
   23    continue
      endif
c
      if (itcor .eq. 5 .or. itcor .eq. 6 .or. itcor .eq. 7) then
	 write(lulog,*) ' itcor=5 correction applied'
	 call tcorsv(mxas,nxas,mpas,tas,tn1000,pamsu,
     +               clwas,aslon,aslat,clwth1,clwth2,ncs5,nce5,lulog)
      endif
c
      if (itcor .eq. 2 .or. itcor .eq. 4 .or. itcor .eq. 7) then
	 write(lulog,*) ' itcor=2 correction applied'
	 do 22 k=ncs2,nce2
	    call tcors(mxas,nxas,tas(1,k),clwas,aslon,aslat,
     +                 sslon,sslat,works,dt,dtred,tcrmax)
   22    continue
      endif
c
      if (itcor .eq. 8 .or. itcor .eq. 10) then 
         write(lulog,*) ' itcor=8 correction applied'
         do 26 k=ncsm8,ncem8
            amkl=((amkx*xlambda)/((pamsu(ncem8)-pamsu(ncsm8))**2))*
     &           (pamsu(k)-pamsu(ncsm8))**2
	    write(lulog,460) pamsu(k),amkl
  460       format(' itcor=8 correction for p=',f6.1,1x,' amkl=',e11.4)
            call tcorclw(mxas,nxas,amkl,tas(1,k),clwas,k,lulog)
   26    continue
      endif
c
c     ++ Interpolate temperatures to analysis grid
      do 20 k=1,np
	 do 25 i=1,nxas
	    dumas(i) = tas(i,npst+k-1)
   25    continue
c
	 call barxy(dumas,aslat,aslon,nxas,efldxy,rinfxy,ispf,
     +              rlatd,rlond,t(1,1,k),nx,ny,ierr)
	 if (ierr .ne. 0) then
	    write(lulog,940)
  940       format(/,'Error in barxy')
            stop
         endif
   20 continue
c
c     Interpolate cloud liquid water to analysis grid
      do i=1,nxas
	 if (clwas(i) .gt. 0.0) then
	    dumas(i) = clwas(i)
         else
	    dumas(i) = 0.0
         endif
      enddo
c
      call barxy(dumas,aslat,aslon,nxas,efldxy,rinfxy,ispf,
     +           rlatd,rlond,clwxy,nx,ny,ierr)
      if (ierr .ne. 0) then
         write(lulog,940)
         stop
      endif
c
c     Correct analysis temperatures for cold anomalies if necessary
      if (itcor .eq. 1) then
	 write(lulog,*) ' itcor=1 correction applied'
         do 21 k=ncsm2,ncem2
	    call tcor(nx,ny,t(1,1,k),dt,dtred)
   21    continue
      endif
c
c     If ice correction doesn't converge, the corresponding pressure 
c     level, storm, mon, day, and time will be output to the log file
      if (itcor .eq. 9 .or. itcor .eq. 10) then
         write(lulog,*) ' itcor=9 correction applied'
         do 27 k=ncsm9,ncem9
            write(lulog,470) p(k)/100.0
  470       format(' itcor=9 correction for p=',f6.1)
            call tcorice(k,nx,ny,t(1,1,k),clwxy,
     +                   work1,work2,work3,lulog,lulog,
     +                   atcfid,imon,iday,itime)
   27    continue
         write(lulog,*) 'Finished ice corrections.'
      endif
c
      itest=0
      if (itest .eq. 1) then
	  write (lulog,*) ' itest=1, so program is stopping!'
        stop
      endif
c
c     **** End Processing of AMSU temperature/CLW data ****
c
c     **** Begin hydrostatic integration and wind retrieval on 3-D grid ****
c
c     Specify surface temperature and pressure of AMSU analysis 
c     from NCEP analysis
c     (All pressures except lateral boundary values will be recomputed)
      do 30 j=1,ny
      do 30 i=1,nx
         ts(i,j) = tsn(i,j)
         ps(i,j) = psn(i,j)
   30 continue
c
c     Check for special case with top AMSU and NCEP levels of 100 mb.
c     In this case, the NCEP height field can be used as an upper
c     boundary condition.
      ispec=0
      if (p(1) .eq. 10000.0 .and. pn(1) .eq. 10000.0) then
	 ispec=1
      endif
c
      if (intop .eq. 1 .and. ispec .ne. 1) then
	 write(lulog,920)
  920    format(/,' AMSU domain top must be at 100 mb for intop=1')
	 stop
      endif
c
      if (intop .eq. 1) then
c        Use NCEP height field for upper boundary condition
	 do 31 j=1,ny
	 do 31 i=1,nx
	    z(i,j,1) = zn(i,j,1)
   31    continue
c
c        Calculate height field at AMSU levels and surface pressure
c        for special case
         call zalcals(t,ts,ps,p,z,lulog)
      else
c        Calculate height field at AMSU levels and surface pressure
         call zalcal(t,ts,ps,p,z,lulog)
      endif
c
c     Calculate surface density
      do 35 j=1,ny
      do 35 i=1,nx
	 rhos(i,j) = ps(i,j)/(rd*ts(i,j))
   35 continue
c
c     Extract AMSU t,z at NCEP levels for output
      call altonl(z,t,p,za,ta,pn,ts,ps,nx,ny,np,npn,lulog)
c
c     Make first guess for AMSU retrieved winds at NCEP pressure levels.
c     Use the lowest available NCEP pressure level for the surface winds.
      do 40 k=1,npn
      do 40 j=1,ny
      do 40 i=1,nx
	 ua(i,j,k) = un(i,j,k)
	 va(i,j,k) = vn(i,j,k)
   40 continue
c   
      do 45 j=1,ny
      do 45 i=1,nx
	 us(i,j) = un(i,j,npn)
	 vs(i,j) = vn(i,j,npn)
   45 continue
c
      if (iwinda .eq. 1 .or. (iwinda .gt. 1 .and. ifgnbe .eq. 1)) then
c        Calculate winds from heights using linear balance equation
         do k=1,npn
	    do j=1,ny
	    do i=1,nx
	       work1(i,j) = g*za(i,j,k)
	       work2(i,j) =   ua(i,j,k)
	       work3(i,j) =   va(i,j,k)
            enddo
	    enddo
c
            if (ibwin(k) .eq. 1) then
	       call lbe(work1,work2,work3,nx,ny)
	    endif
c
            if (iwinda .eq. 1) then
	       do j=1,ny
	       do i=1,nx
	          ua(i,j,k) = work2(i,j)
	          va(i,j,k) = work3(i,j)
               enddo
	       enddo
            else
	       do j=2,ny-1
	       do i=2,nx-1
	          ua(i,j,k) = work2(i,j)
	          va(i,j,k) = work3(i,j)
               enddo
	       enddo
	    endif
	 enddo
      endif
c
      if (iwinda .gt. 1) then
c
         do 46 k=1,npn
	    if (ibwin(k) .ne. 1) go to 46
c
	    do 47 j=1,ny
	    do 47 i=1,nx
	       work1(i,j) = g*za(i,j,k)
	       work2(i,j) =   ua(i,j,k)
	       work3(i,j) =   va(i,j,k)
   47       continue
c 
            if (ifgnbe .eq. 2) then
c              Calculate zero curvature first guess
	       do j=1,ny
	          worky(j) = 0.0
               enddo
c
	       do j=1,ny
	       do i=1,nx
	          work4(i,j) = 0.0
	          work5(i,j) = ua(i,j,k)
	          work6(i,j) = va(i,j,k)
               enddo
	       enddo
c
	       call zinter(work5,0.0,nx,ny)
	       call zinter(work6,0.0,nx,ny)
	       call psonxy(work4,worky,work2,work5,ierr)
	       call psonxy(work4,worky,work3,work6,ierr)
	    endif
c
            if (iwinda .eq. 2) then
               call nbei(work1,work2,work3,nx,ny)
	    elseif (iwinda .eq. 3) then
	       write(6,*) 'call nbev for p=',pn(k)
               inon=1
c              call nbei(work1,work2,work3,nx,ny)
               call nbev(work1,work2,work3,inon,nx,ny)
	    elseif (iwinda .eq. 4) then
	       write(6,*) 'call nbevs for p=',pn(k)
               inon=1
               call nbevs(work1,work2,work3,inon,nx,ny)
            endif
c
	    do 48 j=1,ny
	    do 48 i=1,nx
	       ua(i,j,k) = work2(i,j)
	       va(i,j,k) = work3(i,j)
   48       continue
   46   continue
      endif
c
      itest=0
      if (itest .eq. 1) then
	 write(lulog,879) 
  879    format(/,' u,v near domain center')
	 do j=23,19,-1
	    write(lulog,880) (ua(i,j,8),i=19,23),(va(i,j,8),i=19,23)
  880       format(1x,5(f7.1),2x,5(f7.1))
         enddo
      endif
c
c     Calculate spatial averages of ta, tn
      label = ' AMSU T, NCEP T'
      call spata(ta,tn,pn,nx,ny,npn,ibrem,lulog,label)
c
c     Calculate spatial averages of za, zn
      label = ' AMSU Z, NCEP Z'
      call spata(za,zn,pn,nx,ny,npn,ibrem,lulog,label)
c
c     Calculate spatial averages of ps,psn
      label = ' AMSU PS, NCEP PS'
      call spata1(ps,psn,nx,ny,ibrem,lulog,label)
c
c     Calculate spatial averages of tas
      label = ' AMSU swath T'
      call swata(tas,pamsu,mxas,mpas,nxas,lulog,label)
c
      if (ipapk .eq. 1) then
c        Write AMSU fields to packed file
         call wpof(ua,va,ta,za,ps,iyr2,imon,iday,itime,luapk)
         close(luapk)
      endif
c
      if (ipxya .eq. 1) then
c        Write fields (u,v,t,z,ps,clw) to ASCII file
c
c        Main header
c        write(luxya,176) atcfid,slat00,slon00,iyr,imon,iday,
c    +                     itime,sname,
c    +                     sslat,sslon,jyr,jmon,jday,jtimeh,jtimem
c 176    format(' Storm ID: ',a6,f6.2,1x,f7.2,
c    +          ' at ',i4,1x,2(i2.2),1x,i2.2,1x,a10,/,
c    +          ' Swath information:      ',6x,f6.2,1x,f7.2,
c    +          ' at ',i4,1x,2(i2.2),1x,i2.2,i2.2)
        write(luxya,176) coord
        write(luxya,176) times
  176   format(a90)
c
        do 77 i=1,npn
c          Write pressure level to file
           write(luxya,78) pn(i),nx,ny
   78      format('Pressure level (Pa)=',f8.1,1x,
     +            'nlat=',i2,1x,'nlon=',i2)
c
c          Write header line to file
           write(luxya,79)
   79      format(4x,'Lat',5x,'Lon',11x,'U',11x,'V',11x,'T',11x,'Z')
c
           do 177 j=1,ny
           do 177 k=1,nx
             write(luxya,178) rlatd(j),rlond(k),ua(k,j,i),va(k,j,i),
     +                          ta(k,j,i),za(k,j,i)
  178        format(f7.2,1x,f7.2,4(1x,f11.2))
  177    continue 
   77    continue
c   
c        Write information about surface variables
         write(luxya,80) ny,ny
   80    format('Surface pressure level',1x,'nlat=',i2,1x,'nlon=',i2)
c
c        Write header line for surface pressure level
         write(luxya,81)
   81    format(4x,'Lat',5x,'Lon',11x,'U',11x,'V',11x,'T',11x,'P',
     +          10x,'CLW')
c
         do 179 j=1,ny
         do 179 k=1,nx
           write(luxya,180) rlatd(j),rlond(k),us(k,j),vs(k,j),
     +                      ts(k,j),ps(k,j),clwxy(k,j)
  180      format(f7.2,1x,f7.2,5(1x,f11.2))
  179    continue
c     
         close(luxya)
      endif
c
c     Find location of minimum surface pressure in x,y analysis
      pminxy = 1.0e+10
      ibuf=10
      jbuf=10
      im = 0
      jm = 0
      do i=ibuf,nx-ibuf
      do j=jbuf,ny-jbuf
         if (ps(i,j) .lt. pminxy) then
            rlatpm = rlatd(j)
            rlonpm = rlond(i)
            im = i
            jm = j
            pminxy = ps(i,j)
         endif
      enddo
      enddo
      pminxy = pminxy/100.0
c
c     Refine the min pressure location 
      pmaxl = -1.0e+10
      do i=-1,1
      do j=-1,1
         ii = im + i
         jj = jm + j
         if (ps(ii,jj) .gt. pmaxl) pmaxl = ps(ii,jj)
      enddo
      enddo
c
      wtsum = 0.0
      rlatpma = 0.0
      rlonpma = 0.0
      do i=-1,1
      do j=-1,1
         ii = im + i 
         jj = jm + j
         wt = (pmaxl-ps(ii,jj))
         wtsum = wtsum + wt
         rlatpma = rlatpma + wt*rlatd(jj)
         rlonpma = rlonpma + wt*rlond(ii)
      enddo
      enddo
c
      if (wtsum .gt. 0.0) then
         rlatpma = rlatpma/wtsum
         rlonpma = rlonpma/wtsum
      else
         rlatpma = 0.0
         rlonpma = 0.0
      endif
c
c     **** End hydrostatic integration and wind retrieval on 3-D grid ****
c
      if (iaxsym .ne. 1) go to 5000
c
c     **** Begin axisymmetric retrievals ****
c
c     sslat = rlatpma
c     sslon = rlonpma
c
c     Interpolate AMSU swath temperatures to radial grid
      if (iradxy .eq. 1) then
         kk=0
         do 57 j=1,ny
         do 57 i=1,nx
            kk = kk + 1
            temlat(kk) = rlatd(j)
            temlon(kk) = rlond(i)
   57    continue
c
         do 58 k=1,np
            kk=0
            do 59 j=1,ny
            do 59 i=1,nx
               kk = kk + 1
               ftem(kk) = t(i,j,k)
   59       continue
            call barr(ftem,temlat,temlon,kk,efldr,exfac,rinfr,
     +                sslat,sslon,trp(1,k),rr,nr,ierr)
   58    continue
c
      else
         do 50 k=1,np
            do 51 i=1,nxas
               dumas(i) = tas(i,npst+k-1)
   51       continue
c
            call barr(dumas,aslat,aslon,nxas,efldr,exfac,rinfr,
     +                sslat,sslon,trp(1,k),rr,nr,ierr)
c
            if (ierr .ne. 0) then
               write(lulog,950)
  950          format(/,'Error in barr')
               stop
            endif
   50    continue
      endif
c
c     Interpolate surface temperature and pressure to radial grid 
c     for first guess
      kk=0
      do 55 j=1,ny
      do 55 i=1,nx
	 kk = kk + 1
	 ftem(kk)   = tsn(i,j)
	 temlat(kk) = rlatd(j)
	 temlon(kk) = rlond(i)
   55 continue
c
      call barr(ftem,temlat,temlon,kk,efldr,exfac,rinfr,
     +          sslat,sslon,tsr,rr,nr,ierr)
c
      if (ierr .ne. 0) then
         write(lulog,950)
         stop
      endif
c
      kk=0
      do 56 j=1,ny
      do 56 i=1,nx
	 kk = kk + 1
	 ftem(kk)   = psn(i,j)
	 temlat(kk) = rlatd(j)
	 temlon(kk) = rlond(i)
   56 continue
c
      call barr(ftem,temlat,temlon,kk,efldr,exfac,rinfr,
     +          sslat,sslon,psr,rr,nr,ierr)
c
      if (ierr .ne. 0) then
         write(lulog,950)
         stop
      endif
c
c     Calculate gradient wind
      call bwgcal(rr,p,trp,psr,tsr,dz,nr,np,nz,sslat,
     +            zrp,vrp,zz,trz,prz,rhorz,vrz)
c
c     Print gradient winds to log file
      write(lulog,284)
  284 format(/,'Tangential wind profiles')
c
      do 90 k=1,nr
	 write(lulog,285) rr(k)/1000.,prz(k,1)/100.,
     +                    zz( 1)/1000.,vrz(k, 1),
     +                    zz( 6)/1000.,vrz(k, 6),
     +                    zz(11)/1000.,vrz(k,11)
  285    format(' r=',f5.0,1x,' ps=',f6.1,3('  z=',f5.1,' v=',f5.1))
   90 continue
c
c     Calculate temperature anomalies
      do k=1,nz
      do i=1,nr
	 tarz(i,k) = trz(i,k)-trz(nr,k)
      enddo
      enddo
c
      if (iprza .eq. 1 .and. iaxsym .eq. 1) then
c        Write all r,z variables to a file 
         labelr      = '            '
         write(labelr,701) atcfid,imon,iday,itime,
     +                     jmon,jday,jtimeh,jtimem
  701    format(a6,1x,3(i2.2),1x,4(i2.2))
         call tcrwrit(prz,rhorz,trz,vrz,nr,nz,sslat,sslon,lurza,
     +                labelr,coord,times,rr(1),rr(nr),zz(1),zz(nz)) 
      endif
c
      if (ipsta .eq. 1 .and. iaxsym .eq. 1) then
c        Write basic statistics of the analysis to a file
         write(lusta,700) atcfid,slat00,slon00,iyr,imon,iday,
     +                     itime,sname,
     +                     sslat,sslon,jyr,jmon,jday,jtimeh,jtimem
  700    format(' Analysis statistics for ',a6,f6.2,1x,f7.2,
     +          ' at ',i4,1x,2(i2.2),1x,i2.2,1x,a10,/,
     +          ' Swath information:      ',6x,f6.2,1x,f7.2,
     +          ' at ',i4,1x,2(i2.2),1x,i2.2,i2.2)
c
c        Evaluate parameters from analyses
c
c        Find minimum pressure and pressure drop
         pmin =  prz(1,1)/100.0
	 delp =  (prz(nr,1)-prz(1,1))/100.0
c        
c        Find pressure drop at 3 km
	 delp3 = (prz(nr,4)-prz(1,4))/100.0
c
c        Find max temperature anomalies at 3 km or above
         tmax = -999.0
	 ztmax= -99.0
	 do k=4,nz
	    if (tarz(1,k) .gt. tmax) then
	       tmax = tarz(1,k)
	       ztmax= zz(k)/1000.0
            endif
         enddo
c
c        Find max winds (first local maximum starting at r=0)
         thlm = 5.0
         vmx0 = -99.
	 vmx3 = -99.
	 rmx0 = -99.
	 rmx3 = -99.
c
	 isz0 = 1
	 isz3 = 1
	 do i=1,nr-1
	    dvmax0 = vmx0-vrz(i,1)
	    if (isz0 .eq. 1) then
	        if (dvmax0 .gt. thlm) isz0=0
            endif
c
	    if (vrz(i,1) .gt. vmx0 .and. isz0 .eq. 1) then
	       vmx0 = vrz(i,1)
	       rmx0 = rr(i)/1000.0
            endif
c
	    dvmax3 = vmx0-vrz(i,4)
	    if (isz3 .eq. 1) then
	        if (dvmax3 .gt. thlm) isz3=0
            endif
c
	    if (vrz(i,4) .gt. vmx3 .and. isz3 .eq. 1) then
	       vmx3 = vrz(i,4)
	       rmx3 = rr(i)/1000.0
            endif
         enddo
	 vmx0 = 1.944*vmx0
	 vmx3 = 1.944*vmx3
c
c        Find wind radii
	 v15 = 15.0/1.944
	 v25 = 25.0/1.944
	 v35 = 35.0/1.944
c
         r035 = -99.0
         r025 = -99.0
         r015 = -99.0
c
         r335 = -99.0
         r325 = -99.0
         r315 = -99.0
c
         if (vmx0 .gt. 35.0) then
	    rmx = 1000.0*rmx0
	    do i=1,nr
	       if (vrz(i,1) .lt. v35 .and. rr(i) .gt. rmx) then
		  r035 = rr(i)/1000.0
		  go to 1001
               endif
            enddo
 1001    endif
c
         if (vmx0 .gt. 25.0) then
	    rmx = 1000.0*rmx0
	    do i=1,nr
	       if (vrz(i,1) .lt. v25 .and. rr(i) .gt. rmx) then
		  r025 = rr(i)/1000.0
		  go to 1002
               endif
            enddo
 1002    endif
c
         if (vmx0 .gt. 15.0) then
	    rmx = 1000.0*rmx0
	    do i=1,nr
	       if (vrz(i,1) .lt. v15 .and. rr(i) .gt. rmx) then
		  r015 = rr(i)/1000.0
		  go to 1003
               endif
            enddo
            r015 = rr(nr)/1000.0
 1003    endif
c
         if (vmx3 .gt. 35.0) then
	    rmx = 1000.0*rmx3
	    do i=1,nr
	       if (vrz(i,4) .lt. v35 .and. rr(i) .gt. rmx) then
		  r335 = rr(i)/1000.0
		  go to 4001
               endif
            enddo
 4001    endif
c
         if (vmx3 .gt. 25.0) then
	    rmx = 1000.0*rmx0
	    do i=1,nr
	       if (vrz(i,4) .lt. v25 .and. rr(i) .gt. rmx) then
		  r325 = rr(i)/1000.0
		  go to 4002
               endif
            enddo
 4002    endif
c
         if (vmx3 .gt. 15.0) then
	    rmx = 1000.0*rmx0
	    do i=1,nr
	       if (vrz(i,4) .lt. v15 .and. rr(i) .gt. rmx) then
		  r315 = rr(i)/1000.0
		  go to 4003
               endif
            enddo
            r315 = rr(nr)/1000.0
 4003    endif
c
c        Find mean tangential wind at 0,3 and 5 km 
c        for r=0 to 250 km and r=250 to 500 km
         rthi = 250.0e+3
	 rtho = 500.0e+3
c
         vbi0 = 0.0
	 vbi3 = 0.0
	 vbi5 = 0.0
	 vbo0 = 0.0
	 vbo3 = 0.0
	 vbo5 = 0.0
c
         cbi = 0.0
	 cbo = 0.0
c
	 do i=1,nr
	    if (rr(i) .le. rthi) then
	       cbi  = cbi  + 1.0
	       vbi0 = vbi0 + vrz(i,1)
	       vbi3 = vbi3 + vrz(i,4)
	       vbi5 = vbi5 + vrz(i,6)
	    endif
c
	    if (rr(i) .gt. rthi .and. rr(i) .le. rtho) then
	       cbo  = cbo  + 1.0
	       vbo0 = vbo0 + vrz(i,1)
	       vbo3 = vbo3 + vrz(i,4)
	       vbo5 = vbo5 + vrz(i,6)
	    endif
         enddo
c
	 vbi0 = vbi0/cbi
	 vbi3 = vbi3/cbi
	 vbi5 = vbi5/cbi
c
	 vbo0 = vbo0/cbo
	 vbo3 = vbo3/cbo
	 vbo5 = vbo5/cbo
c
c        Average cloud liquid water near storm center
c        and find fractional area within radat km covered
c        by cthresh mm of CLW
	 clwavg = 0.0
	 cnt    = 0.0
	 radmin = 100.0
c
	 radat = 300.0
	 cthresh = 0.5
	 pcount  = 0.0
	 pthresh = 0.0
c
	 do j=1,ny
	 do i=1,nx
	    call distk(sslon,sslat,rlond(i),rlatd(j),dx,dy,rad)
c
	    if (rad .lt. radmin .and. clwxy(i,j) .ge. 0.0) then
	      clwavg = clwavg + clwxy(i,j)
	      cnt    = cnt + 1.0
            endif
c
	    if (rad .lt. radat) then
	       pcount = pcount + 1.0
	       if (clwxy(i,j) .gt. cthresh) then
		  pthresh = pthresh + 1.0
               endif
            endif
         enddo
	 enddo
c
         if (cnt .gt. 0.0) then
	    clwavg = clwavg/cnt
         else
	    clwavg = 0.0
         endif
c
         if (pcount .gt. 0.0) then
            pcrat = 100.0*(pthresh/pcount)
         else
	    pcrat = 0.0
         endif
c
         write(lusta,710) pmin,delp,delp3,tmax,ztmax,ares,
     +                     vmx0,rmx0,vmx3,rmx3
  710    format(' Min. Pressure=  ',f6.1,/,
     +          ' Sfc. P drop  =  ',f6.1,/,
     +          ' 3 km P drop  =  ',f6.1,/,
     +          ' Max T anomaly=  ',f6.1,/,
     +          ' z(max T)     =  ',f6.1,/,
     +          ' Swath spacing=  ',f6.1,/,
     +          ' Vmx(z=0)     =  ',f6.1,/,
     +          ' r(vmx0)      =  ',f6.1,/,
     +          ' Vmx(z=3)     =  ',f6.1,/,
     +          ' r(vmx3)      =  ',f6.1)
c
	 write(lusta,720) vbi0,vbi3,vbi5,vbo0,vbo3,vbo5,clwavg,
     +                     pcrat
  720    format(' vbi0         =  ',f6.1,/,
     +          ' vbi3         =  ',f6.1,/,
     +          ' vbi5         =  ',f6.1,/,
     +          ' vbo0         =  ',f6.1,/,
     +          ' vbo3         =  ',f6.1,/,
     +          ' vbo5         =  ',f6.1,/,
     +          ' Average CLW  =  ',f6.2,/,
     +          ' CLW percent  =  ',f6.2)
c
	 write(lusta,730) pminxy,rlatpm,rlonpm,rlatpma,rlonpma
  730    format(' pminxy       =  ',f6.1,/,
     +          ' lat of pminxy=  ',f6.1,/,
     +          ' lon of pminxy=  ',f6.1,/,
     +          ' adj lat      = ', f7.2,/,
     +          ' adj lon      = ', f7.2)
c
	 if (ipsta .eq. 1) then
c           Estimate max wind and azimuthal mean wind radii 
c           using statistical relationships
c           Fill predictor array for new form of vradp routine
            preds( 1) = pmin
            preds( 2) = delp
            preds( 3) = delp3
            preds( 4) = tmax
            preds( 5) = ztmax
            preds( 6) = ares
            preds( 7) = vmx0
            preds( 8) = rmx0
            preds( 9) = vmx3
            preds(10) = rmx3
            preds(11) = vbi0
            preds(12) = vbi3
            preds(13) = vbi5
            preds(14) = vbo0
            preds(15) = vbo3
            preds(16) = vbo5
            preds(17) = clwavg
            preds(18) = pcrat
c
            spd  = float(ispeed)
	    head = float(idir)
	    vmaxop = float(ivmax)
	    ias    = 0
c
	    call vradp(preds,sslat,sslon,spd,head,vmaxop,ias,
     +                 pvmx,ppmin,rmp,xp,pr34,pr50,pr64)
c
	    write(lusta,735) pvmx,ppmin,
     +                        pr34(5),pr50(5),pr64(5),
     +                        pr34(6),pr50(6),pr64(6),
     +                        vmaxop,spd,head,
     +                        rmp,xp,(pr34(kk),kk=1,4),
     +                               (pr50(kk),kk=1,4),
     +                               (pr64(kk),kk=1,4)
  735       format(/,' Statistical Intensity/Radii Estimates',
     +        /,1x,f5.0,1x,f5.0,7x,' AMSU Max wind (kt), Min P (hPa)  ',
     +        /,1x,3(f5.0,1x),     ' Mean 34,50,64 kt wind radius (nm)',
     +        /,1x,3(f5.0,1x),     ' Fit  34,50,64 kt wind radius (nm)',
     +        /,1x,3(f5.0,1x),     ' ATCF max wind, speed, heading',
     +        /,1x,f5.0,1x,f5.2,7x,' rm (nm) and x from vortex fit',
     +        /,1x,4(f5.0,1x),' 34 kt radii NE,SE,SW,NW',
     +        /,1x,4(f5.0,1x),' 50 kt radii NE,SE,SW,NW',
     +        /,1x,4(f5.0,1x),' 64 kt radii NE,SE,SW,NW')
         endif
      endif
c
 5000 continue
c 
      if (ipfix .eq. 1 .and. iaxsym .eq. 1) then
c        Write information to NHC fix file
         ippmin = ifix(ppmin+0.5)
	 ipvmax = ifix(pvmx +0.5)
c
         if (ifixall .eq. 1 .and. inhce .ne. 0) then
c           No analysis was performed, and fix file name not created,
c           so create it now
            fnfix        = 'FX0000_000000.DAT'
            fnfix( 2: 2) = atcfid(1:1)
            fnfix( 3: 6) = atcfid(3:6)
            fnfix(18:19) = cnsat
            write(fnfix(8:13),226) imon,iday,itime
c
            if (igenfn .eq. 1) then
	       fnfix='  '
	       fnfix(1:6) = noaass
	       fnfix(7:10) = '.FIX'
            endif
         endif
c
         fntemp=fnfix
         open(file=fnfix,unit=lufix,form='formatted',status='unknown',
     +        err=900)
c     See DATELINE ISSUE. - Knaff 2/20/2006
         if(sslon  .gt. 180.0) sslon  = sslon  - 360.0
         if(slon00 .gt. 180.0) slon00 = slon00 - 360.0
         if(slon12 .gt. 180.0) slon12 = slon12 - 360.0

	 call fixprt(lufix,threshr,radsts,inhce,ippmin,ipvmax,cnsat)
c
         if (inhce .eq. 0) then
c           Open and write fix file in ATCF format
c
            fntemp=fnafx
            open(file=fnafx,unit=luafx,form='formatted',
     +           status='unknown',err=900)
c
            read(atcfid(3:4),295) istnum
  295       format(i2)
            irmp = ifix(0.5 +  rmp)
            isdist = ifix(rad)
            cstype(1:4)='NOAA'
            cstype(5:6)=cnsat
c
            do i=1,4
               ir34(i) = ifix(0.5 + pr34(i))
               ir50(i) = ifix(0.5 + pr50(i))
               ir64(i) = ifix(0.5 + pr64(i))
            enddo
c
            call AMSUfdeck(cbasin,istnum,cstype,
     +                     jyr,jmon,jday,jtimeh,jtimem,sslat,sslon,
     +                     ippmin,ipvmax,ir34,ir50,ir64,
     +                     irmp,isdist,luafx)
c
         endif
      endif
c
      write(lulog,290)
  290 format(/,' Program oparet completed normally.')
c
      stop
c
c     Error processing
  900 continue
      write(lulog,990) fntemp
  990 format(/,' Error opening file ',a25)
c
      stop
      end
      subroutine fixprt(lufix,thresh,rad,ierr,ippmin,ipvmax,cnsat)
     +                  
c     This routine writes the fix file
c
      dimension pr34(6),pr50(6),pr64(6)
      character *2 cnsat
      character *3 cdow
      character *6 atcfid
      character *9 sname
c
      dimension ipr34(6),ipr50(6),ipr64(6)
c
      common /sinfo1/ atcfid,sname
      common /sinfo2/ iyr,imon,iday,itime4,
     +                jyr,jmon,jday,jtime4,
     +                ivmax,spd,head,
     +                slat00,slat12,sslat,
     +                slon00,slon12,sslon,
     +                rmp,xp,pr34,pr50,pr64
c
c     Convert output variables to integers 
      do k=1,6
	 ipr34(k) = ifix(0.5 + pr34(k))
	 ipr50(k) = ifix(0.5 + pr50(k))
	 ipr64(k) = ifix(0.5 + pr64(k))
      enddo
      ispd = ifix(0.5 +  spd)
      ihead= ifix(0.5 + head)
      irmp = ifix(0.5 +  rmp)
      irad = ifix(0.5 +  rad)
c
c     Get current date and time
      call cdt(kyr,kmon,kday,ktime4,cdow)
c
c     Calculate time difference between AMSU and ATCF date/times
      itime2 = itime4/100
      jtime2 = jtime4/100
      call tdiff(jyr,jmon,jday,jtime2,iyr,imon,iday,itime2,idtsa)
c
      if (ierr .eq. 0) then
         write(lufix,200) cnsat
  200    format(' ***************************************',
     +          '*******************************',
     +          /,' CIRA/NESDIS Experimental AMSU-A TC',
     +            ' Intensity/Size Estimation - NOAA',a2,/)
c
	 write(lufix,210) atcfid(1:4),iyr,sname
  210    format(' Tropical Cyclone ',1x,a4,i4.4,2x,a9)
c
	 write(lufix,220) kyr,kmon,kday,ktime4, 
     +                    iyr,imon,iday,itime4,
     +                    jyr,jmon,jday,jtime4
  220    format(' Current    date/time: ',i4,1x,2(i2.2),1x,i4.4,' UTC',
     +        /,' ATCF file  date/time: ',i4,1x,2(i2.2),1x,i4.4,' UTC',
     +      /,/,' AMSU swath date/time: ',i4,1x,2(i2.2),1x,i4.4,' UTC')
c
         write(lufix,300) ippmin,ipvmax
  300    format(/,' Minimum Sea-Level Pressure: ',i5,' hPa',
     +          /,' Maximum Surface Winds:      ',i5,' kt',/)
c
	 write(lufix,310) (ipr34(k),k=1,4) 
  310    format(' 34 kt wind radii (NE,SE,SW,NW): ',4(i4),'  nmi')
	 write(lufix,312) (ipr50(k),k=1,4) 
  312    format(' 50 kt wind radii (NE,SE,SW,NW): ',4(i4),'  nmi')
	 write(lufix,314) (ipr64(k),k=1,4) 
  314    format(' 64 kt wind radii (NE,SE,SW,NW): ',4(i4),'  nmi')
c
         write(lufix,320) irmp
  320    format(/,' AMSU-retrieved max wind radius: ',i4,' nmi')
c
         write(lufix,330) irad
  330    format(/,' Storm center is ',i4,' km from AMSU swath center',
     +          /,'                0-300 km is optimal ', 
     +          /,'              300-600 km is adequate', 
     +          /,'                 >600 km is marginal')
c
         write(lufix,340) idtsa
  340    format(/,' AMSU data is ',i4,' hr from time of ATCF input',/)
c
         write(lufix,201)
  201    format(' ***************************************',
     +          '*******************************')
c
         write(lufix,400) atcfid(1:4),iyr,imon,iday,itime4 
  400    format(' ATCF File Input: ',
     +         /,1x,a4,i4.4,1x,2(i2.2),1x,i4.4,' UTC')
c
	 write(lufix,410) slat00,slon00,slat12,slon12,
     +                    idtsa,sslat,sslon
  410    format(/,' Storm lat,lon (t =   0 hr): ',f6.2,1x,f7.2,
     +          /,' Storm lat,lon (t = -12 hr): ',f6.2,1x,f7.2,
     +          /,' Storm lat,lon (t =',i4,' hr): ',f6.2,1x,f7.2,
     +            ' (AMSU swath time)')
c
         write(lufix,420) ivmax,ihead,ispd
  420    format(/,' Storm max winds (ATCF):  ',i3  ,' kt',
     +          /,' Storm heading:           ',i3.3,' deg',
     +          /,' Storm translation speed: ',i3  ,' kt',
     +   //,' Note: AMSU wind radii provided for all wind thresholds',
     +    /,'       up to the ATCF max winds. Thus, AMSU wind radii ',
     +    /,'       may be provided for thresholds that exceed the  ',
     +    /,'       AMSU max wind estimate.         ')
c
         write(lufix,201)
         return
      elseif (ierr .eq. 1) then
         write(lufix,200) cnsat
	 write(lufix,210) atcfid(1:4),iyr,sname
	 write(lufix,220) kyr,kmon,kday,ktime4,
     +                    iyr,imon,iday,itime4,
     +                    jyr,jmon,jday,jtime4
c
	 write(lufix,901) rad,thresh
  901    format(/,' INTENSITY/SIZE ESTIMATION FAILED',
     +          /,' Storm too far from center of AMSU data swath',
     +          /,' Observed Distance:      ',f5.0,' km',
     +          /,' Max allowable distance: ',f5.0,' km')
c
         write(lufix,201)
	 stop
      elseif (ierr .eq. 2) then
         write(lufix,200) cnsat
	 write(lufix,210) atcfid(1:4),iyr,sname
	 write(lufix,220) kyr,kmon,kday,ktime4,
     +                    iyr,imon,iday,itime4,
     +                    jyr,jmon,jday,jtime4
c
	 write(lufix,902) 
  902    format(/,' INTENSITY/SIZE ESTIMATION FAILED',
     +          /,' Error Processing AMSU data',
     +          /,' (Usually caused by data that is not available yet)') 
c
         write(lufix,201)
	 stop
      endif
c
      return
      end
      subroutine cdt(kyr,kmon,kday,ktime,cdow)
c     This routine gets the current dat and time (UTC)
c     from the file tdate.dat that is created by the mdasmu.ksh 
c     script using the HP-UX command date -u. The output of this
c     command is of the form
c
c     Fri May 17 16:00:42 GMT 2002
c
      dimension cmonl(12)
c
      character *3 cdow,cmon,cmonl
c
      kyr = 0
      kmon= 0
      kday= 0 
      ktime=0
      cdow='XXX'
c
      cmonl( 1) = 'Jan'
      cmonl( 2) = 'Feb'
      cmonl( 3) = 'Mar'
      cmonl( 4) = 'Apr'
      cmonl( 5) = 'May'
      cmonl( 6) = 'Jun'
      cmonl( 7) = 'Jul'
      cmonl( 8) = 'Aug'
      cmonl( 9) = 'Sep'
      cmonl(10) = 'Oct'
      cmonl(11) = 'Nov'
      cmonl(12) = 'Dec'
c
      open(unit=65,file='tdate.dat',form='formatted',status='old',
     +     err=900)
c
      read(65,100,err=900) cdow,cmon,kday,khr,kmin,kyr
  100 format(a3,1x,a3,1x,i2,1x,i2,1x,i2,8x,i4)
      close(65)
c
      do k=1,12     
	 if (cmon .eq. cmonl(k)) kmon = k
      enddo
      if (kmon .eq. 0) go to 900
c
      ktime = 100*khr + kmin
c
      return
c
  900 continue
c
      kyr = 0
      kmon= 0
      kday= 0 
      ktime=0
      cdow='XXX'
c
      return
      end
