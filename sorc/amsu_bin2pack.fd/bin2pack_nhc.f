      program bin2pack_nhc
c     Modified from aapack.f by Jack Dostalek      
c     This program makes a packed ASCII file from unformatted
c     files generated by program bintrans_nhc.exe.
c
c     This version is for data from the GFS model. The date information
c     information is contained in the file gfs_indate
c
c     The standard atmosphere heights can be subtracted if necessary.
c
c     Set izsub=1 to subtract standard atmosphere heights
      data izsub /1/
c
c     5/24/99 Modifications for Y2K compliance
c
c     Specify the number of variables to look for.
      parameter (nvar=51)
c
c     Specify variable selection numbers and corresponding pressure levels
c          Choices: 1  = u
c                   2  = v
c                   3  = z
c                   4  = temperature
c                   5  = relative humidity

c ** START Als Code obtain environmental variables set by perl code

c getenv is not considered an Intrinsic function by pgf90
c      INTRINSIC getenv
	  
c ** END Als Code
	  
      dimension ivar(nvar)
      dimension iplev(nvar)
c
      data ivar /1,1,1,1,1,1,1,1,1,1,1,
     +           2,2,2,2,2,2,2,2,2,2,2,
     +           3,3,3,3,3,3,3,3,3,3,3,
     +           4,4,4,4,4,4,4,4,4,4,4,
     +           5,5,5,5,5,5,5/
c
      data iplev /1000,925,850,700,500,400,300,250,200,150,100,
     +            1000,925,850,700,500,400,300,250,200,150,100,
     +            1000,925,850,700,500,400,300,250,200,150,100,
     +            1000,925,850,700,500,400,300,250,200,150,100,
     +            1000,925,850,700,500,400,300/
c
c     Initialize arrays for variable names
      parameter (nvmax=5)
      character *1 cvarrd(nvmax)
      character *8 cvarin(nvmax)
c    ****Start Jack****      
      character *12 clog,cpack
c      character *28 cdatdir
c    *****End Jack*****      
c
c     Data for input variable names
      data cvarin /'u       ','v       ','height  ','temp    ',
     +             'relh    '/
c
c     Data for output variable names
      data cvarrd /'U','V','Z','T','R'/
c
c     Temporary arrays and characters
      parameter (mxsize=100000)
      parameter (mxlon=360,mxlat=181)
c
      real dtem(mxsize)
      real dtemll(mxlon,mxlat),doutll(mxlon,mxlat)	  
c
      character *2 ctime
      character *3 cfhour
	  character *6 cdate
      character *4 ctemp4
      character *8 ctemp8	  
c
      character *1 pds(100)
      integer      kgds(200)
c
c     Character variables for packing routines
      character *1 type
      character *2 code(mxsize)
c
c     I/O variables
      character *50 ifile,dfile,pfile
      data luin,ludata,lupack,lulog /30,31,32,11/
c
c      data ifile /'gfs_indate'/
c
      data pfile /'G00095_X0000_PACK.DAT'/
c
c     Specify grid information for the output file
c     (longitude convention: -180 to 180)
c     (latitude convention:   -90 to  90)
      dlat = 2.0
      dlon = 2.0
      slat = -90.0
      slon = 0.0
      nlat = 91
      nlon = 181
      elat = slat + dlat*float(nlat-1)
      elon = slon + dlon*float(nlon-1)
	  
c ** START Als Code obtain environmental variables set by perl code	  
      
	  call getenv("YMD", cdate)
      call getenv("RUN", ctime)
      call getenv("FHOUR", cfhour)
	  
c ** END Als Code	  


c    ****Jack's Code****
c      cdatdir='/home/dostalek/jack/gfs/bin/'
      clog='pack_nhc.log'
c      open(unit=lulog,file=cdatdir//clog,status='replace')
	   open(unit=lulog,file=clog,status='replace')
c    *****End Jack*****           
c
c     Open the input file with the date information
c      open(unit=luin,file=cdatdir//ifile,status='old',form='formatted',
c     +     err=900)
c     
c
c     Read the date and forecast hour information
c      read(luin,100,err=905) cdate
c  100 format(a6)
c      read(luin,102,err=905) ctime
c  102 format(a2)
c      read(luin,104,err=905) cfhour
c  104 format(a3)
c
      write(lulog,800) cdate,ctime,cfhour
  800 format(' Starting bin2pack: date, time, forecast hr: ',
     +         a6,1x,a2,1x,a3)
c
c     Create and open the packed ASCII output file name
      read(cdate,110) iyr,imon,iday
  110 format(3(i2))
      read(ctime,112) ihr
  112 format(i2)
      call clen(cfhour,3,len_loc)
      if (len_loc .eq. 3) then
         read(cfhour,113) ifhour
  113    format(i3)
      else
         read(cfhour,114) ifhour
  114    format(i2)
      endif
c
      write(pfile(2:4),200) ifhour
  200 format(i3.3)
      write(pfile(5:6),202) iyr
  202 format(i2.2)
      write(pfile(9:10),202) imon
c
      idayt = iday
      if (ihr .ge. 12) then
         idayt = idayt + 50
      endif
      write(pfile(11:12),202) idayt
c
      if (ihr .eq. 6 .or. ihr .eq. 18) then
         pfile(8:8) = 'Y'
      endif

c  *****Jack's Code*****      
c     Write out name of PACK.DAT file to file packfile for use in script
c     getgfs.ksh_nhc   
      lupack1=66
      cpack='packfile_nhc'
c      open(unit=lupack1,file=cdatdir//cpack,status='replace')
	  open(unit=lupack1,file=cpack,status='replace')
      write(66,67) pfile
67    format(a21)               
c

c  ******End Jack******
      open(unit=lupack,file=pfile,form='formatted',
     + status='replace',err=910)
c      open(unit=lupack,file=cdatdir//pfile,form='formatted',
c     + status='replace',err=910)
c	  open(unit=lupack,file=pfile,form='formatted', 
c	 + status='replace',err=910)
c
      write(lulog,810) pfile
  810 format(/,' Packed ASCII file opened: ',a50)
c
c     Write the header on the packed ASCII file
      wx=12.0
      dayx = float(10000*iyr + 100*imon + iday)
      utcx = float(100*ihr)
      rlatmn = slat
      rlatmx = elat
      rlonmn = -elon
      rlonmx = -slon
      fhour = float(ifhour)
c
c     write(lupack,210) wx,dayx,utcx,rlatmn,rlatmx,rlonmn,rlonmx,
c    +                  dlat,dlon,fhour
c 210 format(1x,f3.0,f7.0,f5.0,4f8.3,2f4.1,f6.0)
c
c     Modification for Y2K compliance
      write(lupack,210) wx,iyr,imon,iday,utcx,rlatmn,rlatmx,
     +                  rlonmn,rlonmx,dlat,dlon,fhour
  210 format(1x,f3.0,3(i2.2),'.',f5.0,4f8.3,2f4.1,f6.0)
  
c   ******Jack's Code*****
c   Write header to log file as well
c   
      write(lulog,210) wx,iyr,imon,iday,utcx,rlatmn,rlatmx,
     +                  rlonmn,rlonmx,dlat,dlon,fhour
     
c   ****End Jack****     
  
  
  
c
c     Start the variable loop
      nvart=nvar
      do 99 n=1,nvart
c        Create data file name
         dfile= ' '
         dfile(1:6) = cdate
         dfile(7:8) = ctime
         dfile(9:10) = '_F'
c
         call clen(cfhour,3,len_loc)
         m1 = 11
         m2 = m1 + len_loc-2
         dfile(m1:m2) = cfhour(2:len_loc)
c
         m1 = m2+1
         m2 = m1
         dfile(m1:m2) = '_'
c
         in = ivar(n)
         ctemp8 = cvarin(in)
         call clen(ctemp8,8,len_loc)
         m1 = m2+1
         m2 = m1 + len_loc-1
         dfile(m1:m2) = ctemp8(1:len_loc)
c
c        write(ctemp8,*) iplev(n)
c        ctemp4 = ctemp8(2:5)
         ipt = iplev(n)
         if     (ipt .ge. 1000) then
            write(ctemp4,602) ipt
  602       format(i4)
         elseif (ipt .lt. 1000 .and. ipt .ge. 100) then
            write(ctemp4,604) ipt
  604       format(i3)
         else
            write(ctemp4,606) ipt
  606       format(i2)
         endif
c
         call clen(ctemp4,4,len_loc)
         m1 = m2+1
         m2 = m1 + len_loc-1
         dfile(m1:m2) = ctemp4(1:len_loc)
c
         m1 = m2+1
         m2 = m1+4
         dfile(m1:m2) = '.gtmp'
c
         write(lulog,215) dfile
  215    format(' Data file name: ',a50)
c
c        Open the current data file
c         open(unit=ludata,file=cdatdir//dfile,status='old',
c     +   access='sequential',form='unformatted',err=915)
		 open(unit=ludata,file=dfile,status='old',
     +   access='sequential',form='unformatted',err=915)
c
c        Read the first record of the grib output file
         read(ludata,end=920) lenpds,lenkgds,nwords
         print *,'lenpds,lenkgds,nwords=',
     +            lenpds,lenkgds,nwords
c
c        Read the pds section of the grib output file
         read(ludata,end=920) (pds(j),j=1,lenpds)
c
         if (lenkgds .gt. 0) then
            read(ludata,end=920) (kgds(j),j=1,lenkgds)
            print *,'kgds = '
            print *,(kgds(j),j=1,lenkgds)
         endif
c
c        Read the data section of the grib output file
         read(ludata,end=920) (dtem(j),j=1,nwords)
c
c        do 10 j=1,nwords
c           if (mod(j,1000) .eq. 0) then
c              write(6,*) dtem(j)
c           endif
c  10    continue
c
         if (n .eq. 1) then
            nlong = kgds(2)
            nlatg = kgds(3)
            idlong = kgds( 9)
            idlatg = kgds(10)
            dlong = float(idlong)/1000.0
            dlatg = float(idlatg)/1000.0
c
            rlatbg = -90.0
            rlattg =  90.0
            rlonlg = 0.0
            rlonrg = 360.0-dlong
         endif
c
c        Check to make sure grid from grib output file does not change
         nlonck = kgds(2)
         nlatck = kgds(3)
         idlonck = kgds( 9)
         idlatck = kgds(10)
c
         if ( (nlong .ne. nlonck) .or. (idlong .ne. idlonck) .or.
     +        (nlatg .ne. nlatck) .or. (idlatg .ne. idlatck) ) then
            go to 925
         endif
c
c        Put the data in a 2-D lon/lat array.
c        This version assumes a global grid from 90.0 to -90.0 in lat,
c        0.0 to 360.0 in lon read in from the grib output file.
c       
         icount = 0
         do 15 j=nlatg,1,-1
         do 15 i=1,nlong
            icount = icount+1
            dtemll(i,j) = dtem(icount)
   15    continue
c
c        Interpolate field to desired grid for packing 
         if (slon .lt. 0) slon = slon + 360.0
         izp = 1
c
         rlong = 0.0
         rlatg = -90.0
         call llintp(dtemll,rlong,rlatg,dlong,dlatg,
     +                      mxlon,mxlat,nlong,nlatg,
     +               doutll, slon, slat, dlon, dlat,
     +                      mxlon,mxlat, nlon, nlat,
     +               izp,ierr)
c
c        write(6,888) rlong,rlatg,dlong,dlatg,
c    +                mxlon,mxlat,nlong,nlatg,
c    +                slon, slat, dlon, dlat,
c    +                mxlon,mxlat, nlon, nlat
c 888    format(/,'rlong,rlatg,dlong,dlatg: ',4f8.1,
c    +          /,'mxlon,mxlat,nlong,nlatg: ',4f8.1,
c    +          /,' slon, slat, dlon, dlat: ',4f8.1,
c    +          /,'mxlon,mxlat, nlon, nlat: ',4f8.1)
c
c        Put current variable in a one-dimensional array for packing
c        and subtract standard atmosphere variables if necessary
c
         if (ivar(n) .ne. 3) then
            vbar = 0.0
         else
            if (izsub .eq. 1) then
               pltemp = float(iplev(n))
               call stndz(pltemp,zbar,tbar,theta)
               vbar = zbar
            else
               vbar = 0.0
            endif
         endif
c
         icount = 0
         do 20 j=1,nlat
         do 20 i=1,nlon
            icount = icount + 1
            dtem(icount) = (doutll(i,j)-vbar)
   20    continue
c
c        Find max and min of current variable for packing
         ipts = nlat*nlon
         call maxmin(dtem,1,ipts,dmax,dmin)
c
c        Pack the data
         call tstcod(dtem,1,ipts,dmax,dmin,bsub,smpy,code)
c
c        Write the header line for this variable
         in = ivar(n)
         type = cvarrd(in)
         plev = float(iplev(n))
         write(lupack,220) type,plev,bsub,smpy
  220    format(1x,a1,1x,f6.1,2(1x,g15.9))
c
c        write packed data for this variable   
         krow = 1 + (ipts-1)/36
         do 25 k=1,krow
            ks = 1 + (k-1)*36
            ke = ks + 35
            write(lupack,230) (code(kk),kk=ks,ke)
  230       format(36(a2))
   25    continue
c
         close(ludata)
   99 continue
c
c  *****Start Jack****
      close(unit=lulog)
c  ****End Jack***      

      stop
c
  900 write(lulog,950) ifile
  950 format(/,' Error opening input file: ',a50)
      stop
c
  905 write(lulog,955) ifile
  955 format(/,' Error reading input file: ',a50)
      stop
c
  910 write(lulog,960) pfile
  960 format(/,' Error opening packed output file: ',/,a50)
      stop
c
  915 write(lulog,965) dfile
  965 format(/,' Error opening data file: ',/,a50)
      stop
c
  920 write(lulog,970) dfile
  970 format(/,' Unexpected end on data file: ',/,a50)
      stop
c
  925 write(lulog,975) nlong,nlonck,idlong,idlonck,
     +                 nlatg,nlatck,idlatg,idlatck
  975 format(/,' Basic grid parameters must remain consistent: ',
     +       /,' nlong=',i4,' nlonck=',i4,' idlong=',i8,' idlonck=',i8,
     +       /,' nlatg=',i4,' nlatck=',i4,' idlatg=',i8,' idlatck=',i8)
      stop
c
      end
      subroutine clen(char,lenmx,len_loc)
c     This routine finds the first blank in the character char
c     starting from the left to determine the length of char 
c
      character *(*) char
c
      len_loc = lenmx
      do 10 i=1,lenmx
         if (char(i:i) .eq. ' ') then
            len_loc = i-1
            go to 1000
         endif
   10 continue
c
 1000 continue
      return
      end