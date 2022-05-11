      program rcarq1
c this program filters the output CARQ lines to 
c individual storm position files.  This is done from standard input
c
c Input: POSITIONS file which contains the unordered CARQ ATCF records
c	 for all active storms. 
c
c Outputs: Extracted data from the POSITIONS file is reformated
c	   into one line per active storm.  This data is printed
c	   to standard output which is then redirected into a file
c	   named COORDINATES. 
c
c written by John Knaff, CIRA
c April 2001
c Modified : Nov 22, 2002
c Modified by A. Krautkramer Nov 25, 2002
c   - taylored the read statements to the ATCF files at NHC
c Modified by A. Krautkramer 3/28/2003
c	- added max intensity at -12 hrs to output
c	- added radius of maximum winds at 0 hrs  output
c Modified by A. Krautkramer 10/25/2006
c       - added a check for Southern Hemisphere storms
c         since in the SH storms forming after June 30 are
c         assigned to the next year.
c	- changed format statement so the storm name would 
c	  no longer be truncated.  Storm name is 11 characters not 10
c
c * discription of variables *
c
c     mm    - two digit month (1-12)
c     dd    - day of the month (0-31)
c     hh    - hour of the day
c     lat0  - Latitude position at t=0 hrs
c     lon0  - Longitude position at t= 0 hrs
c     lat12 - at t= -12 hrs
c     lon12 - at t= -12 hrs
c     vtm0  - Max intensity at t= 0 hrs
c	  vtm12 - Max intensity at t = -12 hrs
c     dir0  - Storm heading at t= 0 hrs
c     spd0  - Storm speed at t= 0 hrs
c	  rmw0  - Radius of maximum winds at t = 0 hrs
c     ijul  - Julian day
c     y2kyy - 4 digit year
c     stname- storm name
c     inum  - storm number - keeps track of multiple systems
c
c Notes written by A. Krautkramer
c   This program reads ATCF A deck formatted records and searches for storms.
c	If the program finds 0 and -12 CARQ positions for the same storm then
c   it calculates the julian day of the storm and outputs the pertainent storm
c   information.  A storm can be uniquely identified by the storm basin and 
c   storm number.
c
c************************************************************************
c
      implicit none
c      integer mm, dd, hh, lat0, lon0, lat12, lon12,vtm0, dir0, spd0, 
c     .     y2kyy,inum,ijul, md2jul 
	  integer mm, dd, hh, lat0, lon0, lat12, lon12, vtm0, vtm12,dir0
	  integer spd0, rmw0, y2kyy,inum,ijul, md2jul 
      integer istnum, itime, ifstnum
      character*6 stnum
      character*11 stname
      character*10 cdaytime
      character*220 line
      character*2 cbas,ifcbas
      character*1 ns,ew
      logical first,yes12,yes00
      real flon,flat
      external md2jul
c
c -- set logical variables
c
      first=.true.
      yes12=.false.
      yes00=.false.
c
c -- loop throught the CARQ lines in a goto loop
c
      inum=1
 1    read(*,10,end=99)line
 10   format(a220)
 	 
c
c -- Check basin
c
      read(line,14)cbas
  14  format(a2)
c  14  format(14x,a2)  
c 14  format(42x,a2) !original
c
c -- extract storm number and time
c  
      read(line,15)istnum,itime
  15  format(3x,i3,23x,i4)
c  15  format(3x,i2,38x,i4) 
c
c 15  format(45x,i3,23x,i4)! modifided for pathname if contained in the
c                           ! standard output from grep
c 15  format(3x,i3,23x,i4) !original
c
c -- if first then set up the storm number and basin
c
      if(first)then
         ifstnum=istnum
         ifcbas=cbas
         first=.false.
c
c -- check for change of the storm number or basin (i.e. a new storm)
c
      elseif(ifstnum.ne.istnum.or.ifcbas.ne.cbas)then
         ifstnum=istnum
         ifcbas=cbas
         inum=inum+1
         yes12=.false.
         yes00=.false.
      endif
      if (itime.eq.-12.or.itime.eq.00)then  ! we are only interested in
      else                                  ! these time periods
         goto 1
      endif	 
c
c -- information from the a-deck time minus 12
c
      if (itime.eq.-12)then
         read(line,30)cbas, istnum, cdaytime,  itime, lat12,ns,
     .        lon12,ew,vtm12
         yes12=.true.
  30      format (a2,1x,i3,2x,a10,11x,i4,1x,i4,a1,1x,i5,a1,1x,i4)		
c  30		  format (1x,a2,i2,17x,a10,11x,i4,1x,i4,a1,1x,i5,a1,1x,i4)	 
c 30      format (42x,a2,1x,i3,2x,a10,11x,i4,1x,i4,a1,1x,i5,a1)
c 30      format (a2,1x,i3,2x,a10,11x,i4,1x,i4,a,1x,i5,a) !original
c		print 666, cbas, istnum, cdaytime,  itime, lat12,ns,lon12,ew,vtm12
c 666	format  (1x,a2,1x,i2,1x,a10,1x,i4,1x,i4,1x,a1,1x,i5,1x,a1,1x,i4)  
c
c -- information from the a-deck at t = 0
c
      elseif(itime.eq.0)then
         read(line,45) cbas,istnum,cdaytime,itime,lat0,ns,lon0,ew,
     .        vtm0,rmw0,dir0,spd0,stname
  45      format (a2,1x,i3,2x,a10,11x,i4,1x,i4,a1,1x,i5,a1,1x,i4,57x,i4,
     .		    26x,i4,1x,i4,1x,a11)
c  45      format (1x,a2,i2,17x,a10,11x,i4,1x,i4,a1,1x,i5,a1,1x,i4,57x,i4
c     .			,26x,i4,1x,i4,1x,a10) 
c 45      format (a2,1x,i3,2x,a10,11x,i4,1x,i4,a1,i5,a1,i4,87x,i4
c     .        ,1x,i4,1x,a10)
c* 45      format (42x,a2,1x,i3,2x,a10,11x,i4,1x,i4,a1,1x,i5,a1,1x,i4,
c*     .        87x,i4
c*     .        ,1x,i4,1x,a10)		
		 yes00=.true.
      endif
      if (yes12.and.yes00)then
         if(stname.eq.'           ')stname='       XXXX'
         read(cdaytime,60)y2kyy,mm,dd,hh
 60      format(i4,i2,i2,i2)
c
c     Convert to Julian Day
c
         ijul=md2jul(dd,mm,y2kyy)
c
c -- hemispheric adjustments....
c
         if (ns.eq.'S'.or.ns.eq.'s')then
            flat=-10.
         else
            flat=10.
         endif
         if (ew.eq.'W'.or.ns.eq.'w')then
            flon=-10.
         else
            flon=10.
         endif
c
c
c     Output record number, year, julian day, synoptic hour, present earth 
c     coordinates, 12-hour old earth coordinated and current heading and 
c     speed.
c
	if (cbas .eq. 'SH' .and. mm .gt. 6) then
            write(*,75)inum, y2kyy,mm,dd,y2kyy,ijul,hh,float(lat0)/flat
     .      ,float(lon0)/flon,float(lat12)/flat, float(lon12)/flon, 
     .      dir0, spd0, rmw0, vtm0,vtm12,cbas,istnum,y2kyy-1999,stname
	 else
	    write(*,75)inum, y2kyy,mm,dd,y2kyy,ijul,hh,float(lat0)/flat
     .      ,float(lon0)/flon,float(lat12)/flat, float(lon12)/flon,
     .      dir0, spd0, rmw0, vtm0,vtm12,cbas,istnum,y2kyy-2000,stname
	 endif

 75      format(i2.2,1x,i4.4,2i2.2,1x,i4.4,i3.3,i2.2,4f7.1,1x,i3.3,1x,
     .        i3.3,1x,i3.3,1x,i3.3,1x,i3.3,1x,a2,i2.2,i2.2,1x,a11)
         goto 1
      else 
         goto 1
      endif
 99   stop
      end
***********S U B P R O G R A M S****************************************
      integer function md2jul(iday,month,iyear) 
  
c      this routine calculates the julian date from 
c      the year, month, and day.
  
          i n t e g e r 
     + nodays(12) 
  
          d a t a 
     + nodays/31,28,31,30,31,30,31,31,30,31,30,31/
  
      nodays(2)=28
      if(iyear.eq.0)go to 1 
      if(mod(iyear,4) .eq. 0) nodays(2)=29
      md2jul=0
    1 do 2 i=1,month
    2 md2jul = md2jul + nodays(i) 
      idayout = nodays(month)-iday
      md2jul = md2jul - idayout 
  
      return
      end 




