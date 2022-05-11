      program pickavn
c     This program reads the list of files from ncep.list
c     and finds the date and time closest to that in the 
c     file mdcase.dat. The file name is written to ncep.fname
c     and the number of hours between the date and the file.
c
      parameter (mxf=2000)
c
      character *80 fline,flist(mxf)
c
      character *15 fnlist,fnout,fncase
      character *21 fname
c
      character *1 tind
c
      data fnlist,fncase,fnout /'ncep.list','temp_ret.dat',
     +                          'ncep.fname'/
      data lulist,lucase,luout /31,32,33/
c
c     Open output and input files
      open(unit=luout,file=fnout,form='formatted',status='new',
     +     err=910)
      open(unit=lulist,file=fnlist,form='formatted',status='old',
     +     err=900)
      open(unit=lucase,file=fncase,form='formatted',status='old',
     +     err=900)
c
c     Read the year, Julian Day and time from case file
c***  Modified read line to include "err=900" - by CIRA, April 2005
c      read(lucase,149) idum
      read(lucase,149,err=900) idum      
c***      
  149 format(i2)
      read(lucase,150) iyr,jday,ihr,imin,isec
  150 format(3x,i4,i3,1x,i2,i2,i2)
c
c     Convert Julian day to month and day
      call jdayi(jday,iyr,imn,idy)
c
c     Read the list of available files
      nf = 1
 1000 continue
         read(lulist,100,err=900,end=1100) flist(nf)
  100    format(a80)
	 nf = nf + 1
	 go to 1000
 1100 continue
      nf = nf-1
c
c     Find the file that is closest in time to requested date
      nhr=9999
      do 20 i=1,nf
	 fline = flist(i)
	 read(fline,300) jyr,tind,jmn,jdy
  300    format(4x,i2,1x,a1,i2,i2)
c
c        Decode date
         if (jyr .lt. 50) then
	    jyr=jyr+2000
         else
	    jyr=jyr+1900
         endif
c
	 if (tind .eq. 'X') then
	    jhr=0
         elseif (tind .eq. 'Y') then
	    jhr=6
         endif
c
	 if (jdy .gt. 50) then
	    jhr = jhr + 12
	    jdy = jdy - 50
         endif
c 
c        Calculate time difference
	 call tdiff(iyr,imn,idy,ihr,jyr,jmn,jdy,jhr,idt)
	 idt = iabs(idt)
c
	 if (idt .lt. nhr) then
	    fline = flist(i)
	    fname = fline(1:21)
	    nhr = idt
         endif
   20 continue
c
      write(luout,200) fname,nhr
  200 format(a21,/,i4)
      stop
c
  900 continue
      fname='XXXX'
      nhr = 9999
      write(luout,200) fname,nhr
c
  910 stop
      end
