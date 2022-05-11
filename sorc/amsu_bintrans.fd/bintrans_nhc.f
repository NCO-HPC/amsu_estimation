      program bintrans_nhc
c     This program moves binary files created from the wgrib commands generated
c     by pullvar_nhc.ksh to a binary format which can be read by bin2pack_nhc.exe.  

      character *1 cvar3,cvar4,clev4,pds(100),c4(4),gds(1000)
      character *2 crun
      character *3 cfhour
      character *4 cvarin(5),clev(11),ctmpvar,ctmplev
c      character *6 cvarout(5),cvout,cdate
	  character *6 cvarout(5),cvout
	  character *6 cdate
      character *10 cinfo
      character *14 cdatfil
      character *16 clog
      character *13 cbinin
      character *13 ctmpout
      character *28 cdatdir
      character *80 cbinout,cbinfil,cwgrib
      integer kds(10)
      real data(65160)
	  
c	  Als code to obtain environmental variables                                
c	  set by perl code
c getenv is not considered Intrinsic by pgf90
c      INTRINSIC getenv
      call getenv("YMD", cdate)
	  call getenv("RUN", crun)
	  call getenv("FHOUR", cfhour)
      
      data lenmax /200/
      data nwords /65160/
      data clog /'bintrans_nhc.log'/
c      data cinfo /'gfs_indate'/
c      data cdatdir /'/home/dostalek/jack/gfs/bin/'/
      data cdatfil /'pullvar_nhc.in'/
      data ilog,iin,iout,iinfo /53,54,55,56/
      data nvars,nlevs /5,11/
      data cvarin /'UGRD','VGRD','HGT ','TMP ','RH  '/
      data cvarout/'u     ','v     ','height','temp  ','relh  '/
      data clev /'100 ','150 ','200 ','250 ','300 ','400 ','500 ',
     +'700 ','850 ','925 ','1000'/     
      data lenkds /10/
      data kds /10*0/
      kds(2)=360
      kds(3)=181
      kds(9)=1000
      kds(10)=1000
      
c     Open log file
c      open(unit=ilog,file=cdatdir//clog,status='replace')
	  open(unit=ilog,file=clog,status='replace')
      
c     Open file gfs_indate,which contains information about the gfsfile listed
c     in pullvar.in

c      open(unit=iinfo,file=cdatdir//cinfo,status='old')
c	  open(unit=iinfo,file=cinfo,status='old')
c      read(iinfo,5) cdate
c5     format(a6)
c      read(iinfo,10) crun
c10    format(a2)
c      read(iinfo,15) cfhour
c15    format(a3)   


c     Convert files of type 0000VAR.bin to format used by aapack.f            

      do 100 ivar=1,nvars
        ctmpvar=cvarin(ivar)
        cvar3=ctmpvar(3:3)
        cvar4=ctmpvar(4:4)
        nvarchar=4
        if(cvar4.eq.' ') nvarchar=3
        if(cvar3.eq.' ') nvarchar=2
        do 200 ilev=1,nlevs
          ctmplev=clev(ilev)
          clev4=ctmplev(4:4)
          nlevchar=4
          if(clev4.eq.' ') nlevchar=3
          cbinin=ctmplev(1:nlevchar)//ctmpvar(1:nvarchar)//'.bin'
         
          if(cbinin(1:5).eq.'250RH') goto 200
          if(cbinin(1:5).eq.'200RH') goto 200
          if(cbinin(1:5).eq.'150RH') goto 200
          if(cbinin(1:5).eq.'100RH') goto 200          
          
          call clen(cbinin,lenbin,lenmax)
         
c          cwgrib=cdatdir//cbinin(1:lenbin)
		  cwgrib=cbinin(1:lenbin)
          write(ilog,*) cwgrib
		  write(ilog,*) cdate
		  write(ilog,*) crun
		  write(ilog,*) cfhour
          
          
c      Create output binary file
          ctmpout='YYMMDDHH_F00_'          

c      Substitute year, month, date, and model run          
          ctmpout(1:6)=cdate
          ctmpout(7:8)=crun

c      Substitute model verification hour
          ctmpout(11:12)=cfhour(2:3)
          
          cvout=cvarout(ivar)
          call clen(cvout,ilen,6)
        
          cbinfil=ctmpout//cvout(1:ilen)//ctmplev(1:nlevchar)//'.gtmp' 
          call clen(cbinfil,lenfil,lenmax)
          
c          cbinout=cdatdir//cbinfil(1:lenfil)                                
 		  cbinout=cbinfil(1:lenfil)
		         
          write(ilog,*) cbinout

c      Open output file
          open(unit=iout,file=cbinout,status='replace',
     +    form='unformatted',access='sequential')     


c      ***The following sections of code were found at***
c      ***ftp://wesley.wwb.noaa.gov/pub/wgrib/formats_update.txt***               

         
          open(unit=iin,file=cwgrib,status='old',form='unformatted',
     +    access='sequential',err=9998)
      
c     Read PDS and length of PDS

          read(iin) c4,(pds(i),i=1,3)
      
c
c     Check for letters "PDS " in PDS
          if(c4(1).ne.'P') goto 9999            
          if(c4(2).ne.'D') goto 9999
          if(c4(3).ne.'S') goto 9999
          if(c4(4).ne.' ') goto 9999         
          
c
c     Calculate length of variable PDS
          lenpds=ichar(pds(1))*256*256+ichar(pds(2))*256+ichar(pds(3))
      
c     Print out length
          write(ilog,*) 'Length of variable PDS in the header= ',lenpds
      
c     Backspace
          backspace(unit=iin)
      
c     Read PDS using the legnth information
          read(iin) c4, (pds(i),i=1,lenpds)
      
c     Convert a character argument to an integer based on the character position
c     in the collating sequence
          idog1=ichar(pds(13))
          idog2=ichar(pds(14))
          idog3=ichar(pds(15))
          idog4=ichar(pds(7))
          
      
          write(ilog,8) idog1,idog2,idog3,idog4
8         format('PDS :',i10,1x,i5,1x,i6,1x,i3)            
                                        

c     Read GDS and length of the GDS
          read(iin) c4, (gds(i),i=1,3)
      
c
c     Check for letters "GDS " in the GDS
          if(c4(1).ne.'G') goto 9999            
          if(c4(2).ne.'D') goto 9999
          if(c4(3).ne.'S') goto 9999
          if(c4(4).ne.' ') goto 9999    

c
c     Calculate length of variable GDS
          lengds=ichar(gds(1))*256*256+ichar(gds(2))*256+ichar(gds(3))
          
c     Print out length
          write(ilog,*) 'Length of variable GDS in the header= ',lengds
          
c     Backspace
          backspace(unit=iin)
      
c     Read GDS using the lenght information
          read(iin) c4, (gds(i),i=1,lengds)
          
c     Convert a character argument to an integer based on the character position
c     in the collating sequence
          idog1=ichar(gds(13))
          idog2=ichar(gds(14))
          idog3=ichar(gds(15))
          
          write(ilog,9) idog1,idog2,idog3
9         format('GDS: ',i10,1x,i5,1x,i6)                                          
          write(ilog,*) ' ' 

c      ***End of code from ftp://wesley.wwb.noaa.gov/pub/wgrib/formats_update.txt***               


c         Read data from binary file

	  read(iin) (data(i),i=1,nwords)


c         Write data to output file

c         First record
          write(iout) lenpds,lenkds,nwords                 
          
c         PDS record
          write(iout) (pds(j),j=1,lenpds)
          
c         KDS record
          write(iout) (kds(j),j=1,lenkds)
          
c         Data
          write(iout) (data(j),j=1,nwords)          


        close(unit=iin)
        close(unit=iout)
200     continue
100   continue          
       
       
      close(unit=ilog) 
      stop
c     Error warning
9999  write(ilog,*) 'Error in beginning of PDS or GDS'      
      close(unit=ilog)
      
9998  continue      
      stop
      end
      
       subroutine clen(char,len,lenmx)
c     This routine finds the first blank in the character char
c     starting from the left to determine the length of char 
c     **** From aapack.f*****
      character *(*) char
c
      len = lenmx
      do 10 i=1,lenmx
         if (char(i:i) .eq. ' ') then
            len = i-1
            go to 1000
         endif
   10 continue
c
 1000 continue
      return
      end     
  
      
