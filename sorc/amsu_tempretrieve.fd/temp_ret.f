      program temp_ret
c
c     program reads gathers postion information from the COORDINATES file
c     reads raw brightness temperature data aquired via buffer data on the
c     NCEP IBM, creates a TIMES file, and temperature retreivals for each of 
c     the NOAA satellites (15,16,18).
c
c
c     INPUT:    COORDINATES - This file contains a sequential list of
c                             active storms their most current position
c                             and their position 12-h earlier along with
c                             heading, speed, intensity, and adeck storm
c                             number and basin information.
c               BUFFER DATA - Three files will be generated for each storm 
c                             in the COORDINATES file with the following
c                             naming convention:
c                             noaa15.out
c                             noaa16.out
c                             noaa18.out
c                             noaa19.out
c                             nasaaq.out
c                             metop2.out
c		DUMP FILE -   This file contains specific information about
c			      the storm including the date time group,
c			      the geographic location, and the ATCF storm
c			      number.  
c		COEFFICIENT FILES - Assorted files containing temperature
c				    retrieval coefficents. 
****************************************************************************
c
c     OUTPUT:       
c    Temperature Retreivals - Three files will be generated for each storm 
c                             in the COORDINATES file with the following
c                             naming convention:
c                             noaa15.ret
c                             noaa16.ret
c                             noaa18.ret
c                             noaa19.ret
c                             nasaaq.ret
c                             metop2.ret
c    Log files for each
c                             noaa15.log
c                             noaa16.log
c                             noaa18.log
c                             noaa19.log
c                             nasaaq.log
c                             metop2.log
c
c
****************************************************************************
*
*
*     Written By   J. Knaff
*                  CIRA
*                  March 2003
*
*     Last Modified: Dec 13, 2005
*
*     History:  
*     April 10, 2003,  changed the documentation at the top
*                      made modifications to M. Goldenbergs code
*                      land is now coded as 0  (damn wierd!!!)
*     December 13, 2005, J. Knaff replaced NOAA 17 with NOAA 18
*     April 3, 2007, J. Knaff changed logic to account for different 
*                    times being assigned to line elements as a result
*                    of changes to the readbfr code and (likely) changes
*                    to the AMSU database on the IBMs which took affect
*                    02/27/2007.
*     July 7, 2008, J. Knaff updated the NOAA16 coefficient files and 
*		   as a result updated the code to read the files. The coefficient
* 		   files were updated because of a channel 4 issue.   
*     December 30,2010, J. Knaff, Add the capability to uses AMSU-A data
*                    from Metop 2A, and NOAA19.  J. Dostalek provided *.out
*                    files for testing.  Aqua does not have land/sea
*                    information.
*     January 7, 2011, Provided land mask and elevation information to
*                    the AQUA dataset by adding a subroutine, landelev.f90,
*                    which uses llintsp.f and datafile lsmask30.  The later
*                    provides 30-minute global elevations and ocean depths. Added
*                    logical variable lefirst
*     March 22, 2011, J. Knaff, Cleaned up the code for distribution to
*                       NCO and A.Krautkramer at NHC.
*     Jan 8, 2013, A Krautkramer - WCOSS transition  
*
****************************************************************************
      implicit none  
c
c     functions
c
      external md2jul
      integer md2jul
c
c     subroutines
c
      external jdayi, distk, llintsp, landelev
c
c     parameters variables
c
      integer ip, nup
      parameter(ip=10000,nup=30)
c
c     misc. integer variables
c
      integer m, n, j
      integer it1, it2, istuff
      integer inumd, iflag
c
c     data integer variables
c
      integer num15, num16, num18, num19, numaq, numm2
      integer idate15(ip),itime15(ip),isat15(ip),isen15(ip),
     .     iscan15(ip),ielev15(ip),isatz15(ip),ls15(ip)
      integer idate16(ip),itime16(ip),isat16(ip),isen16(ip),
     .     iscan16(ip),ielev16(ip),isatz16(ip),ls16(ip)
      integer idate18(ip),itime18(ip),isat18(ip),isen18(ip),
     .     iscan18(ip),ielev18(ip),isatz18(ip),ls18(ip)
      integer idate19(ip),itime19(ip),isat19(ip),isen19(ip),
     .     iscan19(ip),ielev19(ip),isatz19(ip),ls19(ip)
      integer idateaq(ip),itimeaq(ip),isataq(ip),isenaq(ip),
     .     iscanaq(ip),ielevaq(ip),isatzaq(ip),lsaq(ip)
      integer idatem2(ip),itimem2(ip),isatm2(ip),isenm2(ip),
     .     iscanm2(ip),ielevm2(ip),isatzm2(ip),lsm2(ip)
c
c     time/date variables
c
      integer times(2),dates(2)
      integer inum,iy,im,id,i,dir0,spd0,vtmax,numatcf,vtmax12,irmw
      integer iy2,im2,id2,hh2,mm2,ss2,jd2,iy1,im1,id1,hh1,mm1,ss1
      integer sattime,satdate,satjday,idt,imt,iyt,ijult,ijul,idtg,jd1
c
c     data real variables
c
      real rlat15(ip),rlon15(ip),satzen15(ip),solzen15(ip),tb15(ip,15),
     .     temp15(ip,40),tpw15(ip),clw15(ip)
      real rlat16(ip),rlon16(ip),satzen16(ip),solzen16(ip),tb16(ip,15),
     .     temp16(ip,40),tpw16(ip),clw16(ip)
      real rlat18(ip),rlon18(ip),satzen18(ip),solzen18(ip),tb18(ip,15),
     .     temp18(ip,40),tpw18(ip),clw18(ip)     
      real rlat19(ip),rlon19(ip),satzen19(ip),solzen19(ip),tb19(ip,15),
     .     temp19(ip,40),tpw19(ip),clw19(ip)
      real rlataq(ip),rlonaq(ip),satzenaq(ip),solzenaq(ip),tbaq(ip,15),
     .     tempaq(ip,40),tpwaq(ip),clwaq(ip)
      real rlatm2(ip),rlonm2(ip),satzenm2(ip),solzenm2(ip),tbm2(ip,15),
     .     tempm2(ip,40),tpwm2(ip),clwm2(ip)
      real lats(2),lons(2)
c
c     misc.  real variables
c
      real dx, dy
      real closest,strmlatc,strmlonc,strmlat,strmlon,
     .     satclat1,satclon1,satclat2,satclon2,satclat,satclon,
     .     dist2cen,rad,tdiff,tpw,clw
      real bl(15),dummy(15),tret(40),bt(15)
c
      character*6 stnum
      character*8 cdir
      character*12 cstuff
      character*11 stname
      character*12 dumpfile
      character*11 coorfile
      character*2 cbasin
c      character*10 cdata15, cdata16, cdata18
c      character*10 coutp15, coutp16, coutp18
c      character*10 clogf15, clogf16, clogf18
      character*10 cdata15, cdata16, cdata18, cdata19, cdataaq, cdatam2
      character*10 coutp15, coutp16, coutp18, coutp19, coutpaq, coutpm2
      character*10 clogf15, clogf16, clogf18, clogf19, clogfaq, clogfm2
c
c     logical variables
c
      logical open15,open16,open18,open19,openaq,openm2,opendump
      logical lefirst
c
c***********************************************************************
c     Start executable part of the code
c***********************************************************************
c
c     open dump files, one for each entry in the COORDINATES file
c
      do i=1,10
         write(dumpfile,50)i
 50      format('dumpjb.in_',i2.2)
         opendump=.false.
         inquire(file=dumpfile,exist=opendump)
         if (opendump)then
            inumd=i
            open (1,file=dumpfile,status='old')
            read(1,51)idtg
 51         format(i10)
            print *,idtg
            read(1,*)
c            read(1,*)istuff
            read(1,52)cdir
 52         format(a8)
            print *,cdir
            close (1)
         endif
      enddo

c     
c
c     open COORDINATE file
c
      coorfile='COORDINATES'
      open (10,file=coorfile,status='old')
c
c     read information from the coordinate files and loop through storms
c 
 1    read(10,30,err=888) inum,iy,im,id,it1,it2,times(2),lats(2),
     .     lons(2),lats(1),lons(1),dir0,spd0,irmw,vtmax,vtmax12,
     .     stnum,stname
 30      format(i2.2,1x,i4.4,2i2.2,1x,i4.4,i3.3,i2.2,4f7.1,1x,
     .     5(i3.3,1x),a6,1x,a11)
         if(inum.eq.inumd)goto 31
         goto 1
 31   continue
      close(10)
c
c     internal read to strip atcf information out of the coordinate files
c     which will be used to access the ascii buffer data.
c
      read(stnum,45)cbasin,numatcf
 45   format(a2,i2)
c
c
c     convert date to julian day
c
      ijul=md2jul(id,im,iy)
c
c     create dates(2)
c
      dates(2)=iy*10000+im*100+id
c
c     determine the t-12h position time and date
c
      times(2)=times(2)         ! nearest synoptic hour in this application
c
      times(1)=times(2)-12
      if (times(1).lt.0)then
         times(1)=24+times(1)
         ijult=ijul-1
         call jdayi(ijult,iyt,imt,idt)
         dates(1)=iyt*10000+imt*100+idt
      else
         dates(1)=iy*10000+im*100+id
      endif
c
c     convert hrs into hhmmss
c
      times(1)=times(1)*10000
      times(2)=times(2)*10000
c
c     construct the datafile names for NOAA - 15, 16, 18 etc..
c
      cdata15='noaa15.out'
      cdata16='noaa16.out'
      cdata18='noaa18.out'
      cdata19='noaa19.out'
      cdataaq='nasaaq.out'
      cdatam2='metop2.out'
c
c     construct output files for this portion of the loop
c
      coutp15='noaa15.ret'
      coutp16='noaa16.ret'
      coutp18='noaa18.ret'
      coutp19='noaa19.ret'
      coutpaq='nasaaq.ret'
      coutpm2='metop2.ret'
c
c     construct log file names for this date
c
      clogf15='noaa15.log'
      clogf16='noaa16.log'
      clogf18='noaa18.log'
      clogf19='noaa19.log'
      clogfaq='nasaaq.log'
      clogfm2='metop2.log'
c
c     open data(old),output(new),and log (new) files
c
      inquire(file=cdata15,exist=open15)
      if(open15)then
         open(15,file=cdata15,status='old')
         open(25,file=coutp15,status='replace')
         open(35,file=clogf15,status='replace')
      endif
      inquire(file=cdata16,exist=open16)
      if(open16)then
         open(16,file=cdata16,status='old')
         open(26,file=coutp16,status='replace')
         open(36,file=clogf16,status='replace')
      endif
      inquire(file=cdata18,exist=open18)
      if(open18)then
         open(18,file=cdata18,status='old')
         open(28,file=coutp18,status='replace')
         open(38,file=clogf18,status='replace')
      endif
      inquire(file=cdata19,exist=open19)
      if(open19)then
         open(19,file=cdata19,status='old')
         open(29,file=coutp19,status='replace')
         open(39,file=clogf19,status='replace')
      endif
      inquire(file=cdataaq,exist=openaq)
      if(openaq)then
         open(11,file=cdataaq,status='old')
         open(21,file=coutpaq,status='replace')
         open(31,file=clogfaq,status='replace')
      endif
      inquire(file=cdatam2,exist=openm2)
      if(openm2)then
         open(12,file=cdatam2,status='old')
         open(22,file=coutpm2,status='replace')
         open(32,file=clogfm2,status='replace')
      endif
c    
c     read data NOAA 15
c
      if(open15)then
         m=1
 115     read(15,75,end=915) idate15(m),itime15(m),rlat15(m),
     &        rlon15(m),isat15(m),isen15(m),iscan15(m),ls15(m),
     &        satzen15(m),solzen15(m),ielev15(m),isatz15(m),
     &        (tb15(m,n),n=1,15)
 75      format(i8,1x,i6,1x,2(f8.3,1x),1x,i2,2x,i3,2x,i2,2x,i1,2x,
     &        f8.3,1x,f8.3,1x,i5,2x,i6,2x,15(f6.2,2x))
         m=m+1
         goto 115
c
 915     continue
         if (m.eq.1)then 
            write(35,835) cdata15,coutp15
 835        format('Error encountered when reading file ',a10,/,
     &           'The file ',a10,' will be contain COORDINATE and ',
     &           'TIMES data, but no data')
         endif
         write(35,935) cdata15,m-1
 935     format('data file ',a10,' read with ',i8,' data points')
         num15=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the storm
c     center.
c
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
c
cvvvvv
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
c^^^^^
c
         do i=1,num15
c
cvvvv
c     Check for elements 15 and 16
      if (iscan15(i).eq.15) then
c^^^^
            
c 
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idate15(i)/10000
            im1=(idate15(i)-iy1*10000)/100
            id1=(idate15(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itime15(i)/10000
            mm1=(itime15(i)-hh1*10000)/100
            ss1=(itime15(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .           (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c
c     calculate distance from strom center to this point at the point relative
c     time.
            call distk(strmlon,strmlat,rlon15(i),rlat15(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itime15(i)
               satdate=idate15(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
               satclat1=rlat15(i)
               satclon1=rlon15(i)
               satclat2=rlat15(i)
               satclon2=rlon15(i)
               if (i.ne.num15)then
                  if (iscan15(i+1).eq.16.)then
                     satclat2=rlat15(i)
                     satclon2=rlon15(i)
                  endif
               endif
            endif
           endif
         enddo
         write(35,945)strmlatc,strmlonc,satdate,sattime,closest
 945     format('storm center located is  at ',2f8.2,' at ',2i10,/,
     .        'The closest point to this center is ',f8.1,' km')
c
c     one the satellite relative storm center is found, find the center of 
c     the amsu swath that corresponds to the time of the observation.
c
c	  satclat1=0.0
c         satclon1=0.0
c         satclat2=0.0
c         satclon2=0.0
c         do i=1,num15
c            if (itime15(i).eq.sattime)then
c               if (iscan15(i).eq.15)then
c                  satclat1=rlat15(i)
c                  satclon1=rlon15(i)
c               endif
c               if (iscan15(i).eq.16)then
c                  satclat2=rlat15(i)
c                  satclon2=rlon15(i)
c               endif
c            endif
c         enddo
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(35,955)satclat,satclon,sattime,dist2cen
 955     format('satellite center is located at ',2f8.2,' at ',i6,/,
     .        'This point is ',f8.1,' km from the closest point')
c
c     writing coordinate and times information to output file and log file
c
         write(35,965)
 965     format('writing COORDINATE and TIMES information')
         write(25, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(25,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
 975     format(i2.2,1x,i4.4,i3.3,1x,i6.6,4f9.2)
         write(25,985)num15
 985     format(i6)
c     
c     call temperature retrieval
c    
         if (num15.ne.0) write(35,995)
 995     format('starting NOAA 15 temperature retieval')
c
         do i=1, num15
            iflag=0
            do j=1,15
               dummy(j)=tb15(i,j)
               if (j.ne.11.and.j.ne.14.and.dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_n15(nup,dummy,bt,iscan15(i),ls15(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  temp15(i,j)=tret(j)
               else
                  temp15(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpw15(i)=tpw
               clw15(i)=clw
            else
               tpw15(i)=-99.00
               clw15(i)=-99.00
            endif
c
c     write NOAA 15 ascii data with temperature retreivals, total 
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(25,1575) idate15(i),itime15(i),rlat15(i),
     &           rlon15(i),isat15(i),isen15(i),iscan15(i),ls15(i),
     &           satzen15(i),solzen15(i),ielev15(i),isatz15(i),
     &           (tb15(i,n),n=1,15),(temp15(i,n),n=1,40),tpw15(i),
     &           clw15(i)
 1575       format(i8,1x,i6,1x,2(f8.3,1x),1x,i2,2x,i3,2x,i2,2x,i1,2x,
     &           f8.3,1x,f8.3,1x,i5,2x,i6,2x,57(f6.2,2x))
         enddo
c
c     close NOAA 15 output file for coordinate file inum 
         if (num15.ne.0)write(35,990)
 990     format('Normal completion of NOAA 15 temperature Retrieval')
 1015    continue
C         call flush_(15)
          flush (15)
         close(15)
C         call flush_(25)
          flush (25)
         close(25)
C         call flush_(35)
          flush (35)
         close(35)
      endif

c    
c     read data NOAA 16
c
      if(open16)then
         m=1
 116     read(16,75,end=916) idate16(m),itime16(m),rlat16(m),
     &        rlon16(m),isat16(m),isen16(m),iscan16(m),ls16(m),
     &        satzen16(m),solzen16(m),ielev16(m),isatz16(m),
     &        (tb16(m,n),n=1,15)
         m=m+1
         goto 116
 916     continue
         if(m.eq.1) then
            write(36,835) cdata16,coutp16
         endif
         write(36,935) cdata16,m-1
         num16=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the storm
c     center.
c
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
cvvvv
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
c^^^^
         do i=1,num16

cvvvv
            if (iscan16(i).eq.15) then
c^^^^
c 
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idate16(i)/10000
            im1=(idate16(i)-iy1*10000)/100
            id1=(idate16(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itime16(i)/10000
            mm1=(itime16(i)-hh1*10000)/100
            ss1=(itime16(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .           (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c
c    calculate distance from strom center to this point at the point relative
c    time.
           call distk(strmlon,strmlat,rlon16(i),rlat16(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itime16(i)
               satdate=idate16(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
cvvvv
               satclat1=rlat16(i)
               satclon1=rlon16(i)
               satclat2=rlat16(i)
               satclon2=rlon16(i)
               if (i.ne.num16)then
                  if (iscan16(i+1).eq.16)then
                     satclat2=rlat16(i)
                     satclon2=rlon16(i)
                  endif
               endif
           endif
c^^^^^
          endif
         enddo
         write(36,945)strmlatc,strmlonc,satdate,sattime,closest
c
c     one the satellite relative storm center is found, find the center of 
c     the amsu swath that corresponds to the time of the observation.
c
cvvvv
c        satclat1=0.0
c         satclon1=0.0
c         satclat2=0.0
c         satclon2=0.0
c         do i=1,num16
c            if (itime16(i).eq.sattime)then
c               if (iscan16(i).eq.16)then
c                  satclat1=rlat16(i)
c                  satclon1=rlon16(i)
c               endif
c               if (iscan16(i).eq.16)then
c                  satclat2=rlat16(i)
c                  satclon2=rlon16(i)
c               endif
c            endif
c         enddo
c^^^^^
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(36,955)satclat,satclon,sattime,dist2cen
c
c     writing COORDINATE, and timesinformation to output file and log file
c
         write(36,965)
         write(26, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(26,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
         write(26,985)num16
c
c     call temperature retrieval
c    
         if(num16.ne.0)write(36,996)
 996     format('starting NOAA 16 temperature retieval')
         do i=1, num16
            iflag=0
            do j=1,15
               dummy(j)=tb16(i,j)
c --- Changed 7/7/2008
c               if (dummy(j).lt.0.0)iflag=1
                if (j.ne.4.and.dummy(j).lt.0.0)iflag=1
c---   End Change 7/7/2008
            enddo
            call amsua_ret_n16(nup,dummy,bt,iscan16(i),ls16(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  temp16(i,j)=tret(j)
               else
                  temp16(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpw16(i)=tpw
               clw16(i)=clw
            else
               tpw16(i)=-99.00
               clw16(i)=-99.00
            endif
c
c     write NOAA 16 ascii data with temperature retreivals, total 
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(26,1575) idate16(i),itime16(i),rlat16(i),
     &           rlon16(i),isat16(i),isen16(i),iscan16(i),ls16(i),
     &           satzen16(i),solzen16(i),ielev16(i),isatz16(i),
     &           (tb16(i,n),n=1,15),(temp16(i,n),n=1,40),tpw16(i),
     &           clw16(i)
         enddo
c
c     close NOAA 16 output file for coordinate file inum 
         if(num16.ne.0)write(36,991)
 991     format('Normal completion of NOAA 16 temperature Retrieval')
 1016    continue
C         call flush_(16)
          flush (16)
         close(16)
C         call flush_(26)
          flush (26)
         close(26)
C         call flush_(36)
          flush (36)
         close(36)
      endif
c    
c     read data NOAA 18
c
      if(open18)then
         m=1
 118     read(18,75,end=918) idate18(m),itime18(m),rlat18(m),
     &        rlon18(m),isat18(m),isen18(m),iscan18(m),ls18(m),
     &        satzen18(m),solzen18(m),ielev18(m),isatz18(m),
     &        (tb18(m,n),n=1,15)
         m=m+1
         goto 118
 918     continue
         if(m.eq.1) then
            write(38,835) cdata18,coutp18
         endif   
         write(38,935) cdata18,m-1
         num18=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the storm
c     center.
c
c
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
cvvvv
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
c^^^^
c
         do i=1,num18
cvvvv
            if (iscan18(i).eq.15) then
c^^^^
c 
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idate18(i)/10000
            im1=(idate18(i)-iy1*10000)/100
            id1=(idate18(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itime18(i)/10000
            mm1=(itime18(i)-hh1*10000)/100
            ss1=(itime18(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .              (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c     
c     calculate distance from strom center to this point at the point relative
c     time.
c
            call distk(strmlon,strmlat,rlon18(i),rlat18(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itime18(i)
               satdate=idate18(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
               satclat1=rlat18(i)
               satclon1=rlon18(i)
               satclat2=rlat18(i)
               satclon2=rlon18(i)
               if (i.ne.num18)then
                  if (iscan18(i).eq.16)then
                     satclat2=rlat18(i)
                     satclon2=rlon18(i)
                  endif
               endif
            endif
         endif
      enddo
         write(38,945)strmlatc,strmlonc,satdate,sattime,closest
c
c     one the satellite relative storm center is found, find the center of 
c     the amsu swath that corresponds to the time of the observation.
c     
cvvvvv
c        satclat1=0.0
c         satclon1=0.0
c         satclat2=0.0
c         satclon2=0.0
c         do i=1,num18
c            if (itime18(i).eq.sattime)then
c               if (iscan18(i).eq.18)then
c                  satclat1=rlat18(i)
c                  satclon1=rlon18(i)
c               endif
c               if (iscan18(i).eq.18)then
c                  satclat2=rlat18(i)
c                  satclon2=rlon18(i)
c               endif
c            endif
c         enddo
c^^^^^
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(38,955)satclat,satclon,sattime,dist2cen
c     
c     writing COORDINATE, TIMES, number of points information to output file
c
         write(38,965)
         write(28, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(28,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
         write(28,985)num18
c
c     call temperature retrieval
c    
         if(num18.ne.0)write(38,989)
 989     format('starting NOAA 18 temperature retieval')
         do i=1, num18
            iflag=0
            do j=1,15
               dummy(j)=tb18(i,j)
               if (j.ne.7.and.dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_n18(nup,dummy,bt,iscan18(i),ls18(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  temp18(i,j)=tret(j)
               else
                  temp18(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpw18(i)=tpw
               clw18(i)=clw
            else
               tpw18(i)=-99.00
               clw18(i)=-99.00
            endif
c
c     write NOAA 18 ascii data with temperature retreivals, total 
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(28,1575) idate18(i),itime18(i),rlat18(i),
     &           rlon18(i),isat18(i),isen18(i),iscan18(i),ls18(i),
     &           satzen18(i),solzen18(i),ielev18(i),isatz18(i),
     &           (tb18(i,n),n=1,15),(temp18(i,n),n=1,40),tpw18(i),
     &           clw18(i)
         enddo
c
c     close NOAA 18 output file for coordinate file inum 
         if(num18.ne.0)write(38,997)
 997     format('Normal completion of NOAA 18 temperature Retrieval')
 1018    continue
C         call flush_(18)
          flush (18)
         close(18)
C         call flush_(28)
          flush (28)
         close(28)
C         call flush_(38)
          flush (38)
         close(38)
      endif
c     read data NOAA 19
c
      if(open19)then
         m=1
 119     read(19,75,end=919) idate19(m),itime19(m),rlat19(m),
     &        rlon19(m),isat19(m),isen19(m),iscan19(m),ls19(m),
     &        satzen19(m),solzen19(m),ielev19(m),isatz19(m),
     &        (tb19(m,n),n=1,15)
         m=m+1
         goto 119
 919     continue
         if(m.eq.1) then
            write(39,835) cdata19,coutp19
         endif
         write(39,935) cdata19,m-1
         num19=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the
c     storm center.
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
c
         do i=1,num19
            if (iscan19(i).eq.15) then
c
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idate19(i)/10000
            im1=(idate19(i)-iy1*10000)/100
            id1=(idate19(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itime19(i)/10000
            mm1=(itime19(i)-hh1*10000)/100
            ss1=(itime19(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .              (ss2-ss1)/3600.
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c
c     calculate distance from strom center to this point at the point
c     relative time.
c
            call distk(strmlon,strmlat,rlon19(i),rlat19(i),dx,dy,rad)

c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itime19(i)
               satdate=idate19(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
               satclat1=rlat19(i)
               satclon1=rlon19(i)
               satclat2=rlat19(i)
               satclon2=rlon19(i)
               if (i.ne.num19)then
                  if (iscan19(i).eq.16)then
                     satclat2=rlat19(i)
                     satclon2=rlon19(i)
                  endif
               endif
            endif
         endif
      enddo
         write(39,945)strmlatc,strmlonc,satdate,sattime,closest
c
c     one the satellite relative storm center is found, find the center
c     of the amsu swath that corresponds to the time of the observation.
c
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(39,955)satclat,satclon,sattime,dist2cen
c
c     writing COORDINATE, TIMES, number of points information to output
c     file
c
         write(39,965)
         write(29, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(29,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
         write(29,985)num19
c
c     call temperature retrieval
c
         if(num19.ne.0)write(39,998)
 998     format('starting NOAA 19 temperature retieval')
         do i=1, num19
            iflag=0
            do j=1,15
               dummy(j)=tb19(i,j)
               if (dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_n19(nup,dummy,bt,iscan19(i),ls19(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  temp19(i,j)=tret(j)
               else
                  temp19(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpw19(i)=tpw
               clw19(i)=clw
            else
               tpw19(i)=-99.00
               clw19(i)=-99.00
            endif
c
c     write NOAA 19 ascii data with temperature retreivals, total
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(29,1575) idate19(i),itime19(i),rlat19(i),
     &           rlon19(i),isat19(i),isen19(i),iscan19(i),ls19(i),
     &           satzen19(i),solzen19(i),ielev19(i),isatz19(i),
     &           (tb19(i,n),n=1,15),(temp19(i,n),n=1,40),tpw19(i),
     &           clw19(i)
         enddo
c
c     close NOAA 19 output file for coordinate file inum
c
         if(num19.ne.0)write(39,993)
 993     format('Normal completion of NOAA 19 temperature Retrieval')
 1019    continue
c         call flush_(19)
          flush (19)
         close(19)
c         call flush_(29)
          flush (29)
         close(29)
c         call flush_(39)
          flush (39)
         close(39)
      endif
c
c     read data Aqua
c
      if(openaq)then
         m=1
         lefirst=.true.
 111     read(11,75,end=911) idateaq(m),itimeaq(m),rlataq(m),
     &        rlonaq(m),isataq(m),isenaq(m),iscanaq(m),lsaq(m),
     &        satzenaq(m),solzenaq(m),ielevaq(m),isatzaq(m),
     &        (tbaq(m,n),n=1,15)

c     Aqua did not contain land/sea mask or elevation information.  The
c     routine landelev provides this information by reading a 30-minute
c     elevation/depth dataset and linearly interpolating those data to
c     the provided latitude and longitude points. USES landelev.f90,
c     llintsp.f, and lsmask30, the later is a data file.  - J. Knaff

         call landelev(rlonaq(m),rlataq(m),lefirst,ielevaq(m),lsaq(m))

         m=m+1
         goto 111
 911     continue
         if(m.eq.1) then
            write(31,835) cdataaq,coutpaq
         endif
         write(31,935) cdataaq,m-1
         numaq=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the
c     storm center.
c
         sattime=times(2)
        satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
c
         do i=1,numaq
            if (iscanaq(i).eq.15) then
c
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idateaq(i)/10000
            im1=(idateaq(i)-iy1*10000)/100
            id1=(idateaq(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
           hh1=itimeaq(i)/10000
            mm1=(itimeaq(i)-hh1*10000)/100
            ss1=(itimeaq(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .              (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c
c     calculate distance from strom center to this point at the point
c     relative time.
c
            call distk(strmlon,strmlat,rlonaq(i),rlataq(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itimeaq(i)
               satdate=idateaq(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
               satclat1=rlataq(i)
               satclon1=rlonaq(i)
               satclat2=rlataq(i)
               satclon2=rlonaq(i)
               if (i.ne.numaq)then
                  if (iscanaq(i).eq.16)then
                     satclat2=rlataq(i)
                     satclon2=rlonaq(i)
                  endif
               endif
            endif
         endif
      enddo
        write(31,945)strmlatc,strmlonc,satdate,sattime,closest
c
c     once the satellite relative storm center is found, find the center
c     of the amsu swath that corresponds to the time of the observation.
c
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(31,955)satclat,satclon,sattime,dist2cen
c     writing COORDINATE, TIMES, number of points information to output
c     file
c
         write(31,965)
         write(21, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(21,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
         write(21,985)numaq
c
c     call temperature retrieval
c
         if(numaq.ne.0)write(31,891)
 891     format('starting Aqua temperature retieval')
         do i=1, numaq
            iflag=0
            do j=1,15
               dummy(j)=tbaq(i,j)
               if (j.ne.4.and.dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_aqua(nup,dummy,bt,iscanaq(i),lsaq(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                  tempaq(i,j)=tret(j)
               else
                  tempaq(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpwaq(i)=tpw
               clwaq(i)=clw
            else
               tpwaq(i)=-99.00
               clwaq(i)=-99.00
            endif
c
c     write Aqua ascii data with temperature retreivals, total
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(21,1575) idateaq(i),itimeaq(i),rlataq(i),
     &           rlonaq(i),isataq(i),isenaq(i),iscanaq(i),lsaq(i),
     &           satzenaq(i),solzenaq(i),ielevaq(i),isatzaq(i),
     &           (tbaq(i,n),n=1,15),(tempaq(i,n),n=1,40),tpwaq(i),
     &           clwaq(i)
         enddo
c
c     close Aqua output file for coordinate file inum
         if(numaq.ne.0)write(31,893)
 893     format('Normal completion of Aqua temperature Retrieval')
 1011    continue
c         call flush_(11)
          flush(11)
         close(11)
c       call flush_(21)
          flush (21)
         close(21)
c         call flush_(31)
          flush (31)
         close(31)
      endif
c
c     read data Metop 2A
c
      if(openm2)then
         m=1
 112     read(12,75,end=912) idatem2(m),itimem2(m),rlatm2(m),
     &        rlonm2(m),isatm2(m),isenm2(m),iscanm2(m),lsm2(m),
     &        satzenm2(m),solzenm2(m),ielevm2(m),isatzm2(m),
     &        (tbm2(m,n),n=1,15)
         m=m+1
         goto 112
 912     continue
         if(m.eq.1) then
            write(32,835) cdatam2,coutpm2
         endif
         write(32,935) cdatam2,m-1
         numm2=m-1
         closest=9999.9
c
c     loop through the data to find the closest satellite point to the
c     storm center.
c
         sattime=times(2)
         satdate=iy*10000+im*100+id
         satjday=ijul
         strmlatc=lats(2)
         strmlonc=lons(2)
         satclat1=0.0
         satclon1=0.0
         satclat2=0.0
         satclon2=0.0
c
         do i=1,numm2
            if (iscanm2(i).eq.15) then
c
c     Calculate time difference for the extrapolation/interpolation
c
            iy2=dates(2)/10000
            im2=(dates(2)-iy2*10000)/100
            id2=(dates(2)-(iy2*10000+im2*100))
            hh2=times(2)/10000
            mm2=(times(2)-hh2*10000)/100
            ss2=(times(2)-(hh2*10000+mm2*100))
            jd2=md2jul(id2,im2,iy2)
            iy1=idatem2(i)/10000
            im1=(idatem2(i)-iy1*10000)/100
            id1=(idatem2(i)-(iy1*10000+im1*100))
            jd1=md2jul(id1,im1,iy1)
            hh1=itimem2(i)/10000
            mm1=(itimem2(i)-hh1*10000)/100
            ss1=(itimem2(i)-(hh1*10000+mm1*100))
            tdiff=(jd2-jd1)*24.+(hh2-hh1)+(mm2-mm1)/60.+
     .              (ss2-ss1)/3600.
c
c     find the storm center
c
            strmlat=lats(2) + tdiff*((lats(1)-lats(2))/12.)
            strmlon=lons(2) + tdiff*((lons(1)-lons(2))/12.)
c
c     calculate distance from strom center to this point at the point
c     relative time.
c
            call distk(strmlon,strmlat,rlonm2(i),rlatm2(i),dx,dy,rad)
c
c     save the closest point
c
            if (rad.lt.closest)then
               closest=rad
               sattime=itimem2(i)
               satdate=idatem2(i)
               satjday=jd1
               strmlatc=strmlat
               strmlonc=strmlon
               satclat1=rlatm2(i)
               satclon1=rlonm2(i)
               satclat2=rlatm2(i)
               satclon2=rlonm2(i)
               if (i.ne.numm2)then
                  if (iscanm2(i).eq.16)then
                     satclat2=rlatm2(i)
                     satclon2=rlonm2(i)
                  endif
               endif
            endif
         endif
      enddo
         write(32,945)strmlatc,strmlonc,satdate,sattime,closest
c
c     once the satellite relative storm center is found, find the center
c     of the amsu swath that corresponds to the time of the observation.
c
         satclat=(satclat1+satclat2)/2.
         satclon=(satclon1+satclon2)/2.
         call distk(strmlon,strmlat,satclon,satclat,dx,dy,rad)
         dist2cen=rad
         write(32,955)satclat,satclon,sattime,dist2cen
c
c     writing COORDINATE, TIMES, number of points information to output
c     file
c
         write(32,965)
         write(22, 30)inum,iy,im,id,it1,it2,times(2)/10000
     .        ,lats(2),lons(2),lats(1),lons(1),dir0,spd0,irmw,
     .        vtmax,vtmax12,stnum,stname
         write(22,975)inum,satdate/10000,satjday,sattime,
     .        satclat,satclon,strmlatc,strmlonc
         write(22,985)numm2
c
c     call temperature retrieval
c
         if(numm2.ne.0)write(32,992)
 992     format('starting METOP2A temperature retieval')
         do i=1, numm2
            iflag=0
            do j=1,15
               dummy(j)=tbm2(i,j)
               if (j.ne.7.and.dummy(j).lt.0.0)iflag=1
            enddo
            call amsua_ret_metop2(nup,dummy,bt,iscanm2(i),lsm2(i),bl,
     &           tret,tpw,clw)
            do j=1,40
               if (iflag.eq.0)then
                 tempm2(i,j)=tret(j)
               else
                  tempm2(i,j)=-99.00
               endif
            enddo
            if (iflag.eq.0)then
               tpwm2(i)=tpw
               clwm2(i)=clw
            else
               tpwm2(i)=-99.00
               clwm2(i)=-99.00
            endif
c
c     write Metop 2A ascii data with temperature retreivals, total
c     precipitable water, and cloud liquid water tacked on the end.
c
            write(22,1575) idatem2(i),itimem2(i),rlatm2(i),
     &           rlonm2(i),isatm2(i),isenm2(i),iscanm2(i),lsm2(i),
     &           satzenm2(i),solzenm2(i),ielevm2(i),isatzm2(i),
     &           (tbm2(i,n),n=1,15),(tempm2(i,n),n=1,40),tpwm2(i),
     &           clwm2(i)
         enddo
c
c     close Metop 2A output file for coordinate file inum
c
         if(numm2.ne.0)write(32,994)
 994     format('Normal completion of METOP 2A temperature Retrieval')
 1012    continue
c         call flush_(12)
          flush (12)
         close(12)
c         call flush_(22)
          flush (22)
         close(22)
c         call flush_(32)
          flush (32)
         close(32)
      endif
      goto 999
 888  open (28,file='ret.log')
      write(28,889)coorfile
      close(28)
 889  format('END OF ',a22,' REACHED PREMATURELY EXICUTION STOPPED')
 999  stop
      end
